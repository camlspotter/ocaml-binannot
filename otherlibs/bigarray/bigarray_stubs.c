/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*         Manuel Serrano and Xavier Leroy, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 2000 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License, with    */
/*  the special exception on linking described in file ../../LICENSE.  */
/*                                                                     */
/***********************************************************************/

/* $Id$ */

#include <stddef.h>
#include <stdarg.h>
#include <string.h>
#include "alloc.h"
#include "bigarray.h"
#include "custom.h"
#include "fail.h"
#include "intext.h"
#include "memory.h"
#include "mlvalues.h"

extern void bigarray_unmap_file(void * addr, unsigned long len);
                                          /* from mmap_xxx.c */

/* Compute the number of elements of a big array */

static unsigned long bigarray_num_elts(struct caml_bigarray * b)
{
  unsigned long num_elts;
  int i;
  num_elts = 1;
  for (i = 0; i < b->num_dims; i++) num_elts = num_elts * b->dim[i];
  return num_elts;
}

/* Size in bytes of a bigarray element, indexed by bigarray kind */

int bigarray_element_size[] =
{ 4 /*FLOAT32*/, 8 /*FLOAT64*/,
  1 /*SINT8*/, 1 /*UINT8*/,
  2 /*SINT16*/, 2 /*UINT16*/,
  4 /*INT32*/, 8 /*INT64*/,
  sizeof(value) /*CAML_INT*/, sizeof(value) /*NATIVE_INT*/,
  8 /*COMPLEX32*/, 16 /*COMPLEX64*/
};

/* Compute the number of bytes for the elements of a big array */

unsigned long bigarray_byte_size(struct caml_bigarray * b)
{
  return bigarray_num_elts(b)
         * bigarray_element_size[b->flags & BIGARRAY_KIND_MASK];
}

/* Operation table for bigarrays */

static void bigarray_finalize(value v);
static int bigarray_compare(value v1, value v2);
static long bigarray_hash(value v);
static void bigarray_serialize(value, unsigned long *, unsigned long *);
unsigned long bigarray_deserialize(void * dst);
static struct custom_operations bigarray_ops = {
  "_bigarray",
  bigarray_finalize,
  bigarray_compare,
  bigarray_hash,
  bigarray_serialize,
  bigarray_deserialize
};

/* Allocation of a big array */

#define MAX_BIGARRAY_MEMORY 256*1024*1024
/* 256 Mb -- after allocating that much, it's probably worth speeding
   up the major GC */

value alloc_bigarray(int flags, int num_dims, void * data, long * dim)
{
  long num_elts, size;
  int i;
  value res;
  struct caml_bigarray * b;

  Assert(num_dims >= 1 && num_dims <= MAX_NUM_DIMS);
  Assert((flags & BIGARRAY_KIND_MASK) <= BIGARRAY_COMPLEX64);
  size = 0;
  if (data == NULL) {
    num_elts = 1;
    for (i = 0; i < num_dims; i++) num_elts = num_elts * dim[i];
    size = num_elts * bigarray_element_size[flags & BIGARRAY_KIND_MASK];
    data = malloc(size);
    if (data == NULL && size != 0) raise_out_of_memory();
    flags |= BIGARRAY_MANAGED;
  }
  res = alloc_custom(&bigarray_ops,
                     sizeof(struct caml_bigarray) 
                     + (num_dims - 1) * sizeof(long),
                     size, MAX_BIGARRAY_MEMORY);
  b = Bigarray_val(res);
  b->data = data;
  b->num_dims = num_dims;
  b->flags = flags;
  b->proxy = NULL;
  for (i = 0; i < num_dims; i++) b->dim[i] = dim[i];
  return res;
}

/* Same as alloc_bigarray, but dimensions are passed as a list of
   arguments */

value alloc_bigarray_dims(int flags, int num_dims, void * data, ...)
{
  va_list ap;
  long dim[MAX_NUM_DIMS];
  int i;
  value res;

  va_start(ap, data);
  for (i = 0; i < num_dims; i++) dim[i] = va_arg(ap, long);
  va_end(ap);
  res = alloc_bigarray(flags, num_dims, data, dim);
  return res;
}

/* Allocate a bigarray from Caml */

CAMLprim value bigarray_create(value vkind, value vlayout, value vdim)
{
  long dim[MAX_NUM_DIMS];
  mlsize_t num_dims;
  int i, flags;

  num_dims = Wosize_val(vdim);
  if (num_dims < 1 || num_dims > MAX_NUM_DIMS)
    invalid_argument("Bigarray.create: bad number of dimensions");
  for (i = 0; i < num_dims; i++) {
    dim[i] = Long_val(Field(vdim, i));
    if (dim[i] < 0 || dim[i] > 0x7FFFFFFFL)
      invalid_argument("Bigarray.create: negative dimension");
  }
  flags = Int_val(vkind) | Int_val(vlayout);
  return alloc_bigarray(flags, num_dims, NULL, dim);
}

/* Given a big array and a vector of indices, check that the indices
   are within the bounds and return the offset of the corresponding
   array element in the data part of the array. */

static long bigarray_offset(struct caml_bigarray * b, long * index)
{
  long offset;
  int i;

  offset = 0;
  if ((b->flags & BIGARRAY_LAYOUT_MASK) == BIGARRAY_C_LAYOUT) {
    /* C-style layout: row major, indices start at 0 */
    for (i = 0; i < b->num_dims; i++) {
      if ((unsigned long) index[i] >= (unsigned long) b->dim[i])
        invalid_argument("Bigarray: out-of-bound access");
      offset = offset * b->dim[i] + index[i];
    }
  } else {
    /* Fortran-style layout: column major, indices start at 1 */
    for (i = b->num_dims - 1; i >= 0; i--) {
      if ((unsigned long) (index[i] - 1) >= (unsigned long) b->dim[i])
        invalid_argument("Bigarray: out-of-bound access");
      offset = offset * b->dim[i] + (index[i] - 1);
    }
  }
  return offset;
}

/* Helper function to allocate a record of two double floats */

static value copy_two_doubles(double d0, double d1)
{
  value res = alloc_small(2 * Double_wosize, Double_array_tag);
  Store_double_field(res, 0, d0);
  Store_double_field(res, 1, d1);
  return res;
}

/* Generic code to read from a big array */

value bigarray_get_N(value vb, value * vind, int nind)
{
  struct caml_bigarray * b = Bigarray_val(vb);
  long index[MAX_NUM_DIMS];
  int i;
  long offset;

  /* Check number of indices = number of dimensions of array
     (maybe not necessary if ML typing guarantees this) */
  if (nind != b->num_dims)
    invalid_argument("Bigarray.get: wrong number of indices");
  /* Compute offset and check bounds */
  for (i = 0; i < b->num_dims; i++) index[i] = Long_val(vind[i]);
  offset = bigarray_offset(b, index);
  /* Perform read */
  switch ((b->flags) & BIGARRAY_KIND_MASK) {
  default:
    Assert(0);
  case BIGARRAY_FLOAT32:
    return copy_double(((float *) b->data)[offset]);
  case BIGARRAY_FLOAT64:
    return copy_double(((double *) b->data)[offset]);
  case BIGARRAY_SINT8:
    return Val_int(((schar *) b->data)[offset]);
  case BIGARRAY_UINT8:
    return Val_int(((unsigned char *) b->data)[offset]);
  case BIGARRAY_SINT16:
    return Val_int(((int16 *) b->data)[offset]);
  case BIGARRAY_UINT16:
    return Val_int(((uint16 *) b->data)[offset]);
  case BIGARRAY_INT32:
    return copy_int32(((int32 *) b->data)[offset]);
  case BIGARRAY_INT64:
    return copy_int64(((int64 *) b->data)[offset]);
  case BIGARRAY_NATIVE_INT:
    return copy_nativeint(((long *) b->data)[offset]);
  case BIGARRAY_CAML_INT:
    return Val_long(((long *) b->data)[offset]);
  case BIGARRAY_COMPLEX32:
    { float * p = ((float *) b->data) + offset * 2;
      return copy_two_doubles(p[0], p[1]); }
  case BIGARRAY_COMPLEX64:
    { double * p = ((double *) b->data) + offset * 2;
      return copy_two_doubles(p[0], p[1]); }
  }
}

CAMLprim value bigarray_get_1(value vb, value vind1)
{
  return bigarray_get_N(vb, &vind1, 1);
}

CAMLprim value bigarray_get_2(value vb, value vind1, value vind2)
{
  value vind[2];
  vind[0] = vind1; vind[1] = vind2;
  return bigarray_get_N(vb, vind, 2);
}

CAMLprim value bigarray_get_3(value vb, value vind1, value vind2, value vind3)
{
  value vind[3];
  vind[0] = vind1; vind[1] = vind2; vind[2] = vind3;
  return bigarray_get_N(vb, vind, 3);
}

#if 0
CAMLprim value bigarray_get_4(value vb, value vind1, value vind2, 
                     value vind3, value vind4)
{
  value vind[4];
  vind[0] = vind1; vind[1] = vind2; vind[2] = vind3; vind[3] = vind4;
  return bigarray_get_N(vb, vind, 4);
}

CAMLprim value bigarray_get_5(value vb, value vind1, value vind2, 
                     value vind3, value vind4, value vind5)
{
  value vind[5];
  vind[0] = vind1; vind[1] = vind2; vind[2] = vind3; 
  vind[3] = vind4; vind[4] = vind5;
  return bigarray_get_N(vb, vind, 5);
}

CAMLprim value bigarray_get_6(value vb, value vind1, value vind2, 
                     value vind3, value vind4, value vind5, value vind6)
{
  value vind[6];
  vind[0] = vind1; vind[1] = vind2; vind[2] = vind3; 
  vind[3] = vind4; vind[4] = vind5; vind[5] = vind6;
  return bigarray_get_N(vb, vind, 6);
}
#endif

CAMLprim value bigarray_get_generic(value vb, value vind)
{
  return bigarray_get_N(vb, &Field(vind, 0), Wosize_val(vind));
}

/* Generic write to a big array */

static value bigarray_set_aux(value vb, value * vind, long nind, value newval)
{
  struct caml_bigarray * b = Bigarray_val(vb);
  long index[MAX_NUM_DIMS];
  int i;
  long offset;
  
  /* Check number of indices = number of dimensions of array
     (maybe not necessary if ML typing guarantees this) */
  if (nind != b->num_dims)
    invalid_argument("Bigarray.set: wrong number of indices");
  /* Compute offset and check bounds */
  for (i = 0; i < b->num_dims; i++) index[i] = Long_val(vind[i]);
  offset = bigarray_offset(b, index);
  /* Perform write */
  switch (b->flags & BIGARRAY_KIND_MASK) {
  default:
    Assert(0);
  case BIGARRAY_FLOAT32:
    ((float *) b->data)[offset] = Double_val(newval); break;
  case BIGARRAY_FLOAT64:
    ((double *) b->data)[offset] = Double_val(newval); break;
  case BIGARRAY_SINT8:
  case BIGARRAY_UINT8:
    ((schar *) b->data)[offset] = Int_val(newval); break;
  case BIGARRAY_SINT16:
  case BIGARRAY_UINT16:
    ((int16 *) b->data)[offset] = Int_val(newval); break;
  case BIGARRAY_INT32:
    ((int32 *) b->data)[offset] = Int32_val(newval); break;
  case BIGARRAY_INT64:
    ((int64 *) b->data)[offset] = Int64_val(newval); break;
  case BIGARRAY_NATIVE_INT:
    ((long *) b->data)[offset] = Nativeint_val(newval); break;
  case BIGARRAY_CAML_INT:
    ((long *) b->data)[offset] = Long_val(newval); break;
  case BIGARRAY_COMPLEX32:
    { float * p = ((float *) b->data) + offset * 2;
      p[0] = Double_field(newval, 0);
      p[1] = Double_field(newval, 1);
      break; }
  case BIGARRAY_COMPLEX64:
    { double * p = ((double *) b->data) + offset * 2;
      p[0] = Double_field(newval, 0);
      p[1] = Double_field(newval, 1);
      break; }
  }
  return Val_unit;
}

CAMLprim value bigarray_set_1(value vb, value vind1, value newval)
{
  return bigarray_set_aux(vb, &vind1, 1, newval);
}

CAMLprim value bigarray_set_2(value vb, value vind1, value vind2, value newval)
{
  value vind[2];
  vind[0] = vind1; vind[1] = vind2;
  return bigarray_set_aux(vb, vind, 2, newval);
}

CAMLprim value bigarray_set_3(value vb, value vind1, value vind2, value vind3,
                     value newval)
{
  value vind[3];
  vind[0] = vind1; vind[1] = vind2; vind[2] = vind3;
  return bigarray_set_aux(vb, vind, 3, newval);
}

#if 0
CAMLprim value bigarray_set_4(value vb, value vind1, value vind2, 
                     value vind3, value vind4, value newval)
{
  value vind[4];
  vind[0] = vind1; vind[1] = vind2; vind[2] = vind3; vind[3] = vind4;
  return bigarray_set_aux(vb, vind, 4, newval);
}

CAMLprim value bigarray_set_5(value vb, value vind1, value vind2, 
                     value vind3, value vind4, value vind5, value newval)
{
  value vind[5];
  vind[0] = vind1; vind[1] = vind2; vind[2] = vind3; 
  vind[3] = vind4; vind[4] = vind5;
  return bigarray_set_aux(vb, vind, 5, newval);
}

CAMLprim value bigarray_set_6(value vb, value vind1, value vind2, 
                     value vind3, value vind4, value vind5,
                     value vind6, value newval)
{
  value vind[6];
  vind[0] = vind1; vind[1] = vind2; vind[2] = vind3; 
  vind[3] = vind4; vind[4] = vind5; vind[5] = vind6;
  return bigarray_set_aux(vb, vind, 6, newval);
}

value bigarray_set_N(value vb, value * vind, int nargs)
{
  return bigarray_set_aux(vb, vind, nargs - 1, vind[nargs - 1]);
}
#endif

CAMLprim value bigarray_set_generic(value vb, value vind, value newval)
{
  return bigarray_set_aux(vb, &Field(vind, 0), Wosize_val(vind), newval);
}

/* Return the number of dimensions of a big array */

CAMLprim value bigarray_num_dims(value vb)
{
  struct caml_bigarray * b = Bigarray_val(vb);
  return Val_long(b->num_dims);
}

/* Return the n-th dimension of a big array */

CAMLprim value bigarray_dim(value vb, value vn)
{
  struct caml_bigarray * b = Bigarray_val(vb);
  long n = Long_val(vn);
  if (n >= b->num_dims) invalid_argument("Bigarray.dim");
  return Val_long(b->dim[n]);
}

/* Finalization of a big array */

static void bigarray_finalize(value v)
{
  struct caml_bigarray * b = Bigarray_val(v);

  switch (b->flags & BIGARRAY_MANAGED_MASK) {
  case BIGARRAY_EXTERNAL:
    break;
  case BIGARRAY_MANAGED:
    if (b->proxy == NULL) {
      free(b->data);
    } else {
      if (-- b->proxy->refcount == 0) {
        free(b->proxy->data);
        stat_free(b->proxy);
      }
    }
    break;
  case BIGARRAY_MAPPED_FILE:
    if (b->proxy == NULL) {
      bigarray_unmap_file(b->data, bigarray_byte_size(b));
    } else {
      if (-- b->proxy->refcount == 0) {
        bigarray_unmap_file(b->proxy->data, b->proxy->size);
        stat_free(b->proxy);
      }
    }
    break;
  }
}

/* Comparison of two big arrays */

static int bigarray_compare(value v1, value v2)
{
  struct caml_bigarray * b1 = Bigarray_val(v1);
  struct caml_bigarray * b2 = Bigarray_val(v2);
  unsigned long n, num_elts;
  int i;

  /* Compare number of dimensions */
  if (b1->num_dims != b2->num_dims) return b2->num_dims - b1->num_dims;
  /* Same number of dimensions: compare dimensions lexicographically */
  for (i = 0; i < b1->num_dims; i++) {
    long d1 = b1->dim[i];
    long d2 = b2->dim[i];
    if (d1 != d2) return d1 < d2 ? -1 : 1;
  }
  /* Same dimensions: compare contents lexicographically */
  num_elts = bigarray_num_elts(b1);

#define DO_COMPARISON(type) \
  { type * p1 = b1->data; type * p2 = b2->data; \
    for (n = 0; n < num_elts; n++) { \
      type e1 = *p1++; type e2 = *p2++; \
      if (e1 < e2) return -1; \
      if (e1 > e2) return 1; \
    } \
    return 0; \
  }

  switch (b1->flags & BIGARRAY_KIND_MASK) {
  case BIGARRAY_COMPLEX32:
    num_elts *= 2; /*fallthrough*/
  case BIGARRAY_FLOAT32:
    DO_COMPARISON(float);
  case BIGARRAY_COMPLEX64:
    num_elts *= 2; /*fallthrough*/
  case BIGARRAY_FLOAT64:
    DO_COMPARISON(double);
  case BIGARRAY_SINT8:
    DO_COMPARISON(schar);
  case BIGARRAY_UINT8:
    DO_COMPARISON(unsigned char);
  case BIGARRAY_SINT16:
    DO_COMPARISON(int16);
  case BIGARRAY_UINT16:
    DO_COMPARISON(uint16);
  case BIGARRAY_INT32:
    DO_COMPARISON(int32);
  case BIGARRAY_INT64:
#ifdef ARCH_INT64_TYPE
    DO_COMPARISON(int64);
#else
    invalid_argument("Bigarray.compare: 64-bit int arrays not supported");
#endif
  case BIGARRAY_CAML_INT:
  case BIGARRAY_NATIVE_INT:
    DO_COMPARISON(long);
  default:
    Assert(0);
    return 0;                   /* should not happen */
  }
#undef DO_COMPARISON
}

/* Hashing of a bigarray */

static long bigarray_hash(value v)
{
  struct caml_bigarray * b = Bigarray_val(v);
  long num_elts, n, h;
  int i;

  num_elts = 1;
  for (i = 0; i < b->num_dims; i++) num_elts = num_elts * b->dim[i];
  if (num_elts >= 50) num_elts = 50;
  h = 0;

#define COMBINE(h,v) ((h << 4) + h + (v))

  switch (b->flags & BIGARRAY_KIND_MASK) {
  case BIGARRAY_SINT8:
  case BIGARRAY_UINT8: {
    unsigned char * p = b->data;
    for (n = 0; n < num_elts; n++) h = COMBINE(h, *p++);
    break;
  }
  case BIGARRAY_SINT16:
  case BIGARRAY_UINT16: {
    unsigned short * p = b->data;
    for (n = 0; n < num_elts; n++) h = COMBINE(h, *p++);
    break;
  }
  case BIGARRAY_FLOAT32:
  case BIGARRAY_COMPLEX32:
  case BIGARRAY_INT32:
#ifndef ARCH_SIXTYFOUR
  case BIGARRAY_CAML_INT:
  case BIGARRAY_NATIVE_INT:
#endif
  {
    uint32 * p = b->data;
    for (n = 0; n < num_elts; n++) h = COMBINE(h, *p++);
    break;
  }
  case BIGARRAY_FLOAT64:
  case BIGARRAY_COMPLEX64:
  case BIGARRAY_INT64:
#ifdef ARCH_SIXTYFOUR
  case BIGARRAY_CAML_INT:
  case BIGARRAY_NATIVE_INT:
#endif
#ifdef ARCH_SIXTYFOUR
  {
    unsigned long * p = b->data;
    for (n = 0; n < num_elts; n++) h = COMBINE(h, *p++);
    break;
  }
#else
  {
    uint32 * p = b->data;
    for (n = 0; n < num_elts; n++) {
#ifdef ARCH_BIG_ENDIAN
      h = COMBINE(h, p[1]); h = COMBINE(h, p[0]); p += 2;
#else
      h = COMBINE(h, p[0]); h = COMBINE(h, p[1]); p += 2;
#endif
    }
    break;
  }
#endif
  }
#undef COMBINE
  return h;
}

static void bigarray_serialize_longarray(void * data,
                                         long num_elts,
                                         long min_val, long max_val)
{
#ifdef ARCH_SIXTYFOUR
  int overflow_32 = 0;
  long * p, n;
  for (n = 0, p = data; n < num_elts; n++, p++) {
    if (*p < min_val || *p > max_val) { overflow_32 = 1; break; }
  }
  if (overflow_32) {
    serialize_int_1(1);
    serialize_block_8(data, num_elts);
  } else {
    serialize_int_1(0);
    for (n = 0, p = data; n < num_elts; n++, p++) serialize_int_4((int32) *p);
  }
#else
  serialize_int_1(0);
  serialize_block_4(data, num_elts);
#endif
}

static void bigarray_serialize(value v, 
                               unsigned long * wsize_32,
                               unsigned long * wsize_64)
{
  struct caml_bigarray * b = Bigarray_val(v);
  long num_elts;
  int i;

  /* Serialize header information */
  serialize_int_4(b->num_dims);
  serialize_int_4(b->flags & (BIGARRAY_KIND_MASK | BIGARRAY_LAYOUT_MASK));
  for (i = 0; i < b->num_dims; i++) serialize_int_4(b->dim[i]);
  /* Compute total number of elements */
  num_elts = 1;
  for (i = 0; i < b->num_dims; i++) num_elts = num_elts * b->dim[i];
  /* Serialize elements */
  switch (b->flags & BIGARRAY_KIND_MASK) {
  case BIGARRAY_SINT8:
  case BIGARRAY_UINT8:
    serialize_block_1(b->data, num_elts); break;
  case BIGARRAY_SINT16:
  case BIGARRAY_UINT16:
    serialize_block_2(b->data, num_elts); break;
  case BIGARRAY_FLOAT32:
  case BIGARRAY_INT32:
    serialize_block_4(b->data, num_elts); break;
  case BIGARRAY_COMPLEX32:
    serialize_block_4(b->data, num_elts * 2); break;
  case BIGARRAY_FLOAT64:
  case BIGARRAY_INT64:
    serialize_block_8(b->data, num_elts); break;
  case BIGARRAY_COMPLEX64:
    serialize_block_8(b->data, num_elts * 2); break;
  case BIGARRAY_CAML_INT:
    bigarray_serialize_longarray(b->data, num_elts, -0x40000000, 0x3FFFFFFF);
    break;
  case BIGARRAY_NATIVE_INT:
    bigarray_serialize_longarray(b->data, num_elts, -0x80000000, 0x7FFFFFFF);
    break;
  }
  /* Compute required size in Caml heap.  Assumes struct caml_bigarray
     is exactly 4 + num_dims words */
  Assert(sizeof(struct caml_bigarray) == 5 * sizeof(value));
  *wsize_32 = (4 + b->num_dims) * 4;
  *wsize_64 = (4 + b->num_dims) * 8;
}

static void bigarray_deserialize_longarray(void * dest, long num_elts)
{
  int sixty = deserialize_uint_1();
#ifdef ARCH_SIXTYFOUR
  if (sixty) {
    deserialize_block_8(dest, num_elts);
  } else {
    long * p, n;
    for (n = 0, p = dest; n < num_elts; n++, p++) *p = deserialize_sint_4();
  }
#else
  if (sixty)
    deserialize_error("input_value: cannot read bigarray "
                      "with 64-bit Caml ints");
  deserialize_block_4(dest, num_elts);
#endif
}

unsigned long bigarray_deserialize(void * dst)
{
  struct caml_bigarray * b = dst;
  int i, elt_size;
  unsigned long num_elts;

  /* Read back header information */
  b->num_dims = deserialize_uint_4();
  b->flags = deserialize_uint_4() | BIGARRAY_MANAGED;
  b->proxy = NULL;
  for (i = 0; i < b->num_dims; i++) b->dim[i] = deserialize_uint_4();
  /* Compute total number of elements */
  num_elts = bigarray_num_elts(b);
  /* Determine element size in bytes */
  if ((b->flags & BIGARRAY_KIND_MASK) > BIGARRAY_COMPLEX64)
    deserialize_error("input_value: bad bigarray kind");
  elt_size = bigarray_element_size[b->flags & BIGARRAY_KIND_MASK];
  /* Allocate room for data */
  b->data = malloc(elt_size * num_elts);
  if (b->data == NULL)
    deserialize_error("input_value: out of memory for bigarray");
  /* Read data */
  switch (b->flags & BIGARRAY_KIND_MASK) {
  case BIGARRAY_SINT8:
  case BIGARRAY_UINT8:
    deserialize_block_1(b->data, num_elts); break;
  case BIGARRAY_SINT16:
  case BIGARRAY_UINT16:
    deserialize_block_2(b->data, num_elts); break;
  case BIGARRAY_FLOAT32:
  case BIGARRAY_INT32:
    deserialize_block_4(b->data, num_elts); break;
  case BIGARRAY_COMPLEX32:
    deserialize_block_4(b->data, num_elts * 2); break;
  case BIGARRAY_FLOAT64:
  case BIGARRAY_INT64:
    deserialize_block_8(b->data, num_elts); break;
  case BIGARRAY_COMPLEX64:
    deserialize_block_8(b->data, num_elts * 2); break;
  case BIGARRAY_CAML_INT:
  case BIGARRAY_NATIVE_INT:
    bigarray_deserialize_longarray(b->data, num_elts); break;
  }
  return sizeof(struct caml_bigarray) + (b->num_dims - 1) * sizeof(long);
}

/* Create / update proxy to indicate that b2 is a sub-array of b1 */

static void bigarray_update_proxy(struct caml_bigarray * b1,
                                  struct caml_bigarray * b2)
{
  struct caml_bigarray_proxy * proxy;
  /* Nothing to do for un-managed arrays */
  if ((b1->flags & BIGARRAY_MANAGED_MASK) == BIGARRAY_EXTERNAL) return;
  if (b1->proxy != NULL) {
    /* If b1 is already a proxy for a larger array, increment refcount of
       proxy */
    b2->proxy = b1->proxy;
    ++ b1->proxy->refcount;
  } else {
    /* Otherwise, create proxy and attach it to both b1 and b2 */
    proxy = stat_alloc(sizeof(struct caml_bigarray_proxy));
    proxy->refcount = 2;      /* original array + sub array */
    proxy->data = b1->data;
    proxy->size =
      b1->flags & BIGARRAY_MAPPED_FILE ? bigarray_byte_size(b1) : 0;
    b1->proxy = proxy;
    b2->proxy = proxy;
  }
}

/* Slicing */

CAMLprim value bigarray_slice(value vb, value vind)
{
  struct caml_bigarray * b = Bigarray_val(vb);
  long index[MAX_NUM_DIMS];
  int num_inds, i;
  long offset;
  long * sub_dims;
  char * sub_data;
  value res;

  /* Check number of indices < number of dimensions of array */
  num_inds = Wosize_val(vind);
  if (num_inds >= b->num_dims)
    invalid_argument("Bigarray.slice: too many indices");
  /* Compute offset and check bounds */
  if ((b->flags & BIGARRAY_LAYOUT_MASK) == BIGARRAY_C_LAYOUT) {
    /* We slice from the left */
    for (i = 0; i < num_inds; i++) index[i] = Long_val(Field(vind, i));
    for (/*nothing*/; i < b->num_dims; i++) index[i] = 0;
    offset = bigarray_offset(b, index);
    sub_dims = b->dim + num_inds;
  } else {
    /* We slice from the right */
    for (i = 0; i < num_inds; i++)
      index[b->num_dims - num_inds + i] = Long_val(Field(vind, i));
    for (i = 0; i < b->num_dims - num_inds; i++) index[i] = 1;
    offset = bigarray_offset(b, index);
    sub_dims = b->dim;
  }
  sub_data =
    (char *) b->data +
    offset * bigarray_element_size[b->flags & BIGARRAY_KIND_MASK];
  /* Allocate a Caml bigarray to hold the result */
  res = alloc_bigarray(b->flags, b->num_dims - num_inds, sub_data, sub_dims);
  /* Create or update proxy in case of managed bigarray */
  bigarray_update_proxy(b, Bigarray_val(res));
  /* Return result */
  return res;
}

/* Extracting a sub-array of same number of dimensions */

CAMLprim value bigarray_sub(value vb, value vofs, value vlen)
{
  struct caml_bigarray * b = Bigarray_val(vb);
  long ofs = Long_val(vofs);
  long len = Long_val(vlen);
  int i, changed_dim;
  long mul;
  char * sub_data;
  value res;

  /* Compute offset and check bounds */
  if ((b->flags & BIGARRAY_LAYOUT_MASK) == BIGARRAY_C_LAYOUT) {
    /* We reduce the first dimension */
    mul = 1;
    for (i = 1; i < b->num_dims; i++) mul *= b->dim[i];
    changed_dim = 0;
  } else {
    /* We reduce the last dimension */
    mul = 1;
    for (i = 0; i < b->num_dims - 1; i++) mul *= b->dim[i];
    changed_dim = b->num_dims - 1;
    ofs--;                      /* Fortran arrays start at 1 */
  }
  if (ofs < 0 || len < 0 || ofs + len > b->dim[changed_dim])
    invalid_argument("Bigarray.sub: bad sub-array");
  sub_data =
    (char *) b->data +
    ofs * mul * bigarray_element_size[b->flags & BIGARRAY_KIND_MASK];
  /* Allocate a Caml bigarray to hold the result */
  res = alloc_bigarray(b->flags, b->num_dims, sub_data, b->dim);
  /* Doctor the changed dimension */
  Bigarray_val(res)->dim[changed_dim] = len;
  /* Create or update proxy in case of managed bigarray */
  bigarray_update_proxy(b, Bigarray_val(res));
  /* Return result */
  return res;
}

/* Copying a big array into another one */

CAMLprim value bigarray_blit(value vsrc, value vdst)
{
  struct caml_bigarray * src = Bigarray_val(vsrc);
  struct caml_bigarray * dst = Bigarray_val(vdst);
  int i;
  long num_bytes;

  /* Check same numbers of dimensions and same dimensions */
  if (src->num_dims != dst->num_dims) goto blit_error;
  for (i = 0; i < src->num_dims; i++)
    if (src->dim[i] != dst->dim[i]) goto blit_error;
  /* Compute number of bytes in array data */
  num_bytes =
    bigarray_num_elts(src)
    * bigarray_element_size[src->flags & BIGARRAY_KIND_MASK];
  /* Do the copying */
  memmove (dst->data, src->data, num_bytes);
  return Val_unit;
 blit_error:
  invalid_argument("Bigarray.blit: dimension mismatch");
  return Val_unit;              /* not reached */
}

/* Filling a big array with a given value */

CAMLprim value bigarray_fill(value vb, value vinit)
{
  struct caml_bigarray * b = Bigarray_val(vb);
  long num_elts = bigarray_num_elts(b);

  switch (b->flags & BIGARRAY_KIND_MASK) {
  default:
    Assert(0);
  case BIGARRAY_FLOAT32: {
    float init = Double_val(vinit);
    float * p;
    for (p = b->data; num_elts > 0; p++, num_elts--) *p = init;
    break;
  }
  case BIGARRAY_FLOAT64: {
    double init = Double_val(vinit);
    double * p;
    for (p = b->data; num_elts > 0; p++, num_elts--) *p = init;
    break;
  }
  case BIGARRAY_SINT8:
  case BIGARRAY_UINT8: {
    int init = Int_val(vinit);
    char * p;
    for (p = b->data; num_elts > 0; p++, num_elts--) *p = init;
    break;
  }
  case BIGARRAY_SINT16: 
  case BIGARRAY_UINT16: {
    int init = Int_val(vinit);
    short * p;
    for (p = b->data; num_elts > 0; p++, num_elts--) *p = init;
    break;
  }
  case BIGARRAY_INT32: {
    int32 init = Int32_val(vinit);
    int32 * p;
    for (p = b->data; num_elts > 0; p++, num_elts--) *p = init;
    break;
  }
  case BIGARRAY_INT64: {
    int64 init = Int64_val(vinit);
    int64 * p;
    for (p = b->data; num_elts > 0; p++, num_elts--) *p = init;
    break;
  }
  case BIGARRAY_NATIVE_INT: {
    long init = Nativeint_val(vinit);
    long * p;
    for (p = b->data; num_elts > 0; p++, num_elts--) *p = init;
    break;
  }
  case BIGARRAY_CAML_INT: {
    long init = Long_val(vinit);
    long * p;
    for (p = b->data; num_elts > 0; p++, num_elts--) *p = init;
    break;
  case BIGARRAY_COMPLEX32: {
    float init0 = Double_field(vinit, 0);
    float init1 = Double_field(vinit, 1);
    float * p;
    for (p = b->data; num_elts > 0; num_elts--) { *p++ = init0; *p++ = init1; }
    break;
  }
  case BIGARRAY_COMPLEX64: {
    double init0 = Double_field(vinit, 0);
    double init1 = Double_field(vinit, 1);
    double * p;
    for (p = b->data; num_elts > 0; num_elts--) { *p++ = init0; *p++ = init1; }
    break;
  }
  }
  }
  return Val_unit;
}

/* Reshape an array: change dimensions and number of dimensions, preserving
   array contents */

CAMLprim value bigarray_reshape(value vb, value vdim)
{
  struct caml_bigarray * b = Bigarray_val(vb);
  long dim[MAX_NUM_DIMS];
  mlsize_t num_dims;
  unsigned long num_elts;
  int i;
  value res;

  num_dims = Wosize_val(vdim);
  if (num_dims < 1 || num_dims > MAX_NUM_DIMS)
    invalid_argument("Bigarray.reshape: bad number of dimensions");
  num_elts = 1;
  for (i = 0; i < num_dims; i++) {
    dim[i] = Long_val(Field(vdim, i));
    if (dim[i] < 0 || dim[i] > 0x7FFFFFFFL)
      invalid_argument("Bigarray.reshape: negative dimension");
    num_elts *= dim[i];
  }
  /* Check that sizes agree */
  if (num_elts != bigarray_num_elts(b))
    invalid_argument("Bigarray.reshape: size mismatch");
  /* Create bigarray with same data and new dimensions */
  res = alloc_bigarray(b->flags, num_dims, b->data, dim);
  /* Create or update proxy in case of managed bigarray */
  bigarray_update_proxy(b, Bigarray_val(res));
  /* Return result */
  return res;
}

/* Initialization */

CAMLprim value bigarray_init(value unit)
{
  register_custom_operations(&bigarray_ops);
  return Val_unit;
}
