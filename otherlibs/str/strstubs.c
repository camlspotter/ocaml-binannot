/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*           Xavier Leroy, projet Cristal, INRIA Rocquencourt          */
/*                                                                     */
/*  Copyright 1996 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License, with    */
/*  the special exception on linking described in file ../../LICENSE.  */
/*                                                                     */
/***********************************************************************/

/* $Id$ */

#include <assert.h>
#include <string.h>
#include <ctype.h>
#include <mlvalues.h>
#include <alloc.h>
#include <memory.h>
#include <fail.h>

/* The backtracking NFA interpreter */

struct backtrack_point {
  unsigned char * txt;
  value * pc;
  int mask;
};

#define BACKTRACK_STACK_BLOCK_SIZE 500

struct backtrack_stack {
  struct backtrack_stack * previous;
  struct backtrack_point point[BACKTRACK_STACK_BLOCK_SIZE];
};

#define Opcode(x) ((x) & 0xFF)
#define Arg(x) ((unsigned long)(x) >> 8)
#define SignedArg(x) ((long)(x) >> 8)

enum {
  CHAR,       /* match a single character */
  CHARNORM,   /* match a single character, after normalization */
  STRING,     /* match a character string */
  STRINGNORM, /* match a character string, after normalization */
  CHARCLASS,  /* match a character class */
  BOL,        /* match at beginning of line */
  EOL,        /* match at end of line */
  WORDBOUNDARY, /* match on a word boundary */
  BEGGROUP,   /* record the beginning of a group */
  ENDGROUP,   /* record the end of a group */
  REFGROUP,   /* match a previously matched group */
  ACCEPT,     /* report success */
  SIMPLEOPT,  /* match a character class 0 or 1 times */
  SIMPLESTAR, /* match a character class 0, 1 or several times */
  SIMPLEPLUS, /* match a character class 1 or several times */
  GOTO,       /* unconditional branch */
  PUSHBACK,   /* record a backtrack point -- 
                 where to jump in case of failure */
  GOTO_STAR,  /* like goto, except that we fall through if no
                 characters were consumed since last PUSHBACK */
  GOTO_PLUS,  /* like goto, except that we fall through if no
                 characters were consumed since penultimate PUSHBACK */
};

/* Accessors in a compiled regexp */
#define Prog(re) Field(re, 0)
#define Cpool(re) Field(re, 1)
#define Normtable(re) Field(re, 2)
#define Numgroups(re) Int_val(Field(re, 3))
#define Startchars(re) Int_val(Field(re, 4))

/* Record positions of matched groups */
struct re_group {
  unsigned char * tentative_start;
  unsigned char * start;
  unsigned char * end;
};
static struct re_group re_group[32];

/* Bitvector recording which groups were fully matched */
static int re_mask;

/* The initial backtracking stack */
static struct backtrack_stack initial_stack = { NULL, };

/* Free a chained list of backtracking stacks */
static void free_backtrack_stack(struct backtrack_stack * stack)
{
  struct backtrack_stack * prevstack;
  while ((prevstack = stack->previous) != NULL) {
    stat_free(stack);
    stack = prevstack;
  }
}

/* Membership in a bit vector representing a set of booleans */
#define In_bitset(s,i,tmp) (tmp = (i), ((s)[tmp >> 3] >> (tmp & 7)) & 1)

/* Determine if a character is a word constituent */
static unsigned char re_word_letters[32] = {
  0, 0, 0, 0, 0, 0, 0, 0, 254, 255, 255, 7, 254, 255, 255, 7,
  0, 0, 0, 0, 0, 0, 0, 0, 255, 255, 127, 255, 255, 255, 127, 255
};
#define Is_word_letter(c) ((re_word_letters[(c) >> 3] >> ((c) & 7)) & 1)

/* Return the n-th previous backtrack point.
   Must have  n < BACKTRACK_STACK_BLOCK_SIZE. */
static struct backtrack_point *
re_previous_backtrack_point(struct backtrack_point * sp,
                            struct backtrack_stack * stack,
                            int n)
{
  if (sp >= stack->point + n) return sp - n;
  stack = stack->previous;
  if (stack == NULL) return NULL;
  return stack->point + BACKTRACK_STACK_BLOCK_SIZE - n;
}

/* The bytecode interpreter for the NFA */
static int re_match(value re, 
                    unsigned char * starttxt,
                    register unsigned char * txt,
                    register unsigned char * endtxt,
                    int accept_partial_match)
{
  register value * pc;
  struct backtrack_stack * stack;
  struct backtrack_point * sp;
  value cpool;
  value normtable;
  unsigned char c;

  pc = &Field(Prog(re), 0);
  stack = &initial_stack;
  sp = stack->point;
  cpool = Cpool(re);
  normtable = Normtable(re);
  re_mask = 0;
  re_group[0].start = txt;

  while (1) {
    long instr = Long_val(*pc++);
    switch (Opcode(instr)) {
    case CHAR:
      if (txt == endtxt) goto prefix_match;
      if (*txt != Arg(instr)) goto backtrack;
      txt++;
      break;
    case CHARNORM:
      if (txt == endtxt) goto prefix_match;
      if (Byte_u(normtable, *txt) != Arg(instr)) goto backtrack;
      txt++;
      break;
    case STRING: {
      unsigned char * s =
        (unsigned char *) String_val(Field(cpool, Arg(instr)));
      while ((c = *s++) != 0) {
        if (txt == endtxt) goto prefix_match;
        if (c != *txt) goto backtrack;
        txt++;
      }
      break;
    }
    case STRINGNORM: {
      unsigned char * s =
        (unsigned char *) String_val(Field(cpool, Arg(instr)));
      while ((c = *s++) != 0) {
        if (txt == endtxt) goto prefix_match;
        if (c != Byte_u(normtable, *txt)) goto backtrack;
        txt++;
      }
      break;
    }
    case CHARCLASS:
      if (txt == endtxt) goto prefix_match;
      if (! In_bitset(String_val(Field(cpool, Arg(instr))), *txt, c))
        goto backtrack;
      txt++;
      break;
    case BOL:
      if (txt > starttxt && txt[-1] != '\n') goto backtrack;
      break;
    case EOL:
      if (txt < endtxt && *txt != '\n') goto backtrack;
      break;
    case WORDBOUNDARY:
      /* At beginning and end of text: no
         At beginning of text: OK if current char is a letter
         At end of text: OK if previous char is a letter
         Otherwise: 
           OK if previous char is a letter and current char not a letter
           or previous char is not a letter and current char is a letter */
      if (txt == starttxt) {
        if (txt == endtxt) goto prefix_match;
        if (Is_word_letter(txt[0])) break;
        goto backtrack;
      } else if (txt == endtxt) {
        if (Is_word_letter(txt[-1])) break;
        goto backtrack;
      } else {
        if (Is_word_letter(txt[-1]) != Is_word_letter(txt[0])) break;
        goto backtrack;
      }
    case BEGGROUP: {
      int group_no = Arg(instr);
      re_group[group_no].tentative_start = txt;
      break;
    }
    case ENDGROUP: {
      int group_no = Arg(instr);
      struct re_group * group = &(re_group[group_no]);
      group->start = group->tentative_start;
      group->end = txt;
      re_mask |= (1 << group_no);
      break;
    }
    case REFGROUP: {
      int group_no = Arg(instr);
      struct re_group * group = &(re_group[group_no]);
      unsigned char * s;
      if ((re_mask & (1 << group_no)) == 0) goto backtrack;
      for (s = group->start; s < group->end; s++) {
        if (txt == endtxt) goto prefix_match;
        if (*s != *txt) goto backtrack;
        txt++;
      }
      break;
    }
    case ACCEPT:
      goto accept;
    case SIMPLEOPT: {
      char * set = String_val(Field(cpool, Arg(instr)));
      if (txt < endtxt && In_bitset(set, *txt, c)) txt++;
      break;
    }
    case SIMPLESTAR: {
      char * set = String_val(Field(cpool, Arg(instr)));
      while (txt < endtxt && In_bitset(set, *txt, c))
        txt++;
      break;
    }
    case SIMPLEPLUS: {
      char * set = String_val(Field(cpool, Arg(instr)));
      if (txt == endtxt) goto prefix_match;
      if (! In_bitset(set, *txt, c)) goto backtrack;
      txt++;
      while (txt < endtxt && In_bitset(set, *txt, c))
        txt++;
      break;
    }
    case GOTO:
      pc = pc + SignedArg(instr);
      break;
    case PUSHBACK:
      if (sp == stack->point + BACKTRACK_STACK_BLOCK_SIZE) {
        struct backtrack_stack * newstack = 
          stat_alloc(sizeof(struct backtrack_stack));
        newstack->previous = stack;
        stack = newstack;
        sp = stack->point;
      }
      sp->txt = txt;
      sp->pc = pc + SignedArg(instr);
      sp->mask = re_mask;
      sp++;
      break;
    case GOTO_STAR: {
      struct backtrack_point * p = re_previous_backtrack_point(sp, stack, 1);
      if (p == NULL || txt > p->txt)
        pc = pc + SignedArg(instr);
      break;
    }
    case GOTO_PLUS: {
      struct backtrack_point * p = re_previous_backtrack_point(sp, stack, 2);
      if (p == NULL || txt > p->txt)
        pc = pc + SignedArg(instr);
      break;
    }
    default:
      assert(0);
    }
    /* Continue with next instruction */
    continue;
  prefix_match:
    /* We get here when matching failed because the end of text
       was encountered. */
    if (accept_partial_match) goto accept;
  backtrack:
    /* We get here when matching fails.  Backtrack to most recent saved
       point. */
    if (sp == stack->point) {
      struct backtrack_stack * prevstack = stack->previous;
      if (prevstack == NULL) return 0;
      stat_free(stack);
      stack = prevstack;
      sp = stack->point + BACKTRACK_STACK_BLOCK_SIZE;
    }
    sp--;
    txt = sp->txt;
    pc = sp->pc;
    re_mask = sp->mask;
  }  
 accept:
  /* We get here when the regexp was successfully matched */
  free_backtrack_stack(stack);
  re_group[0].end = txt;
  re_mask |= 1;
  return 1;
}

/* Allocate an integer array containing the positions of the matched groups.
   Beginning of group #N is at 2N, end is at 2N+1.
   Take position = -1 when group wasn't matched. */

static value re_alloc_groups(value re, value str)
{
  CAMLparam1(str);
  CAMLlocal1(res);
  unsigned char * starttxt = (unsigned char *) String_val(str);
  int n = Numgroups(re);
  int i;

  res = alloc(n * 2, 0);
  for (i = 0; i < n; i++) {
    if ((re_mask & (1 << i)) == 0) {
      Field(res, i * 2) = Val_int(-1);
      Field(res, i * 2 + 1) = Val_int(-1);
    } else {
      Field(res, i * 2) = Val_long(re_group[i].start - starttxt);
      Field(res, i * 2 + 1) = Val_long(re_group[i].end - starttxt);
    }
  }
  CAMLreturn(res);
}

/* String matching and searching.  All functions return the empty array
   on failure, and an array of positions on success. */

CAMLprim value re_string_match(value re, value str, value pos)
{
  unsigned char * starttxt = &Byte_u(str, 0);
  unsigned char * txt = &Byte_u(str, Long_val(pos));
  unsigned char * endtxt = &Byte_u(str, string_length(str));

  if (txt < starttxt || txt > endtxt)
    invalid_argument("Str.string_match");
  if (re_match(re, starttxt, txt, endtxt, 0)) {
    return re_alloc_groups(re, str);
  } else {
    return Atom(0);
  }
}

CAMLprim value re_partial_match(value re, value str, value pos)
{
  unsigned char * starttxt = &Byte_u(str, 0);
  unsigned char * txt = &Byte_u(str, Long_val(pos));
  unsigned char * endtxt = &Byte_u(str, string_length(str));

  if (txt < starttxt || txt > endtxt)
    invalid_argument("Str.string_partial_match");
  if (re_match(re, starttxt, txt, endtxt, 1)) {
    return re_alloc_groups(re, str);
  } else {
    return Atom(0);
  }
}

CAMLprim value re_search_forward(value re, value str, value startpos)
{
  unsigned char * starttxt = &Byte_u(str, 0);
  unsigned char * txt = &Byte_u(str, Long_val(startpos));
  unsigned char * endtxt = &Byte_u(str, string_length(str));
  unsigned char * startchars;
  unsigned char c;

  if (txt < starttxt || txt > endtxt)
    invalid_argument("Str.search_forward");
  if (Startchars(re) == -1) {
    do {
      if (re_match(re, starttxt, txt, endtxt, 0))
        return re_alloc_groups(re, str);
      txt++;
    } while (txt <= endtxt);
    return Atom(0);
  } else {
    startchars =
      (unsigned char *) String_val(Field(Cpool(re), Startchars(re)));
    do {
      while (txt < endtxt && startchars[*txt] == 0) txt++;
      if (re_match(re, starttxt, txt, endtxt, 0))
        return re_alloc_groups(re, str);
      txt++;
    } while (txt <= endtxt);
    return Atom(0);
  }
}

CAMLprim value re_search_backward(value re, value str, value startpos)
{
  unsigned char * starttxt = &Byte_u(str, 0);
  unsigned char * txt = &Byte_u(str, Long_val(startpos));
  unsigned char * endtxt = &Byte_u(str, string_length(str));
  unsigned char * startchars;
  unsigned char c;

  if (txt < starttxt || txt > endtxt)
    invalid_argument("Str.search_backward");
  if (Startchars(re) == -1) {
    do {
      if (re_match(re, starttxt, txt, endtxt, 0))
        return re_alloc_groups(re, str);
      txt--;
    } while (txt >= starttxt);
    return Atom(0);
  } else {
    startchars =
      (unsigned char *) String_val(Field(Cpool(re), Startchars(re)));
    do {
      while (txt > starttxt && startchars[*txt] == 0) txt--;
      if (re_match(re, starttxt, txt, endtxt, 0))
        return re_alloc_groups(re, str);
      txt--;
    } while (txt >= starttxt);
    return Atom(0);
  }
}

/* Replacement */

CAMLprim value re_replacement_text(value repl, value groups, value orig)
{
  CAMLparam3(repl, groups, orig);
  CAMLlocal1(res);
  mlsize_t start, end, len, n;
  char * p, * q;
  int c;

  len = 0;
  p = String_val(repl);
  n = string_length(repl);
  while (n > 0) {
    c = *p++; n--;
    if(c != '\\')
      len++;
    else {
      if (n == 0) failwith("Str.replace: illegal backslash sequence");
      c = *p++; n--;
      switch (c) {
      case '\\':
        len++; break;
      case '0': case '1': case '2': case '3': case '4':
      case '5': case '6': case '7': case '8': case '9':
        c -= '0';
        if (c*2 >= Wosize_val(groups))
          failwith("Str.replace: reference to unmatched group");
        start = Long_val(Field(groups, c*2));
        end = Long_val(Field(groups, c*2 + 1));
        if (start == (mlsize_t) -1)
          failwith("Str.replace: reference to unmatched group");
        len += end - start;
        break;
      default:
        len += 2; break;
      }
    }
  }
  res = alloc_string(len);
  p = String_val(repl);
  q = String_val(res);
  n = string_length(repl);
  while (n > 0) {
    c = *p++; n--;
    if(c != '\\')
      *q++ = c;
    else {
      c = *p++; n--;
      switch (c) {
      case '\\':
        *q++ = '\\'; break;
      case '0': case '1': case '2': case '3': case '4':
      case '5': case '6': case '7': case '8': case '9':
        c -= '0';
        start = Long_val(Field(groups, c*2));
        end = Long_val(Field(groups, c*2 + 1));
        len = end - start;
        memmove (q, &Byte(orig, start), len);
        q += len;
        break;
      default:
        *q++ = '\\'; *q++ = c; break;
      }
    }
  }
  CAMLreturn(res);
}

