/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1996 Institut National de Recherche en Informatique et   */
/*  Automatique.  Distributed only by permission.                      */
/*                                                                     */
/***********************************************************************/

/* $Id$ */

/* Buffered input/output. */

#include <errno.h>
#include <fcntl.h>
#include <string.h>
#include "config.h"
#ifdef HAS_UNISTD
#include <unistd.h>
#endif
#ifdef __STDC__
#include <limits.h>
#endif
#include "alloc.h"
#include "fail.h"
#include "io.h"
#include "memory.h"
#include "misc.h"
#include "mlvalues.h"
#include "signals.h"
#include "sys.h"
#ifdef HAS_UI
#include "ui.h"
#endif

#ifndef INT_MAX
#define INT_MAX 0x7FFFFFFF
#endif

#ifndef SEEK_SET
#define SEEK_SET 0
#define SEEK_CUR 1
#define SEEK_END 2
#endif

/* Hooks for locking channels */

void (*channel_mutex_free) P((struct channel *)) = NULL;
void (*channel_mutex_lock) P((struct channel *)) = NULL;
void (*channel_mutex_unlock) P((struct channel *)) = NULL;
void (*channel_mutex_unlock_exn) P((void)) = NULL;

/* Basic functions over type struct channel *.
   These functions can be called directly from C.
   No locking is performed. */

struct channel * open_descriptor(fd)
     int fd;
{
  struct channel * channel;

  channel = (struct channel *) stat_alloc(sizeof(struct channel));
  channel->fd = fd;
  channel->offset = 0;
  channel->curr = channel->max = channel->buff;
  channel->end = channel->buff + IO_BUFFER_SIZE;
  channel->mutex = NULL;
  return channel;
}

void close_channel(channel)
     struct channel * channel;
{
  close(channel->fd);
  if (channel_mutex_free != NULL) (*channel_mutex_free)(channel);
  stat_free((char *) channel);
}  

long channel_size(channel)
     struct channel * channel;
{
  long end;

  end = lseek(channel->fd, 0, SEEK_END);
  if (end == -1 ||
      lseek(channel->fd, channel->offset, SEEK_SET) != channel->offset) {
    sys_error(NO_ARG);
  }
  return end;
}

/* Output */

#ifndef EINTR
#define EINTR (-1)
#endif
#ifndef EAGAIN
#define EAGAIN (-1)
#endif
#ifndef EWOULDBLOCK
#define EWOULDBLOCK (-1)
#endif

static int do_write(fd, p, n)
     int fd;
     char * p;
     int n;
{
  int retcode;

  Assert(!Is_young(p));
#ifdef HAS_UI
  retcode = ui_write(fd, p, n);
#else
again:
  enter_blocking_section();
  retcode = write(fd, p, n);
  leave_blocking_section();
  if (retcode == -1) {
    if (errno == EINTR) goto again;
    if (errno == EAGAIN || errno == EWOULDBLOCK) {
      /* We couldn't do a partial write here, probably because
         n <= PIPE_BUF and POSIX says that writes of less than
         PIPE_BUF characters must be atomic.
         So, we force a partial write of 1 character.
         This should always succeed if we've done a select
         on writing just before. */
      if (n > 1) { n = 1; goto again; }
    }
  }
#endif
  if (retcode == -1) sys_error(NO_ARG);
  return retcode;
}

/* Attempt to flush the buffer. This will make room in the buffer for
   at least one character. Returns true if the buffer is empty at the
   end of the flush, or false if some data remains in the buffer. */

int flush_partial(channel)
     struct channel * channel;
{
  int towrite, written;

  Lock(channel);
  towrite = channel->curr - channel->buff;
  if (towrite > 0) {
    written = do_write(channel->fd, channel->buff, towrite);
    channel->offset += written;
    if (written < towrite)
      bcopy(channel->buff + written, channel->buff, towrite - written);
    channel->curr -= written;
  }
  Unlock(channel);
  return (channel->curr == channel->buff);
}

/* Flush completely the buffer. */

void flush(channel)
     struct channel * channel;
{
  while (! flush_partial(channel)) /*nothing*/;
}

/* Output data */

void putword(channel, w)
     struct channel * channel;
     uint32 w;
{
  putch(channel, w >> 24);
  putch(channel, w >> 16);
  putch(channel, w >> 8);
  putch(channel, w);
}

int putblock(channel, p, len)
     struct channel * channel;
     char * p;
     long len;
{
  int n, free, towrite, written;

  n = len >= INT_MAX ? INT_MAX : (int) len;
  free = channel->end - channel->curr;
  if (n <= free) {
    /* Write request small enough to fit in buffer: transfer to buffer. */
    bcopy(p, channel->curr, n);
    channel->curr += n;
    return n;
  } else {
    /* Write request overflows buffer: transfer whatever fits to buffer
       and write the buffer */
    bcopy(p, channel->curr, free);
    towrite = channel->end - channel->buff;
    written = do_write(channel->fd, channel->buff, towrite);
    if (written < towrite)
      bcopy(channel->buff + written, channel->buff, towrite - written);
    channel->offset += written;
    channel->curr = channel->end - written;
    channel->max = channel->end - written;
    return free;
  }
}

void really_putblock(channel, p, len)
     struct channel * channel;
     char * p;
     long len;
{
  int written;
  while (len > 0) {
    written = putblock(channel, p, len);
    p += written;
    len -= written;
  }
}

void seek_out(channel, dest)
     struct channel * channel;
     long dest;
{
  flush(channel);
  if (lseek(channel->fd, dest, 0) != dest) sys_error(NO_ARG);
  channel->offset = dest;
}

long pos_out(channel)
     struct channel * channel;
{
  return channel->offset + channel->curr - channel->buff;
}

/* Input */

static int do_read(fd, p, n)
     int fd;
     char * p;
     unsigned n;
{
  int retcode;

  Assert(!Is_young(p));
  enter_blocking_section();
#ifdef HAS_UI
  retcode = ui_read(fd, p, n);
#else
#ifdef EINTR
  do { retcode = read(fd, p, n); } while (retcode == -1 && errno == EINTR);
#else
  retcode = read(fd, p, n);
#endif
#endif
  leave_blocking_section();
  if (retcode == -1) sys_error(NO_ARG);
  return retcode;
}

unsigned char refill(channel)
     struct channel * channel;
{
  int n;

  n = do_read(channel->fd, channel->buff, IO_BUFFER_SIZE);
  if (n == 0) raise_end_of_file();
  channel->offset += n;
  channel->max = channel->buff + n;
  channel->curr = channel->buff + 1;
  return (unsigned char)(channel->buff[0]);
}

uint32 getword(channel)
     struct channel * channel;
{
  int i;
  uint32 res;

  res = 0;
  for(i = 0; i < 4; i++) {
    res = (res << 8) + getch(channel);
  }
  return res;
}

int getblock(channel, p, len)
     struct channel * channel;
     char * p;
     long len;
{
  int n, avail, nread;

  n = len >= INT_MAX ? INT_MAX : (int) len;
  avail = channel->max - channel->curr;
  if (n <= avail) {
    bcopy(channel->curr, p, n);
    channel->curr += n;
    return n;
  } else if (avail > 0) {
    bcopy(channel->curr, p, avail);
    channel->curr += avail;
    return avail;
  } else if (n < IO_BUFFER_SIZE) {
    nread = do_read(channel->fd, channel->buff, IO_BUFFER_SIZE);
    channel->offset += nread;
    channel->max = channel->buff + nread;
    if (n > nread) n = nread;
    bcopy(channel->buff, p, n);
    channel->curr = channel->buff + n;
    return n;
  } else {
    nread = do_read(channel->fd, p, n);
    channel->offset += nread;
    return nread;
  }
}

int really_getblock(chan, p, n)
     struct channel * chan;
     char * p;
     long n;
{
  int r;
  while (n > 0) {
    r = getblock(chan, p, n);
    if (r == 0) break;
    p += r;
    n -= r;
  }
  return (n == 0);
}

void seek_in(channel, dest)
     struct channel * channel;
     long dest;
{
  if (dest >= channel->offset - (channel->max - channel->buff) &&
      dest <= channel->offset) {
    channel->curr = channel->max - (channel->offset - dest);
  } else {
    if (lseek(channel->fd, dest, SEEK_SET) != dest) sys_error(NO_ARG);
    channel->offset = dest;
    channel->curr = channel->max = channel->buff;
  }
}

long pos_in(channel)
     struct channel * channel;
{
  return channel->offset - (channel->max - channel->curr);
}

long input_scan_line(channel)
     struct channel * channel;
{
  char * p;
  int n;

  p = channel->curr;
  do {
    if (p >= channel->max) {
      /* No more characters available in the buffer */
      if (channel->curr > channel->buff) {
        /* Try to make some room in the buffer by shifting the unread
           portion at the beginning */
        bcopy(channel->curr, channel->buff, channel->max - channel->curr);
        n = channel->curr - channel->buff;
        channel->curr -= n;
        channel->max -= n;
        p -= n;
      }
      if (channel->max >= channel->end) {
        /* Buffer is full, no room to read more characters from the input.
           Return the number of characters in the buffer, with negative
           sign to indicate that no newline was encountered. */
        return -(channel->max - channel->curr);
      }
      /* Fill the buffer as much as possible */
      n = do_read(channel->fd, channel->max, channel->end - channel->max);
      if (n == 0) {
        /* End-of-file encountered. Return the number of characters in the
           buffer, with negative sign since we haven't encountered 
           a newline. */
        return -(channel->max - channel->curr);
      }
      channel->offset += n;
      channel->max += n;
    }
  } while (*p++ != '\n');
  /* Found a newline. Return the length of the line, newline included. */
  return (p - channel->curr);
}

/* Caml entry points for the I/O functions.  Wrap struct channel *
   objects into a heap-allocated, finalized object.  Perform locking
   and unlocking around the I/O operations. */

static void finalize_channel(vchan)
     value vchan;
{
  struct channel * chan = Channel(vchan);
  if (channel_mutex_free != NULL) (*channel_mutex_free)(chan);
  stat_free((char *) chan);
}

static value alloc_channel(chan)
     struct channel * chan;
{
  value res = alloc_final(2, finalize_channel, 1, 32);
  Field(res, 1) = (value) chan;
  return res;
}

value caml_open_descriptor(fd)       /* ML */
     value fd;
{
  return alloc_channel(open_descriptor(Int_val(fd)));
}

value channel_descriptor(vchannel)   /* ML */
     value vchannel;
{
  return Val_long(Channel(vchannel)->fd);
}

value caml_close_channel(vchannel)      /* ML */
     value vchannel;
{
  /* For output channels, must have flushed before */
  struct channel * channel = Channel(vchannel);
  close(channel->fd);
  channel->fd = -1;
  return Val_unit;
}

value caml_channel_size(vchannel)      /* ML */
     value vchannel;
{
  return Val_long(channel_size(Channel(vchannel)));
}

value caml_flush_partial(vchannel)            /* ML */
     value vchannel;
{
  return Val_bool(flush_partial(Channel(vchannel)));
}

value caml_flush(vchannel)            /* ML */
     value vchannel;
{
  flush(Channel(vchannel));
  return Val_unit;
}

value caml_output_char(vchannel, ch)  /* ML */
     value vchannel, ch;
{
  struct channel * channel = Channel(vchannel);
  Lock(channel);
  putch(channel, Long_val(ch));
  Unlock(channel);
  return Val_unit;
}

value caml_output_int(vchannel, w)    /* ML */
     value vchannel, w;
{
  struct channel * channel = Channel(vchannel);
  Lock(channel);
  putword(channel, Long_val(w));
  Unlock(channel);
  return Val_unit;
}

value caml_output_partial(vchannel, buff, start, length) /* ML */
     value vchannel, buff, start, length;
{
  struct channel * channel = Channel(vchannel);
  int res;
  Lock(channel);
  res = putblock(channel, &Byte(buff, Long_val(start)), Long_val(length));
  Unlock(channel);
  return Val_int(res);
}

value caml_output(vchannel, buff, start, length) /* ML */
     value vchannel, buff, start, length;
{
  struct channel * channel = Channel(vchannel);
  long pos = Long_val(start);
  long len = Long_val(length);

  Lock(channel);
  Begin_root(buff)
    while (len > 0) {
      int written = putblock(channel, &Byte(buff, pos), len);
      pos += written;
      len -= written;
    }
  End_roots();
  Unlock(channel);
  return Val_unit;
}

value caml_seek_out(vchannel, pos)    /* ML */
     value vchannel, pos;
{
  struct channel * channel = Channel(vchannel);
  Lock(channel);
  seek_out(channel, Long_val(pos));
  Unlock(channel);
  return Val_unit;
}

value caml_pos_out(vchannel)          /* ML */
     value vchannel;
{
  return Val_long(pos_out(Channel(vchannel)));
}

value caml_input_char(vchannel)       /* ML */
     value vchannel;
{
  struct channel * channel = Channel(vchannel);
  unsigned char c;

  Lock(channel);
  c = getch(channel);
  Unlock(channel);
  return Val_long(c);
}

value caml_input_int(vchannel)        /* ML */
     value vchannel;
{
  struct channel * channel = Channel(vchannel);
  long i;

  Lock(channel);
  i = getword(channel);
  Unlock(channel);
#ifdef ARCH_SIXTYFOUR
  i = (i << 32) >> 32;          /* Force sign extension */
#endif
  return Val_long(i);
}

value caml_input(vchannel, buff, start, length) /* ML */
     value vchannel, buff, start, length;
{
  struct channel * channel = Channel(vchannel);
  long res;

  Lock(channel);
  res = getblock(channel, &Byte(buff, Long_val(start)), Long_val(length));
  Unlock(channel);
  return Val_long(res);
}

value caml_seek_in(vchannel, pos)     /* ML */
     value vchannel, pos;
{
  struct channel * channel = Channel(vchannel);
  Lock(channel);
  seek_in(channel, Long_val(pos));
  Unlock(channel);
  return Val_unit;
}

value caml_pos_in(vchannel)           /* ML */
     value vchannel;
{
  return Val_long(pos_in(Channel(vchannel)));
}

value caml_input_scan_line(vchannel)       /* ML */
     value vchannel;
{
  struct channel * channel = Channel(vchannel);
  long res;

  Lock(channel);
  res = input_scan_line(channel);
  Unlock(channel);
  return Val_long(res);
}

