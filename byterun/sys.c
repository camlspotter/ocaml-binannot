/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1996 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License, with    */
/*  the special exception on linking described in file ../LICENSE.     */
/*                                                                     */
/***********************************************************************/

/* $Id$ */

/* Basic system calls */

#include <errno.h>
#include <fcntl.h>
#include <signal.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <time.h>
#if !macintosh
#include <sys/types.h>
#include <sys/stat.h>
#endif
#if !macintosh && !_WIN32
#include <sys/wait.h>
#endif
#if macintosh
#include "macintosh.h"
#endif
#include "config.h"
#ifdef HAS_UNISTD
#include <unistd.h>
#endif
#ifdef HAS_TIMES
#include <sys/times.h>
#endif
#ifdef HAS_GETTIMEOFDAY
#include <sys/time.h>
#endif
#include "alloc.h"
#include "debugger.h"
#include "fail.h"
#include "instruct.h"
#include "mlvalues.h"
#include "osdeps.h"
#include "signals.h"
#include "stacks.h"
#include "sys.h"
#ifdef HAS_UI
#include "ui.h"
#endif

#ifndef _WIN32
extern int errno;
#endif

#ifdef HAS_STRERROR

#ifndef _WIN32
extern char * strerror(int);
#endif

static char * error_message(void)
{
  return strerror(errno);
}

#else

extern int sys_nerr;
extern char * sys_errlist [];

static char * error_message(void)
{
  if (errno < 0 || errno >= sys_nerr)
    return "unknown error";
  else
    return sys_errlist[errno];
}

#endif /* HAS_STRERROR */

#ifndef EAGAIN
#define EAGAIN (-1)
#endif
#ifndef EWOULDBLOCK
#define EWOULDBLOCK (-1)
#endif

CAMLexport void caml_sys_error(value arg)
{
  CAMLparam1 (arg);
  char * err;
  CAMLlocal1 (str);
  
  if (errno == EAGAIN || errno == EWOULDBLOCK) {
    raise_sys_blocked_io();
  } else {
    err = error_message();
    if (arg == NO_ARG) {
      str = caml_copy_string(err);
    } else {
      int err_len = strlen(err);
      int arg_len = caml_string_length(arg);
      str = caml_alloc_string(arg_len + 2 + err_len);
      memmove(&Byte(str, 0), String_val(arg), arg_len);
      memmove(&Byte(str, arg_len), ": ", 2);
      memmove(&Byte(str, arg_len + 2), err, err_len);
    }
    raise_sys_error(str);
  }
}

CAMLprim value caml_sys_exit(value retcode)
{
#ifndef NATIVE_CODE
  debugger(PROGRAM_EXIT);
#endif
#ifdef HAS_UI
  ui_exit(Int_val(retcode));
#else
  exit(Int_val(retcode));
#endif
  return Val_unit;
}

#ifndef O_BINARY
#define O_BINARY 0
#endif
#ifndef O_TEXT
#define O_TEXT 0
#endif
#ifndef O_NONBLOCK
#ifdef O_NDELAY
#define O_NONBLOCK O_NDELAY
#else
#define O_NONBLOCK 0
#endif
#endif

static int sys_open_flags[] = {
  O_RDONLY, O_WRONLY, O_APPEND | O_WRONLY, O_CREAT, O_TRUNC, O_EXCL,
  O_BINARY, O_TEXT, O_NONBLOCK
};

CAMLprim value caml_sys_open(value path, value flags, value perm)
{
  CAMLparam3(path, flags, perm);
  int fd;
  char * p;

  p = caml_stat_alloc(caml_string_length(path) + 1);
  strcpy(p, String_val(path));
  /* open on a named FIFO can block (PR#1533) */
  enter_blocking_section();
  fd = open(p, caml_convert_flag_list(flags, sys_open_flags)
#if !macintosh
             , Int_val(perm)
#endif
                                       );
  leave_blocking_section();
  caml_stat_free(p);
  if (fd == -1) caml_sys_error(path);
#if defined(F_SETFD) && defined(FD_CLOEXEC)
  fcntl(fd, F_SETFD, FD_CLOEXEC);
#endif
  CAMLreturn(Val_long(fd));
}

CAMLprim value caml_sys_close(value fd)
{
  close(Int_val(fd));
  return Val_unit;
}

CAMLprim value caml_sys_file_exists(value name)
{
#if macintosh
  int f;
  f = open (String_val (name), O_RDONLY);
  if (f == -1) return (Val_bool (0));
  close (f);
  return (Val_bool (1));
#else
  struct stat st;
  return Val_bool(stat(String_val(name), &st) == 0);
#endif
}

CAMLprim value caml_sys_remove(value name)
{
  int ret;
  ret = unlink(String_val(name));
  if (ret != 0) caml_sys_error(name);
  return Val_unit;
}

CAMLprim value caml_sys_rename(value oldname, value newname)
{
  if (rename(String_val(oldname), String_val(newname)) != 0)
    caml_sys_error(oldname);
  return Val_unit;
}

CAMLprim value caml_sys_chdir(value dirname)
{
  if (chdir(String_val(dirname)) != 0) caml_sys_error(dirname);
  return Val_unit;
}

CAMLprim value caml_sys_getcwd(value unit)
{
  char buff[4096];
#ifdef HAS_GETCWD
  if (getcwd(buff, sizeof(buff)) == 0) caml_sys_error(NO_ARG);
#else
  if (getwd(buff) == 0) caml_sys_error(NO_ARG);
#endif /* HAS_GETCWD */
  return caml_copy_string(buff);
}

CAMLprim value caml_sys_getenv(value var)
{
  char * res;

  res = getenv(String_val(var));
  if (res == 0) raise_not_found();
  return caml_copy_string(res);
}

char * caml_exe_name;
static char ** caml_main_argv;

CAMLprim value caml_sys_get_argv(value unit)
{
  CAMLparam0 ();   /* unit is unused */
  CAMLlocal3 (exe_name, argv, res);
  exe_name = caml_copy_string(caml_exe_name);
  argv = caml_copy_string_array((char const **) caml_main_argv);
  res = caml_alloc_small(2, 0);
  Field(res, 0) = exe_name;
  Field(res, 1) = argv;
  CAMLreturn(res);
}

void caml_sys_init(char * exe_name, char **argv)
{
  caml_exe_name = exe_name;
  caml_main_argv = argv;
}

#ifdef _WIN32
#define WIFEXITED(status) 1
#define WEXITSTATUS(status) (status)
#else
#if !(defined(WIFEXITED) && defined(WEXITSTATUS))
/* Assume old-style V7 status word */
#define WIFEXITED(status) (((status) & 0xFF) == 0)
#define WEXITSTATUS(status) (((status) >> 8) & 0xFF)
#endif
#endif

CAMLprim value caml_sys_system_command(value command)
{
  CAMLparam1 (command);
  int status, retcode;
  char *buf;
  unsigned long len;
  
  len = caml_string_length (command);
  buf = caml_stat_alloc (len + 1);
  memmove (buf, String_val (command), len + 1);
  enter_blocking_section ();
  status = system(buf);
  leave_blocking_section ();
  caml_stat_free(buf);
  if (status == -1) caml_sys_error(command);
  if (WIFEXITED(status))
    retcode = WEXITSTATUS(status);
  else
    retcode = 255;
  CAMLreturn (Val_int(retcode));
}

CAMLprim value caml_sys_time(value unit)
{
#ifdef HAS_TIMES
#ifndef CLK_TCK
#ifdef HZ
#define CLK_TCK HZ
#else
#define CLK_TCK 60
#endif
#endif
  struct tms t;
  times(&t);
  return copy_double((double)(t.tms_utime + t.tms_stime) / CLK_TCK);
#else
  /* clock() is standard ANSI C */
  return copy_double((double)clock() / CLOCKS_PER_SEC);
#endif
}

CAMLprim value caml_sys_random_seed (value unit)
{
  long seed;
#ifdef HAS_GETTIMEOFDAY
  struct timeval tv;
  gettimeofday(&tv, NULL);
  seed = tv.tv_sec ^ tv.tv_usec;
#else
  seed = time (NULL);
#endif
#ifdef HAS_UNISTD
  seed ^= getppid() << 16 | getpid();
#endif
  return Val_long(seed);
}

CAMLprim value caml_sys_get_config(value unit)
{
  CAMLparam0 ();   /* unit is unused */
  CAMLlocal2 (result, ostype);

  ostype = caml_copy_string(OCAML_OS_TYPE);
  result = caml_alloc_small (2, 0);
  Field(result, 0) = ostype;
  Field(result, 1) = Val_long (8 * sizeof(value));
  CAMLreturn (result);
}

CAMLprim value caml_sys_read_directory(value path)
{
  CAMLparam1(path);
  CAMLlocal1(result);
  struct ext_table tbl;

  caml_ext_table_init(&tbl, 50);
  if (caml_read_directory(String_val(path), &tbl) == -1) caml_sys_error(path);
  caml_ext_table_add(&tbl, NULL);
  result = caml_copy_string_array((char const **) tbl.contents);
  caml_ext_table_free(&tbl, 1);
  CAMLreturn(result);
}
