/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1996 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License.         */
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
#include "signals.h"
#include "stacks.h"
#include "sys.h"
#ifdef HAS_UI
#include "ui.h"
#endif

extern int errno;

#ifdef HAS_STRERROR

extern char * strerror(int);

char * error_message(void)
{
  return strerror(errno);
}

#else

extern int sys_nerr;
extern char * sys_errlist [];

char * error_message(void)
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

void sys_error(value arg)
{
  char * err;
  value str;
  
  if (errno == EAGAIN || errno == EWOULDBLOCK) {
    raise_sys_blocked_io();
  } else {
    err = error_message();
    if (arg == NO_ARG) {
      str = copy_string(err);
    } else {
      int err_len = strlen(err);
      int arg_len = string_length(arg);
      Begin_root(arg);
      str = alloc_string(arg_len + 2 + err_len);
      End_roots();
      bcopy(String_val(arg), &Byte(str, 0), arg_len);
      bcopy(": ", &Byte(str, arg_len), 2);
      bcopy(err, &Byte(str, arg_len + 2), err_len);
    }
    raise_sys_error(str);
  }
}

value sys_exit(value retcode)          /* ML */
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
  O_RDONLY, O_WRONLY, O_APPEND, O_CREAT, O_TRUNC, O_EXCL,
  O_BINARY, O_TEXT, O_NONBLOCK
};

value sys_open(value path, value flags, value perm) /* ML */
{
  int ret;
  ret = open(String_val(path), convert_flag_list(flags, sys_open_flags)
#if !macintosh
             , Int_val(perm)
#endif
                                       );
  if (ret == -1) sys_error(path);
  return Val_long(ret);
}

value sys_close(value fd)             /* ML */
{
  close(Int_val(fd));
  return Val_unit;
}

value sys_file_exists(value name)     /* ML */
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

value sys_remove(value name)          /* ML */
{
  int ret;
  ret = unlink(String_val(name));
  if (ret != 0) sys_error(name);
  return Val_unit;
}

value sys_rename(value oldname, value newname) /* ML */
{
  if (rename(String_val(oldname), String_val(newname)) != 0)
    sys_error(oldname);
  return Val_unit;
}

value sys_chdir(value dirname)        /* ML */
{
  if (chdir(String_val(dirname)) != 0) sys_error(dirname);
  return Val_unit;
}

value sys_getcwd(value unit)          /* ML */
{
  char buff[4096];
#ifdef HAS_GETCWD
  if (getcwd(buff, sizeof(buff)) == 0) sys_error(NO_ARG);
#else
  if (getwd(buff) == 0) sys_error(NO_ARG);
#endif /* HAS_GETCWD */
  return copy_string(buff);
}

value sys_getenv(value var)           /* ML */
{
  char * res;

  res = getenv(String_val(var));
  if (res == 0) raise_not_found();
  return copy_string(res);
}

static char ** main_argv;

value sys_get_argv(value unit)        /* ML */
{
  return copy_string_array(main_argv);
}

void sys_init(char **argv)
{
  main_argv = argv;
}

#if !(defined(WIFEXITED) && defined(WEXITSTATUS))
/* Assume old-style V7 status word */
#define WIFEXITED(status) (((status) & 0xFF) == 0)
#define WEXITSTATUS(status) (((status) >> 8) & 0xFF)
#endif

value sys_system_command(value command)   /* ML */
{
  int status, retcode;
#ifndef _WIN32
  status = system(String_val(command));
  if (WIFEXITED(status))
    retcode = WEXITSTATUS(status);
  else
    retcode = 255;
#else
  status = retcode = win32_system(String_val(command));
#endif
  if (status == -1) sys_error(command);
  return Val_int(retcode);
}

value sys_time(value unit)            /* ML */
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

value sys_random_seed (value unit)       /* ML */
{
#ifdef HAS_GETTIMEOFDAY
  struct timeval tv;
  gettimeofday(&tv, NULL);
  return Val_int(tv.tv_sec ^ tv.tv_usec);
#else
  return Val_int(time (NULL));
#endif
}

value sys_get_config(value unit)  /* ML */
{
  value result;
  value ostype;

  ostype = copy_string(OCAML_OS_TYPE);
  Begin_root(ostype);
    result = alloc_small (2, 0);
    Field(result, 0) = ostype;
    Field(result, 1) = Val_long (8 * sizeof(value));
  End_roots ();
  return result;
}

/* Search path function */

#ifdef _WIN32

#define S_ISREG(mode) (((mode) & S_IFMT) == S_IFREG)

char * searchpath(char * name)
{
  char * fullname;
  char * path;
  char * p;
  char * q;
  struct stat st;

  if (stat(name, &st) == 0) return name;
  path = getenv("PATH");
  if (path == NULL) return 0;
  fullname = stat_alloc(strlen(name) + strlen(path) + 6);
  strcpy(fullname, name);
  strcat(fullname, ".exe");
  if (stat(fullname, &st) == 0) return fullname;
  for (p = name; *p != 0; p++) {
    if (*p == '/' || *p == '\\' || *p == ':') return name;
  }
  while(1) {
    for (p = fullname; *path != 0 && *path != ';'; p++, path++) *p = *path;
    if (p != fullname && p[-1] != '\\') *p++ = '\\';
    for (q = name; *q != 0; p++, q++) *p = *q;
    *p = 0;
    if (stat(fullname, &st) == 0 && S_ISREG(st.st_mode)) break;
    strcpy(p, ".exe");
    if (stat(fullname, &st) == 0 && S_ISREG(st.st_mode)) break;
    if (*path == 0) return 0;
    path++;
  }
  return fullname;
}

#elif macintosh

/* We don't need searchpath on the Macintosh because there are no #! scripts */

char *searchpath (char * name)
{
  return name;
}

#else

#ifndef S_ISREG
#define S_ISREG(mode) (((mode) & S_IFMT) == S_IFREG)
#endif

char * searchpath(char * name)
{
  char * fullname;
  char * path;
  char * p;
  char * q;
  struct stat st;

  for (p = name; *p != 0; p++) {
    if (*p == '/') return name;
  }
  path = getenv("PATH");
  if (path == NULL) return 0;
  fullname = stat_alloc(strlen(name) + strlen(path) + 2);
  while(1) {
    for (p = fullname; *path != 0 && *path != ':'; p++, path++) *p = *path;
    if (p != fullname) *p++ = '/';
    for (q = name; *q != 0; p++, q++) *p = *q;
    *p = 0;
    if (stat(fullname, &st) == 0 && S_ISREG(st.st_mode)) break;
    if (*path == 0) return 0;
    path++;
  }
  return fullname;
}

#endif /* _WIN32, macintosh, ... */

/* Wrapper around "system" for Win32 */

#ifdef _WIN32

#include <ctype.h>
extern char * mktemp(char *);

int win32_system(char * cmdline)
{
#define MAX_CMD_LENGTH 256
  char cmd[MAX_CMD_LENGTH + 16];
  char template[9];
  char * tempfile;
  FILE * fd;
  int len, i, j, k, retcode;

  len = strlen(cmdline);
  if (len < 1000) {
    return system(cmdline);
  } else {
    /* Skip initial blanks, if any */
    for (i = 0; cmdline[i] != 0 && isspace(cmdline[i]); i++) /*nothing*/;
    /* Copy command name to buffer, stop at first blank */
    for (j = 0; cmdline[i] != 0 && ! isspace(cmdline[i]); i++) {
      if (j < MAX_CMD_LENGTH) cmd[j++] = cmdline[i];
    }
    /* Save remainder of command line to temp file */
    strcpy(template, "cmXXXXXX");
    tempfile = mktemp(template);
    fd = fopen(tempfile, "w");
    if (fd == NULL) return -1;
    for (k = i; k < len; k++)
      fputc((isspace(cmdline[k]) ? '\n' : cmdline[k]), fd);
    fclose(fd);
    /* Add " @tempfile" to the command line */
    sprintf(cmd + j, " @%s", tempfile);
    /* Run command */
    retcode = system(cmd);
    /* Remove temp file and exit */
    unlink(tempfile);
    return retcode;
  }
}

#endif
