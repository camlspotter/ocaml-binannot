/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*           Xavier Leroy, projet Cristal, INRIA Rocquencourt          */
/*                                                                     */
/*  Copyright 1996 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License, with    */
/*  the special exception on linking described in file ../LICENSE.     */
/*                                                                     */
/***********************************************************************/

/* $Id$ */

/* Win32-specific stuff */

#include <windows.h>
#include <stdlib.h>
#include <stdio.h>
#ifndef HAS_UI
#include <io.h>
#endif
#include <fcntl.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <ctype.h>
#include <string.h>
#include <signal.h>
#include "memory.h"
#include "misc.h"
#include "osdeps.h"
#include "signals.h"

#ifndef S_ISREG
#define S_ISREG(mode) (((mode) & S_IFMT) == S_IFREG)
#endif

char * decompose_path(struct ext_table * tbl, char * path)
{
  char * p, * q;
  int n;

  if (path == NULL) return NULL;
  p = stat_alloc(strlen(path) + 1);
  strcpy(p, path);
  q = p;
  while (1) {
    for (n = 0; q[n] != 0 && q[n] != ';'; n++) /*nothing*/;
    ext_table_add(tbl, q);
    q = q + n;
    if (*q == 0) break;
    *q = 0;
    q += 1;
  }
  return p;
}

char * search_in_path(struct ext_table * path, char * name)
{
  char * p, * fullname;
  int i;
  struct stat st;

  for (p = name; *p != 0; p++) {
    if (*p == '/' || *p == '\\') goto not_found;
  }
  for (i = 0; i < path->size; i++) {
    fullname = stat_alloc(strlen((char *)(path->contents[i])) +
                          strlen(name) + 2);
    strcpy(fullname, (char *)(path->contents[i]));
    strcat(fullname, "\\");
    strcat(fullname, name);
    gc_message(0x100, "Searching %s\n", (unsigned long) fullname);
    if (stat(fullname, &st) == 0 && S_ISREG(st.st_mode)) return fullname;
    stat_free(fullname);
  }
 not_found:
  gc_message(0x100, "%s not found in search path\n", (unsigned long) name);
  fullname = stat_alloc(strlen(name) + 1);
  strcpy(fullname, name);
  return fullname;
}
  
CAMLexport char * search_exe_in_path(char * name)
{
#define MAX_PATH_LENGTH 512
  char * fullname = stat_alloc(512);
  char * filepart;

  if (! SearchPath(NULL,              /* use system search path */
                   name,
                   ".exe",            /* add .exe extension if needed */
                   MAX_PATH_LENGTH,   /* size of buffer */
                   fullname,
                   &filepart))
    strcpy(fullname, name);
  return fullname;
}

char * search_dll_in_path(struct ext_table * path, char * name)
{
  char * dllname = stat_alloc(strlen(name) + 5);
  char * res;
  strcpy(dllname, name);
  strcat(dllname, ".dll");
  res = search_in_path(path, dllname);
  stat_free(dllname);
  return res;
}

void * caml_dlopen(char * libname)
{
  return (void *) LoadLibrary(libname);
}

void caml_dlclose(void * handle)
{
  FreeLibrary((HMODULE) handle);
}

void * caml_dlsym(void * handle, char * name)
{
  return (void *) GetProcAddress((HMODULE) handle, name);
}

char * caml_dlerror(void)
{
  static char dlerror_buffer[256];
  DWORD msglen =
    FormatMessage(FORMAT_MESSAGE_FROM_SYSTEM | FORMAT_MESSAGE_IGNORE_INSERTS,
                  NULL,         /* message source */
                  GetLastError(), /* error number */
                  0,            /* default language */
                  dlerror_buffer, /* destination */
                  sizeof(dlerror_buffer), /* size of destination */
                  NULL);         /* no inserts */
  if (msglen == 0)
    return "unknown error";
  else
    return dlerror_buffer;
}

/* Proper emulation of signal(), including ctrl-C and ctrl-break */

typedef void (*sighandler)(int sig);
static int ctrl_handler_installed = 0;
static volatile sighandler ctrl_handler_action = SIG_DFL;

static BOOL WINAPI ctrl_handler(DWORD event)
{
  int saved_mode;
  sighandler action;

  /* Only ctrl-C and ctrl-Break are handled */
  if (event != CTRL_C_EVENT && event != CTRL_BREAK_EVENT) return FALSE;
  /* Default behavior is to exit, which we get by not handling the event */
  if (ctrl_handler_action == SIG_DFL) return FALSE;
  /* Ignore behavior is to do nothing, which we get by claiming that we
     have handled the event */
  if (ctrl_handler_action == SIG_IGN) return TRUE;
  /* Reset handler to default action for consistency with signal() */
  action = ctrl_handler_action;
  ctrl_handler_action = SIG_DFL;
  /* Call user-provided signal handler.  Win32 doesn't like it when
     we do a longjmp() at this point (it looks like we're running in
     a different thread than the main program!).  So, pretend we are not in
     async signal mode, so that the handler simply records the signal. */
  saved_mode = async_signal_mode;
  async_signal_mode = 0;
  action(SIGINT);
  async_signal_mode = saved_mode;
  /* We have handled the event */
  return TRUE;
}

sighandler win32_signal(int sig, sighandler action)
{
  sighandler oldaction;

  if (sig != SIGINT) return signal(sig, action);
  if (! ctrl_handler_installed) {
    SetConsoleCtrlHandler(ctrl_handler, TRUE);
    ctrl_handler_installed = 1;
  }
  oldaction = ctrl_handler_action;
  ctrl_handler_action = action;
  return oldaction;
}

/* Expansion of @responsefile and *? file patterns in the command line */

#ifndef HAS_UI

static int argc;
static char ** argv;
static int argvsize;

static void store_argument(char * arg);
static void expand_argument(char * arg);
static void expand_pattern(char * arg);
static void expand_diversion(char * filename);

static void out_of_memory(void)
{
  fprintf(stderr, "Out of memory while expanding command line\n");
  exit(2);
}

static void store_argument(char * arg)
{
  if (argc + 1 >= argvsize) {
    argvsize *= 2;
    argv = (char **) realloc(argv, argvsize * sizeof(char *));
    if (argv == NULL) out_of_memory();
  }
  argv[argc++] = arg;
}

static void expand_argument(char * arg)
{
  char * p;

  if (arg[0] == '@') {
    expand_diversion(arg + 1);
    return;
  }
  for (p = arg; *p != 0; p++) {
    if (*p == '*' || *p == '?') {
      expand_pattern(arg);
      return;
    }
  }
  store_argument(arg);
}

static void expand_pattern(char * pat)
{
  int handle;
  struct _finddata_t ffblk;
  int preflen;

  handle = _findfirst(pat, &ffblk);
  if (handle == -1) {
    store_argument(pat); /* a la Bourne shell */
    return;
  }
  for (preflen = strlen(pat); preflen > 0; preflen--) {
    char c = pat[preflen - 1];
    if (c == '\\' || c == '/' || c == ':') break;
  }
  do {
    char * name = malloc(preflen + strlen(ffblk.name) + 1);
    if (name == NULL) out_of_memory();
    memcpy(name, pat, preflen);
    strcpy(name + preflen, ffblk.name);
    store_argument(name);
  } while (_findnext(handle, &ffblk) != -1);
  _findclose(handle);
}

static void expand_diversion(char * filename)
{
  struct _stat stat;
  int fd;
  char * buf, * endbuf, * p, * s;

  if (_stat(filename, &stat) == -1 ||
      (fd = _open(filename, O_RDONLY | O_BINARY, 0)) == -1) {
    fprintf(stderr, "Cannot open file %s\n", filename);
    exit(2);
  }
  buf = (char *) malloc(stat.st_size + 1);
  if (buf == NULL) out_of_memory();
  _read(fd, buf, stat.st_size);
  endbuf = buf + stat.st_size;
  _close(fd);
  for (p = buf; p < endbuf; /*nothing*/) {
    /* Skip leading blanks */
    while (p < endbuf && isspace(*p)) p++;
    if (p >= endbuf) break;
    s = p;
    /* Skip to next blank or end of buffer */
    while (p < endbuf && !isspace(*p)) p++;
    /* Delimit argument and expand it */
    *p++ = 0;
    expand_argument(s);
  }
}

CAMLexport void expand_command_line(int * argcp, char *** argvp)
{
  int i;
  argc = 0;
  argvsize = 16;
  argv = (char **) malloc(argvsize * sizeof(char *));
  if (argv == NULL) out_of_memory();
  for (i = 0; i < *argcp; i++) expand_argument((*argvp)[i]);
  argv[argc] = NULL;
  *argcp = argc;
  *argvp = argv;
}

#endif

/* Wrapper around "system" for Win32.  Create a diversion file if
   command line is too long. */

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

/* Wrapper around "getenv" for Win32.  Look up resources if environment
   variable is not set. Notice that the result is a pointer to static
   memory space that will be overwritten at the next call; this is
   OK since sys_getenv immediately copies the returned string. */

char * win32_getenv(char * name)
{
  char * res;
  HKEY hkey;
  DWORD type, size;
  static char buf[256];

  res = getenv(name);
  if (res != NULL) return res;
  if (RegOpenKeyEx(HKEY_CURRENT_USER, /* root directory */
		   "Software\\ocaml", /* entry name */
		   0,                 /* reserved */
		   KEY_QUERY_VALUE,   /* access rights requested */
		   &hkey)             /* [out] result */
      != ERROR_SUCCESS) return NULL;
  size = sizeof(buf);
  if (RegQueryValueEx(hkey,           /* key */
		      name,           /* value name */
		      0,              /* reserved */
		      &type,          /* [out] type of value */
		      buf,            /* [in,out] where to store the value */
		      &size)          /* [in,out] size of buffer */
      != ERROR_SUCCESS) {
    RegCloseKey(hkey);
    return NULL;
  }
  RegCloseKey(hkey);
  if (type != REG_SZ) return NULL;
  buf[size] = 0; /* make sure string is terminated */
  return buf;
}
  
#ifndef NATIVE_CODE

/* Set up a new thread for control-C emulation */

void caml_signal_thread(void * lpParam)
{
  char *endptr;
  HANDLE h;
  /* Get an hexa-code raw handle through the environment */
  h = (HANDLE) strtol(getenv("CAMLSIGPIPE"), &endptr, 16);
  while (1) {
    DWORD numread;
    BOOL ret;
    char iobuf[2];
    /* This shall always return a single character */
    ret = ReadFile(h, iobuf, 1, &numread, NULL);
    if (!ret || numread != 1) sys_exit(Val_int(0));
    switch (iobuf[0]) {
    case 'C':
      pending_signal = SIGINT;
      something_to_do = 1;
      break;
    case 'T':
      exit(0);
      break;
    }
  }
}

#endif
