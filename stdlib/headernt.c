/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1998 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License.         */
/*                                                                     */
/***********************************************************************/

/* $Id$ */

#include <string.h>
#include <wtypes.h>
#include <winbase.h>
#include <process.h>
#include "../byterun/exec.h"

char * default_runtime_name = "ocamlrun";

static unsigned long read_size(char * ptr)
{
  unsigned char * p = (unsigned char *) ptr;
  return ((unsigned long) p[0] << 24) + ((unsigned long) p[1] << 16) +
         ((unsigned long) p[2] << 8) + p[3];
}

static char * read_runtime_path(HANDLE h)
{
  char buffer[TRAILER_SIZE];
  static char runtime_path[MAX_PATH];
  DWORD nread;
  struct exec_trailer tr;
  long size;

  if (SetFilePointer(h, -TRAILER_SIZE, NULL, FILE_END) == -1) return NULL;
  if (! ReadFile(h, buffer, TRAILER_SIZE, &nread, NULL)) return NULL;
  if (nread != TRAILER_SIZE) return NULL;
  tr.path_size = read_size(buffer);
  tr.code_size = read_size(buffer + 4);
  tr.prim_size = read_size(buffer + 8);
  tr.data_size = read_size(buffer + 12);
  tr.symbol_size = read_size(buffer + 16);
  tr.debug_size = read_size(buffer + 20);
  if (tr.path_size >= MAX_PATH) return NULL;
  if (tr.path_size == 0) return default_runtime_name;
  size = tr.path_size + tr.code_size + tr.prim_size +
         tr.data_size + tr.symbol_size + tr.debug_size + TRAILER_SIZE;
  if (SetFilePointer(h, -size, NULL, FILE_END) == -1) return NULL;
  if (! ReadFile(h, runtime_path, tr.path_size, &nread, NULL)) return NULL;
  if (nread != tr.path_size) return NULL;
  runtime_path[tr.path_size - 1] = 0;
  return runtime_path;
}

static void errwrite(char * msg)
{
  DWORD numwritten;
  WriteFile(GetStdHandle(STD_ERROR_HANDLE), msg, strlen(msg),
            &numwritten, NULL);
}

int main(int argc, char ** argv)
{
  char truename[MAX_PATH];
  char * cmdline = GetCommandLine();
  char * runtime_path;
  HANDLE h;
  int retcode;

  GetModuleFileName(NULL, truename, sizeof(truename));
  h = CreateFile(truename, GENERIC_READ, FILE_SHARE_READ | FILE_SHARE_WRITE,
                 NULL, OPEN_EXISTING, 0, NULL);
  if (h == INVALID_HANDLE_VALUE ||
      (runtime_path = read_runtime_path(h)) == NULL) {
    errwrite(truename);
    errwrite(" not found or is not a bytecode executable file\r\n");
    return 2;
  }
  CloseHandle(h);
  retcode = spawnlp(P_WAIT, runtime_path, cmdline, NULL);
  /* We use P_WAIT instead of P_OVERLAY here because under NT,
     P_OVERLAY returns to the command interpreter, displaying the prompt
     before executing the command. */
  if (retcode == -1) {
    errwrite("Cannot exec ");
    errwrite(runtime_path);
    errwrite("\r\n");
    return 2;
  }
  return retcode;
}

/* Prevent VC++ from linking its own _setargv function, which
   performs command-line processing (we don't need it) */

static void _setargv() { }

