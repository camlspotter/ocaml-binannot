/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*  Developed by Jacob Navia.                                          */
/*  Copyright 2001 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License, with    */
/*  the special exception on linking described in file ../LICENSE.     */
/*                                                                     */
/***********************************************************************/

/* $Id$ */

#include <windows.h>
#include <stdio.h>
#include <windows.h>
#include <direct.h>
#include "inria.h"
extern int _get_osfhandle(int);
PROCESS_INFORMATION pi;
#define BUFSIZE 4096
STARTUPINFO startInfo;

/*------------------------------------------------------------------------
 Procedure:     ShowDbgMsg ID:1
 Purpose:       Puts up a dialog box with a message, forcing it to
                the foreground.
 Input:
 Output:
 Errors:
------------------------------------------------------------------------*/
void ShowDbgMsg(char *str)
{
	HWND hWnd;
	char p[20], message[255];
	hWnd = hwndMain;
	if (IsIconic(hWnd)){
		ShowWindow(hWnd,SW_RESTORE);
	}
	strncpy(message, str, 254);
	message[254] = 0;
	strcpy(p, "Error");
	MessageBox(hWnd, message, p, MB_OK | MB_ICONHAND|MB_TASKMODAL|MB_SETFOREGROUND);
}

int AskYesOrNo(char *msg)
{
	HWND hwnd;
	int r;

	hwnd = hwndMain;
	r = MessageBox(hwnd, msg, "Ocaml", MB_YESNO | MB_SETFOREGROUND);
	if (r == IDYES)
		return (TRUE);
	return (FALSE);
}


static DWORD OcamlStatus;

static int RegistryError(void)
{
	char buf[512];

	wsprintf(buf,"Error %d writing to the registry",GetLastError());
	ShowDbgMsg(buf);
	return 0;
}

static int DoCreateKey(HANDLE hkey,char *name,HKEY *hresult)
{
	unsigned long disp;

	return  RegCreateKeyEx(hkey,name,0,NULL,0,KEY_ALL_ACCESS,NULL,hresult,&disp);
}

/*------------------------------------------------------------------------
 Procedure:     GetOcamlPath ID:1
 Purpose:       Reads the registry key
                HKEY_CURRENT_USER\Software\Ocaml, and  creates it if
                it doesn't exists. If any error occurs, i.e. the
                given path doesn't exist, or the key didn't exist, it
                will put up a browse dialog box to allow the user to
                enter the path. The path will be verified that it
                points to a file that exists. If that file is in a
                directory called 'bin', it will look for another
                directory in the same level called lib' and set the
                Lib path to that.
 Input:         None explicit
 Output:        1 means sucess, zero failure
 Errors:        Almost all system calls will be verified
------------------------------------------------------------------------*/
int GetOcamlPath(void)
{
	HKEY hkeySoftware,hkeyOcaml;
	DWORD dwType;
	unsigned long siz;
	char *p,buf[512];
	FILE *f;

	if (RegOpenKeyExA(HKEY_CURRENT_USER,"Software",0,KEY_QUERY_VALUE,&hkeySoftware) != ERROR_SUCCESS)
		return 0;
	if (DoCreateKey(hkeySoftware,"ocaml",&hkeyOcaml) != ERROR_SUCCESS) {
		return RegistryError();
	}
	dwType = REG_SZ;
	siz = sizeof(buf);
	memset(buf,0,sizeof(buf));
	RegQueryValueExA(hkeyOcaml,"InterpreterPath",0,&dwType,buf,&siz);
	if (buf[0] == 0) {
		if (!BrowseForFile("Ocaml interpreter|ocaml.exe",buf)) {
			ShowDbgMsg("Impossible to find ocaml.exe. I quit");
			RegCloseKey(hkeyOcaml);
			RegCloseKey(hkeySoftware);
			exit(0);
		}
		RegSetValueEx(hkeyOcaml,"InterpreterPath",0,REG_SZ,buf,strlen(buf)+1);
	}
	f = fopen(buf,"r");
	if (f == NULL) {
		char *errormsg = malloc(1024);
		wsprintf(errormsg,"Incorrect path for ocaml.exe:\n%s",buf);
		ShowDbgMsg(errormsg);
		free(errormsg);
		buf[0] = 0;
		RegSetValueEx(hkeyOcaml,"InterpreterPath",0,REG_SZ,buf,1);
		RegCloseKey(hkeyOcaml);
		RegCloseKey(hkeySoftware);
		return GetOcamlPath();
	}
	else fclose(f);
	RegCloseKey(hkeyOcaml);
	RegCloseKey(hkeySoftware);
	strcpy(OcamlPath,buf);
	p = strrchr(OcamlPath,'\\');
	if (p) {
		*p = 0;
		strcpy(LibDir,OcamlPath);
		*p = '\\';
		p = strrchr(LibDir,'\\');
		if (p && !stricmp(p,"\\bin")) {
			*p = 0;
			strcat(LibDir,"\\lib");
		}
	}
	return 1;
}

static HANDLE hChildStdinRd, hChildStdinWr,hChildStdoutRd, hChildStdoutWr;
/*------------------------------------------------------------------------
 Procedure:     IsWindowsNT ID:1
 Purpose:       Returns 1 if we are running under windows NT, zero
                otherwise.
 Input:         None
 Output:        1 or zero
 Errors:
------------------------------------------------------------------------*/
int IsWindowsNT(void)
{
	OSVERSIONINFO osv;

	osv.dwOSVersionInfoSize = sizeof(osv);
	GetVersionEx(&osv);
	return(osv.dwPlatformId == VER_PLATFORM_WIN32_NT);
}

/*------------------------------------------------------------------------
 Procedure:     DoStartOcaml ID:1
 Purpose:       Starts the ocaml interpreter ocaml.exe. The standard
                input of the interpreter will be connected to a pipe,
                and the standard output and standard error to another
                pipe. The interpreter starts as a hidden process,
                showing only in the task list. Since this is in an
                own thread, its workings are independent of the rest
                of the program. After starting the interpreter, the
                thread waits in case the interpreter exits, for
                instance if the user or some program types #quit;;.
                In this case, the waiting thread awakens and exits
                the user interface.
 Input:         Not used. It uses the OcamlPath global variable, that
                is supposed to be correct, no test for its validity
                are done here.
 Output:        None visible
 Errors:        If any system call for whatever reason fails, the
                thread will exit. No error message is shown.
------------------------------------------------------------------------*/
int _stdcall DoStartOcaml(HWND hwndParent)
{
	char *cmdline;
	int processStarted;
	LPSECURITY_ATTRIBUTES lpsa=NULL;
	SECURITY_ATTRIBUTES sa;
	SECURITY_DESCRIPTOR sd;

	sa.nLength = sizeof(SECURITY_ATTRIBUTES);
	// Under windows NT/2000/Whistler we have to initialize the security descriptors
	// This is not necessary under windows 98/95.
	if (IsWindowsNT()) {
		InitializeSecurityDescriptor(&sd,SECURITY_DESCRIPTOR_REVISION);
		SetSecurityDescriptorDacl(&sd,TRUE,NULL,FALSE);
		sa.bInheritHandle = TRUE;
		sa.lpSecurityDescriptor = &sd;
		lpsa = &sa;
	}
	memset(&startInfo,0,sizeof(STARTUPINFO));
	startInfo.cb = sizeof(STARTUPINFO);
	// Create a pipe for the child process's STDOUT.
	if (! CreatePipe(&hChildStdoutRd, &hChildStdoutWr, &sa, 0))
		return 0;
	// Create a pipe for the child process's STDIN.
	if (! CreatePipe(&hChildStdinRd, &hChildStdinWr, &sa, 0))
		return 0;
	// Setup the start info structure
	startInfo.dwFlags = STARTF_USESTDHANDLES|STARTF_USESHOWWINDOW;
	startInfo.wShowWindow = SW_HIDE;
	startInfo.hStdOutput = hChildStdoutWr;
	startInfo.hStdError = hChildStdoutWr;
	startInfo.hStdInput = hChildStdinRd;
	cmdline = OcamlPath;
	// Let's go: start the ocaml interpreter
	processStarted = CreateProcess(NULL,cmdline,lpsa,lpsa,1,
		CREATE_NEW_PROCESS_GROUP|NORMAL_PRIORITY_CLASS,
		NULL,ProgramParams.CurrentWorkingDir,&startInfo,&pi);
	if (processStarted) {
		WaitForSingleObject(pi.hProcess,INFINITE);
		GetExitCodeProcess(pi.hProcess,(unsigned long *)&OcamlStatus);
		CloseHandle(pi.hProcess);
		PostMessage(hwndMain,WM_QUITOCAML,0,0);
	}
	else {
		char *msg = malloc(1024);
		wsprintf(msg,"Impossible to start ocaml.exe in:\n%s",cmdline);
		ShowDbgMsg(msg);
		free(msg);
	}
	return 0;
}

/*------------------------------------------------------------------------
 Procedure:     WriteToPipe ID:1
 Purpose:       Writes the given character string to the standard
                input of the interpreter
 Input:         The character string (zero terminated) to be written
 Output:        The number of characters written or zero if an error
                occurs
 Errors:        None
------------------------------------------------------------------------*/
int WriteToPipe(char *data)
{
	DWORD dwWritten;
	if (! WriteFile(hChildStdinWr, data, strlen(data),
		&dwWritten, NULL))
		return 0;
	return dwWritten;

}

/*------------------------------------------------------------------------
 Procedure:     ReadFromPipe ID:1
 Purpose:       Reads from the standard output of the interpreter and
                stores the data in the given buffer up to the given
                length. This is done in a non-blocking manner, i.e.
                it is safe to call this even if there is no data
                available.
 Input:         The buffer to be used and its length.
 Output:        Returns the number of characters read from the pipe.
 Errors:        None explicit
------------------------------------------------------------------------*/
int ReadFromPipe(char *data,int len)
{
	DWORD dwRead;

	PeekNamedPipe(hChildStdoutRd,data,len,NULL,&dwRead,NULL);
	if (dwRead == 0)
		return 0;

	// Read output from the child process, and write to parent's STDOUT.
	if( !ReadFile( hChildStdoutRd, data, len, &dwRead,
		NULL) || dwRead == 0)
		return 0;
	return dwRead;
}

static DWORD tid;
/*------------------------------------------------------------------------
 Procedure:     StartOcaml ID:1
 Purpose:       Starts the thread that will call the ocaml.exe
                program.
 Input:
 Output:
 Errors:
------------------------------------------------------------------------*/
int StartOcaml(void)
{
	getcwd(ProgramParams.CurrentWorkingDir,sizeof(ProgramParams.CurrentWorkingDir));
	CreateThread(NULL,0,DoStartOcaml,hwndMain,0,&tid);
	return 1;
}


void *SafeMalloc(int size)
{
	void *result;

	if (size < 0) {
		char message[1024];

error:
		sprintf(message,"Can't allocate %d bytes",size);
		MessageBox(NULL,message,"Ocaml",MB_OK);
		exit(-1);
	}
	result = malloc(size);
	if (result == NULL)
		goto error;
	return result;
}


void InterruptOcaml(void)
{
  if (! GenerateConsoleCtrlEvent(CTRL_BREAK_EVENT, pi.dwProcessId)) {
    char message[1024];
    sprintf(message, "GenerateConsole failed: %d\n", GetLastError());
    MessageBox(NULL, message, "Ocaml", MB_OK);
  }
  WriteToPipe(" ");
}
