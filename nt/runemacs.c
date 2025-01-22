// Simple program to start Emacs with its console window hidden.
//
// This program is provided purely for convenience, since most users will
// use Emacs in windowing (GUI) mode, and will not want to have an extra
// console window lying around.
// Well, not quite, because emacs can't be linked as a Windows gui program
// (or at least it would be hard while still having option to launch it in
// text mode).

#define DEFER_MS_W32_H
#include <config.h>
#include <windows.h>
#include <string.h>
#include <malloc.h>

int WINAPI WinMain(HINSTANCE hSelf, HINSTANCE hPrev, LPSTR cmdline, int nShow) {
	char *new_cmdline, *p;
	static char const iconic_opt[] = "--iconic ", maximized_opt[] = "--maximized ";

	{
		char modname[MAX_PATH];

		if (
			!GetModuleFileName(NULL, modname, MAX_PATH)
			|| (p = strrchr(modname, '\\')) == NULL
		)
			goto error;
		*p = 0;

		new_cmdline =
			alloca(
				MAX_PATH
				+ strlen(cmdline)
				+ (
					(nShow == SW_SHOWMINNOACTIVE || nShow == SW_SHOWMAXIMIZED)
					? max(sizeof (iconic_opt), sizeof (maximized_opt))
					: 0
				)
				+ 3
			);
		// Quote executable name in case of spaces in the path.
		*new_cmdline = '"';
		strcpy(new_cmdline + 1, modname);
	}

	// Detect and handle un-installed runemacs.exe in nt/ subdirectory,
	// while emacs.exe is in src/.
	if ((p = strrchr(new_cmdline, '\\')) != NULL && stricmp(p, "\\nt") == 0)
		strcpy(p, "\\src");
	strcat(new_cmdline, "\\emacs.exe\" ");

	int wait_for_child = FALSE;
	DWORD priority_class = NORMAL_PRIORITY_CLASS;
	// Append original arguments if any; first look for arguments we
	// recognize (-wait, -high, and -low), and apply them ourselves.
	while (cmdline[0] == '-' || cmdline[0] == '/') {
		if (strncmp(cmdline+1, "wait", 4) == 0) {
			wait_for_child = TRUE;
			cmdline += 5;
		} else if (strncmp(cmdline+1, "high", 4) == 0) {
			priority_class = HIGH_PRIORITY_CLASS;
			cmdline += 5;
		} else if (strncmp(cmdline+1, "low", 3) == 0) {
			priority_class = IDLE_PRIORITY_CLASS;
			cmdline += 4;
		} else
			break;
		// Look for next argument.
		while (*++cmdline == ' ');
	}

	// If the desktop shortcut properties tell to invoke runemacs
	// minimized, or if they invoked runemacs via "start /min", pass
	// '--iconic' to Emacs, as that's what users will expect.
	// Likewise with invoking runemacs maximized: pass '--maximized' to Emacs.
	if (nShow == SW_SHOWMINNOACTIVE)
		strcat(new_cmdline, iconic_opt);
	else if (nShow == SW_SHOWMAXIMIZED)
		strcat(new_cmdline, maximized_opt);
	strcat (new_cmdline, cmdline);

	STARTUPINFO start;
	memset(&start, 0, sizeof (start));
	start.cb = sizeof (start);
	start.dwFlags = STARTF_USESHOWWINDOW | STARTF_USECOUNTCHARS;
	start.wShowWindow = SW_HIDE;
	// Ensure that we don't waste memory if the user has specified a huge
	// default screen buffer for command windows.
	start.dwXCountChars = 80;
	start.dwYCountChars = 25;

	SECURITY_ATTRIBUTES sec_attrs;
	sec_attrs.nLength = sizeof (sec_attrs);
	sec_attrs.lpSecurityDescriptor = NULL;
	sec_attrs.bInheritHandle = FALSE;

	PROCESS_INFORMATION child;
	DWORD ret_code = 0;
	if (
		CreateProcessA(
			NULL, new_cmdline, &sec_attrs, NULL, TRUE, priority_class, NULL, NULL,
			&start, &child
		)
	) {
		if (wait_for_child) {
			WaitForSingleObject (child.hProcess, INFINITE);
			GetExitCodeProcess (child.hProcess, &ret_code);
		}
		CloseHandle(child.hThread);
		CloseHandle(child.hProcess);
		return (int) ret_code;
	}

	error:
	MessageBox(NULL, "Could not start Emacs.", "Error", MB_ICONSTOP);
	return 1;
}
