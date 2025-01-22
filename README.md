My fork of GNU Emacs.  
Very much a work in progress.  
On github now only to show it to recruiters.

Some examples of code I written (most recent ones written in at least 90% by me):
* in C:
	* [src/xdisp.c -> function highlight_trailing_whitespace](src/xdisp.c#L23983)
	* [src/xdisp.c -> function display_and_set_cursor](src/xdisp.c#L33669)
	* [src/w32term.c -> function w32_draw_window_cursor](src/w32term.c#L6441)
* in Lisp:
	* [config/libs/myTab.el](config/libs/myTab.el) - subsystem managing and displaying tabs
	* [config/libs/myClipboard.el](config/libs/myClipboard.el) - clipboard subsystem
	* [config/libs/myInsert.el](config/libs/myInsert.el) - character insertion commands
	* [config/libs/myUtility.el](config/libs/myUtility.el) - common general shared code
	* [config/libs/myUndo.el](config/libs/myUndo.el) - single command for undo/redo

Building should be the same as normal GNU Emacs.  
For now a huge chunk of new functionality is in config/, so emacs.exe should be
invoked with "--init-directory=ROOT/config/" argument.
