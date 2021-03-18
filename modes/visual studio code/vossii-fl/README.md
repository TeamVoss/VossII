# Language support for fl

This extension adds language support for [Voss II](https://github.com/TeamVoss/VossII)'s functional language fl to Visual Studio Code.

## Features

Features include:

- Basic syntaxic highlighting (tested in Dark+ and Light+ themes)
- Commands to send files/lines/selections to an fl interpreter with
	predefined shortcuts
- Tooltips generated from fl's help texts
- Go to definition when pressing `f12` while having the cursor over
	a function name/operator

> Note that appart from syntax highlighting, feature require to have
	an fl interpreter running and symbols defined to work properly.
	Running your current file frequently is advised


Commands and shortcuts:

|Command|Effect|Shortcut|
|-------|------|--------|
|fl: start fl|Starts the interpreter|Automaticaly when opening fl files|
|fl: stop fl|Stops the interpreter| |
|fl: help|Displays help on the word/operator under the cursor in the fl interpreter| |
|fl: restart and run file|Restarts the interpreter and runs current file|`f5`|
|fl: run file|Runs the current file in the interpreter|`f6`|
|fl: run line|Runs the current line in the interpreter|`f7`|
|fl: run selection|Runs the current selection (or word under cursor) in the interpreter|`f8`|

> Commands can be run by typing their name in the command palette (`Ctrl+Shift+P`).

> Shortcuts can be reassigned in `File > Preferences > Keyboard Shortcuts`

## Requirements

The syntax highlighting has no requirements.
All other functionnalities require to have
[Voss II](https://github.com/TeamVoss/VossII) installed and
the path to the fl interpreter specified in settings.

## Extension Settings

This extension contributes the following settings:

* `vossii-fl.path`: path to the fl interpreter (default `"fl"`)
* `vossii-fl.run_on_startup`: Automaticaly run `fl` when opening a
	file/workspace with fl files (default on)
* `vossii-fl.fl_setup`: fl code to run whenever starting fl
	(default `set_font font_larger;`)
* `vossii-fl.temporary_files_root`: path and prefix of temporary files used to
	communicate with fl (default `/tmp/fl_`)

## Known Issues

* When running a selection/line, `DIR` will be the workspace folder and not
	the file root as expected (this isn't a problem when running files)
* Getting tooltips/definition may not work on 1rst try.
	If so, move mouse out and back over or hit `f12` again.
* Can only get tooltips/go to definition for global function, both fail
	for types or local variables.
