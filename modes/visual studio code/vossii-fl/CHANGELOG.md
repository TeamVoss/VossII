# Change Log

All notable changes to the "vossii-fl" extension will be documented in this file.

## Version 1.2.0 - 2021-07-18

- Added support for Jeremy's language
- Added hexadecimal and binary constant coloring
- Fix typo and missing keywords
- Fix some wrong syntax colors when no spaces used
- Used `set +o history` to avoid spamming bash history with terminal commands
- fl color to `new_type_abbrev` constructs

## Version 1.1.0 - 2021-04-21

- New command `fl.eval_paragraph` bound to `f9`
- Added special color/tooltip for `condition => if_true | if_false` construct.
- Added `defix` to keywords
- Fixed number constants not coloring on line start.
- Fixed no syntax coloring for `{expr::type}` when `expr` contained multiple words
- Removed special character coloring in comments, added coloration for TODO and FIXME
- Fixed wrong escaped quotes detection leading to no hover for identifier and hover for strings
- Added an option to save files before running unsaved files.

## Version 1.0.0 - 2021-03-02

- Initial release
- Added syntax coloring for keywords, operators, builtin commands and builtin typs
- Added syntax coloring for simple constructs: let/letrec, lettypes, lambda functions, vals, explicit type specification, strings, comments
- Added commands start, stop, help, run file, run line, run selection, restart and run file
- Added shortcut to commands and placed relevant ones in the context menu
- Added tooltips
- Added go to definition
