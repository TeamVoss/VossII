# fl extension for Visual Studio Code

## Installation

**Method 1 :** download the vossii-fl-X.Y.Z.vsix file and run

	code --install-extension ./path/to/vossii-fl-X.Y.Z.vsix

> replace `code` with `codium` for some unofficial builds

**Method 2 :** download the vossii-fl folder and copy/symlink it into your `.vscode/extensions/` folder:

* on linux/mac it is at `~/.vscode/extensions`
* on windows it is at `%USERPROFILE%\.vscode\extensions`

> path may differ for some unofficial builds, for me it was `~/.vscode-oss/extensions/`

## Features

See the extension's [README](./vossii-fl/README.md) for a detailed list of features. These include :

- syntax coloring
- command to send code to fl
- help tooltip when hovering over functions/operators
- go to definition
