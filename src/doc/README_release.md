Voss II
=======

Voss II is a software suite for describing and reasoning about circuits.
Circuits, and properties about them, are described in a functional language
called *fl*.

Fl is a statically typed language with call-by-need semantics (also known as
*lazy evaluation*) and binary decision diagrams built right into the language
itself.

Voss II has been tested and found to work on Debian, Ubuntu, Fedora, Red Hat
and OpenSUSE. If you're using it on another distribution, we'd love to hear
from you!


Usage
-----

After installing VossII, simply run `fl` to start the standalone interpreter
for VossII. You can also use our official editor integrations, to run VossII
from within either vim or emacs.

See [the VossII tutorial](https://teamvoss.github.io) for more information.


Installation
------------

There are two ways in which you can install our officially supported
binaries for VossII on your system.

### Rolling releases using git

Clone this repository, and add `$REPO_DIRECTORY/bin` to your `$PATH`.
You will also need to [install tk](#deps) before you can use VossII.

To update to the latest released version of VossII, simply run `git pull`
in `$REPO_DIRECTORY`.

### Tarball

Download and unpack the tarball from our
[release page](https://github.com/TeamVoss/VossII/releases/latest), then
add `$REPO_DIRECTORY/bin` to your `$PATH`.
You will also need to [install tk](#deps) before you can use VossII.


<span id="deps"></span>
### Dependencies

Voss II depends on Tk for its graphical bits. If the fl interpreter dies with
an angry message about not being able to find `wish`, you need to install it:

* **On Ubuntu/other Debian-based**
  ```shell
  sudo apt install tk
  ```
* **On Fedora/Red Hat**
  ```shell
  sudo yum install tk
  ```
* **On SUSE**
  ```shell
  sudo zypper install tk
  ```


Building
--------

Currently we officially support building VossII on Debian-based systems.
For build instructions, please se
[our source repository](https://github.com/TeamVoss/VossII/).
