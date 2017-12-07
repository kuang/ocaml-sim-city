README DOCUMENTATION

(This documentation assumes a working installation of Ocaml version 4.05.)

First of all, there are two external dependencies needed for this project:
lablgtk2 and Async. For testing purposes, OUnit2 is also required.

Brew and Opam can be easily used to install both libaries on macOS:

For lablgtk2:
$ brew install gtk+
$ export PKG_CONFIG_PATH=/opt/X11/lib/pkgconfig
$ opam install lablgtk

For Async:
$ opam install async

After installation, the game itself can be run by calling "make run" from the
terminal when you are in the game directory.
