# Installation Instructions

Right now, just do `make`. This will compile `jit.cpp`, a series of functions
that can be accessed from Lisp and act as Hylas' backend, then compile the basic console frontend.

`make system` will symlink the folder where Hylas resides to your `quicklisp/local-projects` folder, so Hylas can be Quickloaded as a library.

`make gui` builds the (Experimental, not production ready) Syntagma IDE.
