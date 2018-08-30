                          The Forlan Project

The Forlan Project consists of

  a toolset (called Forlan) for experimenting with formal languages,

  JForlan, a Java graphical editor for Forlan automata and trees, and

  a draft textbook entitled "Formal Language Theory: Integrating
  Experimentation and Proof",

all released under free software/documentation licenses.

The subdirectory ...

  html

    contains the Forlan WWW page (read this first).

  toolset

    contains the Standard ML source for the toolset, plus
    the ML-Doc source for the toolset's manual.

  jforlan

    contains the distribution of JForlan.

  book

    contains the LaTeX source for the book.

The file TODO.txt is a list of planned revisions/improvements.

The file version.txt contains the current version of the Forlan Project.
It is read by various scripts.

The file html-directory.txt contains the pathname of the Forlan
HTML directory.  It is read by various scripts.

The script build-and-install-tarball builds a compressed tarball of
the Forlan distribution, and installs it in the HTML directory.

The script build-and-install runs build-and-install-tarball and builds
and installs in the Forlan HTML directory:

  * the general WWW pages (see html/install),

  * the manual (see toolset/install-manual),

  * the manual archives (see toolset/build-and-install-manual-archives),

  * the minimum source archives (see
    toolset/build-and-install-min-src-archives),

  * the JForlan WWW pages (see jforlan/build-and-install -- requires
    that Apache Ant be installed and on the shell's PATH).
