Forlan Project
==========================================================================

The [Forlan Project](https://alleystoughton.us/forlan) consists of

* a toolset (called Forlan) for experimenting with formal languages,

* [JForlan](https://alleystoughton.us/forlan/jforlan/), a Java
  graphical editor for Forlan automata and trees, and

* a draft textbook entitled ["Formal Language Theory: Integrating
  Experimentation and
  Proof"](https://alleystoughton.us/forlan/book.pdf),

all released under free software/documentation licenses.

The subdirectory:

* [`html`](html) contains the source for the [Forlan
  website](https://alleystoughton.us/forlan/)

* [`toolset`](toolset) contains the Standard ML source for the
  toolset, plus the [ML-Doc](https://people.cs.uchicago.edu/~jhr/)
  source for the toolset's manual

* [`jforlan`](jforlan) contains the distribution of JForlan

* [`book`](book) contains the LaTeX source for the book.

The file [`version.txt`](version.txt) contains the current version of
the Forlan Project.  It is read by various scripts.

The file [`html-directory.txt`](html-directory.txt) contains the
pathname of the Forlan HTML directory.  It is read by various scripts.

The script [`build-and-install-tarball`](build-and-install-tarball)
builds a compressed tarball of the Forlan distribution, and installs
it in the HTML directory.

The script [`build-and-install`](build-and-install) runs
[`build-and-install-tarball`](build-and-install-tarball) and builds
and installs in the Forlan HTML directory:

* the general WWW pages (see [`html/install`](html/install)),

* the manual (see [`toolset/install-manual`](toolset/install-manual)),

* the manual archives (see
  [`toolset/build-and-install-manual-archives`](toolset/build-and-install-manual-archives)),

* the minimum source archives (see
  [`toolset/build-and-install-min-src-archives`](toolset/build-and-install-min-src-archives)),

* the JForlan WWW pages (see
  [`jforlan/build-and-install`](jforlan/build-and-install); requires
  that [Apache Ant](https://ant.apache.org) be installed and on the
  shell's PATH.
