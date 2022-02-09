JForlan: a Java Graphical Editor for Forlan Finite Automata and Trees
========================================================================

[`JForlan`](https://alleystoughton.us/forlan/jforlan/) is a Java
program for creating and editing Forlan automata and trees: finite
automata, regular expression finite automata, parse trees, regular
expression trees, and program trees.  JForlan automatically maintains
the connections between the components of automata and trees, as those
components are repositioned using the mouse.  And it handles the
conversion of diagrams from and to Forlan's concrete syntax.  JForlan
can be invoked directly (as a standalone applications) or from Forlan.

The subdirectory:

* [`specification`](specification) contains the specification of
    Forlan's syntax and interface with JForlan;

* [`src`](src) contains JForlan's source;

* [`lib`](lib) contains the JAR (Java Archive) files for the Apple Java
    Extensions (used to give JForlan a Mac look-and-feel) and the Mac
    Application Bundler (used to turn JForlan into a Mac application);

* [`doc`](doc) contains documentation for JForlan's source;

* [`icons`](icons) contains JForlan's icon in various formats;

* [`html`](html) contains JForlan's web pages and resources;

* [`mac`](mac) contains macOS-specific resources, and a script for
    building a compressed tarball in html;

* [`linux`](linux) contains Linux-specific resources, and a script for
    building a compressed tarball in html

* [`windows`](windows) contains Windows-specific resources, and a
    script for building a zip archive in html

* [`testing`](testing) contains a testing harness for JForlan

The script [`build-and-install`](build-and-install):

* builds JForlan from source, using [Apache
  Ant](https://ant.apache.org), which must be installed and on the
  shell's PATH

* builds the compressed tarballs for macOS and Linux, and the zip
  archive for Windows, puts these files in the html directory, and
  then installs the html directory in the Forlan html directory
  (`$html`)

Leonard Lee and Jessica Sherrill designed and implemented graphical
editors for Forlan finite automata (JFA), and regular expression and
parse trees (JTR), respectively.  Their work was unified and enhanced
(of particular note was the addition of support for program trees) by
Srinivasa Aditya Uppu, resulting in an initial version of JForlan.
Subsequently, Kenton Born carried out a major redevelopment of
JForlan, resulting in JForlan Version 1.0.  Further revisions, by
Alley Stoughton, led to JForlan Versions 2.0 and 2.1.
