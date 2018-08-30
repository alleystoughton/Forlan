                          The Forlan Toolset

The subdirectory ...

  src

    contains Forlan's source,

  manual

    contains Forlan's manual in ML-Doc format,

  scripts

    contains several shell scripts, and

The script ...

  build-heap-image

    builds an SML/NJ heap image for Forlan, which can be invoked by
    scripts/forlan (on Linux/Mac OS X) and scripts/forlan.bat (on
    Windows)

    installs heap image in /tmp

  build-and-install-min-src-archives

    builds .zip and .tgz archives with just the source needed to build
    Forlan, along with the shell scripts needed to build an SML/NJ
    heap image for Forlan from this source, and to run it (see
    html/index.html for instructions on how to use the archives)

    installs archives in an HTML directory

  install-manual

    builds and installs Forlan's manual in an HTML directory,

  build-and-install-manual-archives

    builds .zip and .tgz archives of the Forlan manual in HTML format,
    and installs them in an HTML directory

Before running any of these scripts, make sure that ML-Doc
(available from http://people.cs.uchicago.edu/~jhr/) is installed
and properly configured (see manual/README.txt).

The script cleanup can be used to delete the automatically generated
files in the subdirectories.
