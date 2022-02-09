Forlan Toolset
======================================================================

The subdirectory:

* [`src`](src) contains Forlan's source,

* [`manual`](manual) contains Forlan's manual in ML-Doc format,

* [`scripts`](scripts) contains several shell scripts.

The script:

* [`build-heap-image`](build-heap-image) builds an SML/NJ heap image
    for Forlan, which can be invoked by
    [`scripts/forlan`](scripts/forlan) (on Linux/macOS) and
    [`scripts/forlan.bat`](scripts/forlan.bat) (on Windows); installs
    heap image in /tmp.

* [`build-and-install-min-src-archives`](build-and-install-min-src-archives)
    builds .zip and .tgz archives with just the source needed to build
    Forlan, along with the shell scripts needed to build an SML/NJ
    heap image for Forlan from this source, and to run it; installs
    archives in an HTML directory.

* [`install-manual`](install-manual) builds and installs Forlan's
    manual in an HTML directory.

* [`build-and-install-manual-archives`](build-and-install-manual-archives)
    builds .zip and .tgz archives of the Forlan manual in HTML format,
    and installs them in an HTML directory.

Before running any of these scripts, make sure that
[ML-Doc](https://people.cs.uchicago.edu/~jhr/tools/ml-doc.html) is
installed and properly configured; see [`manual`](manual).

The script [`cleanup`](cleanup) can be used to delete the
automatically generated files in the subdirectories.
