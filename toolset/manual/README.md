ML-Doc Source for Forlan's Manual
=======================================================================

Forlan's manual is in
[ML-Doc](https://people.cs.uchicago.edu/~jhr/tools/ml-doc.html)
format.

Make sure that the pathnames in [`CATALOG`](CATALOG) and
[`Config.cfg`](Config.cfg) are correct.

The subdirectory:

* [`ML-Doc`](ML-Doc) contains the manual source, in .mldoc files;

* [`HTML`](HTML) is where generated HTML is placed (index.html is the
  root);

* [`Hardcopy`](Hardcopy) and [`Proof`](Proof) are where generated
    LaTeX is placed (in final and proof versions), but we're not using
    this;

* [`Info`](Info) is where ML-Doc caches internal information;

* [`Sigs`](Sigs) is where generated Standard ML signatures are placed
    (but they are automatically moved by `../src/ml-doc-tool` to
    `../src`).

If you add/delete `.mldoc` files to/from `ML-Doc`, you must run the
[`make-makefile`](make-makefile) script.  Occurrences of `SIGBODY`
must name the SGML entity (see `Entities.sgml`) that names the `.sig`
file that should be generated from it.

The `.template` files can be modified to change the layout of pages, the
table of contents (toc) and the index.  To change the Forlan version
number in these files, run the script

```
  change-version Maj.Min
```

(e.g., `change-version 2.3`).  To change the date of the copyright
notices in these files, run the script

```
  change-copyright DDDD
```

(e.g., `change-copyright 2005`).

The script `cleanup` can be used to delete the automatically generated
files in the subdirectories.
