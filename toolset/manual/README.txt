                  ML-Doc Source for Forlan's Manual

Forlan's manual is in ML-Doc format.  ML-Doc may be obtained
from http://people.cs.uchicago.edu/~jhr/ (look in the Software
category).

Make sure that the pathnames in CATALOG and Config.cfg are correct.

The subdirectory ...

  ML-Doc

    contains the manual source, in .mldoc files,

  HTML

    is where generated HTML is placed (index.html is the root),

  Hardcopy and Proof

    are where generated LaTeX is placed (in final and proof versions),
    but we're not using this

  Info

    is where ML-Doc caches internal information,

  Sigs

    is where generated Standard ML signatures are placed (but they
    are automatically moved by ../src/ml-doc-tool to ../src).

If you add/delete .mldoc files to/from ML-Doc, you must run the
make-makefile script.  Occurrences of SIGBODY must name the SGML
entity (see Entities.sgml) that names the .sig file that should be
generated from it.

The .template files can be modified to change the layout of pages, the
table of contents (toc) and the index.  To change the Forlan version
number in these files, run the script

  change-version Maj.Min

(e.g., change-version 2.3).  To change the date of the copyright
notices in these files, run the script

  change-copyright DDDD

(e.g., change-copyright 2005).

The script cleanup can be used to delete the automatically generated
files in the subdirectories.
