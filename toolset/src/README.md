Forlan's Standard ML Source
=========================================================================

The file [`COPYING.txt`](COPYING.txt) says how Forlan may be copied
and used, according to the GNU General Public License, which is
included in the file [`GPL.txt`](GPL.txt).  The files
[`BLURB.txt`](BLURB.txt) and [`BLURB-mldoc.txt`](BLURB-mldoc.txt) are
intended to be included at the beginning of SML and ML-Doc source
files; they refer to `COPYING.txt`.

The main CM description file is [`forlan.cm`](forlan.cm).

There is almost a one-one-correspondece between `.mldoc` (ML-Doc
descriptions of signatures) and `.sig` (signatures) and `.sml` (structure)
files.

The `.mldoc` files are actually symbolic links to `../manual/ML-Doc`.

The `.sig` files should not be modified, they are created by the
[`mldoc-tool`](mldoc-tool) script using ML-Doc.  See `forlan.cm` for
how CM is told to use `ml-doc-tool` to turn `.mldoc` files into `.sig`
files.  (The files [`mldoc-ext.cm`](mldoc-ext.cm) and
[`ml-doc-tool.sml`](ml-doc-tool.sml) assist in this.)

The files [`export.sig`](export.sig) and [`export.sml`](export.sml)
are used to export a heap image.  They aren't documented in the Forlan
manual, since they won't be used by users.

The structure declared by [`top-level.sml`](top-level.sml) is opened
before the heap image is exported.

The file [`version.sml.template`](version.sml.template) is turned by
the [`change-version`](change-version) shell script into
[`version.sml`](version.sml).

The script cleanup can be used to delete the `.cm` directory.
