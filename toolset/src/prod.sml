(********************************** prod.sml **********************************)

(* Copyright (C) 2001-2012 Alley Stoughton

   The file is part of the Forlan toolset for experimenting with
   formal languages.  See the file COPYING.txt for copying and
   usage restrictions. *)

structure Prod :> PROD =
struct

(*********************************** Type ************************************)

type prod = Sym.sym * Str.str

(********************************* Functions *********************************)

val compare = Set.comparePair(Sym.compare, Str.compare)

fun equal(prod1, prod2) = compare(prod1, prod2) = EQUAL

end;
