(********************************** tran.sml **********************************)

(* Copyright (C) 2012 Alley Stoughton

   The file is part of the Forlan toolset for experimenting with
   formal languages.  See the file COPYING.txt for copying and
   usage restrictions. *)

structure Tran :> TRAN =
struct

(*********************************** Type ************************************)

type tran = Sym.sym * Str.str * Sym.sym

(********************************* Functions *********************************)

val compare = Set.compareTriple(Sym.compare, Str.compare, Sym.compare)

fun equal(tran1, tran2) = compare(tran1, tran2) = EQUAL

end;
