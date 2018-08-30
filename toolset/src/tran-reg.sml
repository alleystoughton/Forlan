(******************************* tran-reg.sml ********************************)

(* Copyright (C) 2012 Alley Stoughton

   The file is part of the Forlan toolset for experimenting with
   formal languages.  See the file COPYING.txt for copying and
   usage restrictions. *)

structure TranReg :> TRAN_REG =
struct

(*********************************** Type ************************************)

type tran_reg = Sym.sym * Reg.reg * Sym.sym

(********************************* Functions *********************************)

val compare = Set.compareTriple(Sym.compare, Reg.compare, Sym.compare)

fun equal(tran1, tran2) = compare(tran1, tran2) = EQUAL

end;
