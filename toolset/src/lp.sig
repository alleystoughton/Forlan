(* lp.sig
 *
 * COPYRIGHT (c) 2012 Alley Stoughton.
 *
 * extracted from lp.mldoc
 *)

signature LP =
  sig
    datatype concr
      = Sym of Sym.sym
      | Cons of Sym.sym * Str.str * concr
    type lp
    val fromConcr : concr -> lp
    val toConcr : lp -> concr
    val fromString : string -> lp
    val input : string -> lp
    val toPP : lp -> PP.pp
    val toString : lp -> string
    val output : string * lp -> unit
    val compare : lp Sort.total_ordering
    val equal : lp * lp -> bool
    val sym : Sym.sym -> lp
    val cons : Sym.sym * Str.str * lp -> lp
    val startState : lp -> Sym.sym
    val endState : lp -> Sym.sym
    val label : lp -> Str.str
    val length : lp -> int
    val join : lp * lp -> lp
    val splitAt : lp * int -> lp * lp
    type pumping_division = lp * lp * lp
    val checkPumpingDivision : pumping_division -> unit
    val validPumpingDivision : pumping_division -> bool
    val strsOfValidPumpingDivision : pumping_division
                                       -> Str.str * Str.str * Str.str
    val pumpValidPumpingDivision : pumping_division * int -> lp
    val findValidPumpingDivision : lp -> pumping_division
    val findValidPumpingDivisionOpt : lp -> pumping_division option
  end
