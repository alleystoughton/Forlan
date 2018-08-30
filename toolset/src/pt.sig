(* pt.sig
 *
 * COPYRIGHT (c) 2012 Alley Stoughton.
 *
 * extracted from pt.mldoc
 *)

signature PT =
  sig
    datatype concr = Node of Sym.sym * concr list option
    type pt
    val fromConcr : concr -> pt
    val toConcr : pt -> concr
    val fromString : string -> pt
    val input : string -> pt
    val toPP : pt -> PP.pp
    val toString : pt -> string
    val output : string * pt -> unit
    val validPath : pt * int list -> bool
    val height : pt -> int
    val size : pt -> int
    val numLeaves : pt -> int
    val selectPT : pt * int list -> pt option
    val update : pt * int list * pt -> pt
    val maximumLengthPath : pt -> int list
    val validLeafPath : pt * int list -> bool
    val compare : pt Sort.total_ordering
    val equal : pt * pt -> bool
    val cons : Sym.sym * pt list option -> pt
    val leaf : Sym.sym -> pt
    val decons : pt -> Sym.sym * pt list option
    val rootLabel : pt -> Sym.sym
    val yield : pt -> Str.str
    type pumping_division = (pt * int list) * (pt * int list) * pt
    val checkPumpingDivision : pumping_division -> unit
    val validPumpingDivision : pumping_division -> bool
    val strsOfValidPumpingDivision : pumping_division
                                       -> Str.str
                                       * Str.str
                                       * Str.str
                                       * Str.str
                                       * Str.str
    val pumpValidPumpingDivision : pumping_division * int -> pt
    val findValidPumpingDivision : pt -> pumping_division
    val findValidPumpingDivisionOpt : pt -> pumping_division option
    val jforlanNew : unit -> pt
    val jforlanEdit : pt -> pt
    val jforlanValidate : string -> unit
    val jforlanPretty : string -> unit
  end
