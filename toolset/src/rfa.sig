(* rfa.sig
 *
 * COPYRIGHT (c) 2012 Alley Stoughton.
 *
 * extracted from rfa.mldoc
 *)

signature RFA =
  sig
    type concr = {
                   stats : Sym.sym Set.set,
                   start : Sym.sym,
                   accepting : Sym.sym Set.set,
                   trans : TranReg.tran_reg Set.set
                 }
    type rfa
    val valid : concr -> bool
    val fromConcr : concr -> rfa
    val toConcr : rfa -> concr
    val fromString : string -> rfa
    val input : string -> rfa
    val toPP : rfa -> PP.pp
    val toString : rfa -> string
    val output : string * rfa -> unit
    val states : rfa -> Sym.sym Set.set
    val startState : rfa -> Sym.sym
    val acceptingStates : rfa -> Sym.sym Set.set
    val transitions : rfa -> TranReg.tran_reg Set.set
    val compare : rfa Sort.total_ordering
    val equal : rfa * rfa -> bool
    val numStates : rfa -> int
    val numTransitions : rfa -> int
    val alphabet : rfa -> Sym.sym Set.set
    val sub : rfa * rfa -> bool
    val renameStates : rfa * SymRel.sym_rel -> rfa
    val renameStatesCanonically : rfa -> rfa
    val checkLP : (Str.str * Reg.reg -> bool) * rfa -> LP.lp -> unit
    val validLP : (Str.str * Reg.reg -> bool) * rfa -> LP.lp -> bool
    val standard : rfa -> bool
    val standardize : rfa -> rfa
    val fromFA : (Reg.reg -> Reg.reg) -> FA.fa -> rfa
    val eliminateState : (Reg.reg -> Reg.reg) -> rfa * Sym.sym -> rfa
    val toReg : (Reg.reg -> Reg.reg) -> rfa -> Reg.reg
    val faToReg : (Reg.reg -> Reg.reg) -> FA.fa -> Reg.reg
    val faToRegPerms : int option * (Reg.reg -> Reg.reg) -> FA.fa -> Reg.reg
    val faToRegPermsTrace : int option * (Reg.reg -> Reg.reg)
                              -> FA.fa -> Reg.reg
    val jforlanNew : unit -> rfa
    val jforlanEdit : rfa -> rfa
    val jforlanValidate : string -> unit
    val jforlanPretty : string -> unit
  end
