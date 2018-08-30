(* fa.sig
 *
 * COPYRIGHT (c) 2012 Alley Stoughton.
 *
 * extracted from fa.mldoc
 *)

signature FA =
  sig
    type concr = {
                   stats : Sym.sym Set.set,
                   start : Sym.sym,
                   accepting : Sym.sym Set.set,
                   trans : Tran.tran Set.set
                 }
    type fa
    val valid : concr -> bool
    val fromConcr : concr -> fa
    val toConcr : fa -> concr
    val fromString : string -> fa
    val input : string -> fa
    val toPP : fa -> PP.pp
    val toString : fa -> string
    val output : string * fa -> unit
    val states : fa -> Sym.sym Set.set
    val startState : fa -> Sym.sym
    val acceptingStates : fa -> Sym.sym Set.set
    val transitions : fa -> Tran.tran Set.set
    val compare : fa Sort.total_ordering
    val equal : fa * fa -> bool
    val numStates : fa -> int
    val numTransitions : fa -> int
    val alphabet : fa -> Sym.sym Set.set
    val sub : fa * fa -> bool
    val transitionFun : fa -> Sym.sym * Str.str -> Sym.sym Set.set
    val transitionFunBackwards : fa -> Sym.sym * Str.str -> Sym.sym Set.set
    val processStr : fa -> Sym.sym Set.set * Str.str -> Sym.sym Set.set
    val accepted : fa -> Str.str -> bool
    val emptyClose : fa -> Sym.sym Set.set -> Sym.sym Set.set
    val emptyCloseBackwards : fa -> Sym.sym Set.set -> Sym.sym Set.set
    val reachify : fa -> fa
    val reachified : fa -> bool
    val renameStates : fa * SymRel.sym_rel -> fa
    val renameStatesCanonically : fa -> fa
    val isomorphism : fa * fa * SymRel.sym_rel -> bool
    val findIsomorphismOpt : fa * fa -> SymRel.sym_rel option
    val findIsomorphism : fa * fa -> SymRel.sym_rel
    val isomorphic : fa * fa -> bool
    val renameAlphabet : fa * SymRel.sym_rel -> fa
    val checkLP : fa -> LP.lp -> unit
    val validLP : fa -> LP.lp -> bool
    val findLPOpt : fa
                      -> Sym.sym Set.set * Str.str * Sym.sym Set.set
                        -> LP.lp option
    val findLP : fa -> Sym.sym Set.set * Str.str * Sym.sym Set.set -> LP.lp
    val findAcceptingLPOpt : fa -> Str.str -> LP.lp option
    val findAcceptingLP : fa -> Str.str -> LP.lp
    val emptyStr : fa
    val emptySet : fa
    val fromSym : Sym.sym -> fa
    val simplify : fa -> fa
    val simplified : fa -> bool
    val union : fa * fa -> fa
    val concat : fa * fa -> fa
    val closure : fa -> fa
    val genUnion : fa list -> fa
    val genConcat : fa list -> fa
    val rev : fa -> fa
    val fromStr : Str.str -> fa
    val fromReg : Reg.reg -> fa
    val jforlanNew : unit -> fa
    val jforlanEdit : fa -> fa
    val jforlanValidate : string -> unit
    val jforlanPretty : string -> unit
  end
