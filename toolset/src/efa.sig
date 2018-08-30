(* efa.sig
 *
 * COPYRIGHT (c) 2012 Alley Stoughton.
 *
 * extracted from efa.mldoc
 *)

signature EFA =
  sig
    type efa
    val injToFA : efa -> FA.fa
    val valid : FA.fa -> bool
    val projFromFA : FA.fa -> efa
    val fromString : string -> efa
    val input : string -> efa
    val toPP : efa -> PP.pp
    val toString : efa -> string
    val output : string * efa -> unit
    val states : efa -> Sym.sym Set.set
    val startState : efa -> Sym.sym
    val acceptingStates : efa -> Sym.sym Set.set
    val transitions : efa -> Tran.tran Set.set
    val compare : efa Sort.total_ordering
    val equal : efa * efa -> bool
    val numStates : efa -> int
    val numTransitions : efa -> int
    val alphabet : efa -> Sym.sym Set.set
    val sub : efa * efa -> bool
    val transitionFun : efa -> Sym.sym * Str.str -> Sym.sym Set.set
    val transitionFunBackwards : efa -> Sym.sym * Str.str -> Sym.sym Set.set
    val processStr : efa -> Sym.sym Set.set * Str.str -> Sym.sym Set.set
    val accepted : efa -> Str.str -> bool
    val emptyClose : efa -> Sym.sym Set.set -> Sym.sym Set.set
    val emptyCloseBackwards : efa -> Sym.sym Set.set -> Sym.sym Set.set
    val reachify : efa -> efa
    val reachified : efa -> bool
    val renameStates : efa * SymRel.sym_rel -> efa
    val renameStatesCanonically : efa -> efa
    val isomorphism : efa * efa * SymRel.sym_rel -> bool
    val findIsomorphismOpt : efa * efa -> SymRel.sym_rel option
    val findIsomorphism : efa * efa -> SymRel.sym_rel
    val isomorphic : efa * efa -> bool
    val renameAlphabet : efa * SymRel.sym_rel -> efa
    val checkLP : efa -> LP.lp -> unit
    val validLP : efa -> LP.lp -> bool
    val findLPOpt : efa
                      -> Sym.sym Set.set * Str.str * Sym.sym Set.set
                        -> LP.lp option
    val findLP : efa -> Sym.sym Set.set * Str.str * Sym.sym Set.set -> LP.lp
    val findAcceptingLPOpt : efa -> Str.str -> LP.lp option
    val findAcceptingLP : efa -> Str.str -> LP.lp
    val emptyStr : efa
    val emptySet : efa
    val fromSym : Sym.sym -> efa
    val simplify : efa -> efa
    val simplified : efa -> bool
    val union : efa * efa -> efa
    val concat : efa * efa -> efa
    val closure : efa -> efa
    val genUnion : efa list -> efa
    val genConcat : efa list -> efa
    val rev : efa -> efa
    val inter : efa * efa -> efa
    val genInter : efa list -> efa
    val prefix : efa -> efa
    val fromFA : FA.fa -> efa
  end
