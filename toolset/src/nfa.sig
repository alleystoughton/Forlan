(* nfa.sig
 *
 * COPYRIGHT (c) 2012 Alley Stoughton.
 *
 * extracted from nfa.mldoc
 *)

signature NFA =
  sig
    type nfa
    val injToFA : nfa -> FA.fa
    val injToEFA : nfa -> EFA.efa
    val valid : FA.fa -> bool
    val projFromFA : FA.fa -> nfa
    val projFromEFA : EFA.efa -> nfa
    val fromString : string -> nfa
    val input : string -> nfa
    val toPP : nfa -> PP.pp
    val toString : nfa -> string
    val output : string * nfa -> unit
    val states : nfa -> Sym.sym Set.set
    val startState : nfa -> Sym.sym
    val acceptingStates : nfa -> Sym.sym Set.set
    val transitions : nfa -> Tran.tran Set.set
    val compare : nfa Sort.total_ordering
    val equal : nfa * nfa -> bool
    val numStates : nfa -> int
    val numTransitions : nfa -> int
    val alphabet : nfa -> Sym.sym Set.set
    val sub : nfa * nfa -> bool
    val transitionFun : nfa -> Sym.sym * Str.str -> Sym.sym Set.set
    val transitionFunBackwards : nfa -> Sym.sym * Str.str -> Sym.sym Set.set
    val processStr : nfa -> Sym.sym Set.set * Str.str -> Sym.sym Set.set
    val accepted : nfa -> Str.str -> bool
    val reachableStates : nfa -> Sym.sym Set.set
    val liveStates : nfa -> Sym.sym Set.set
    val deadStates : nfa -> Sym.sym Set.set
    val renameStates : nfa * SymRel.sym_rel -> nfa
    val renameStatesCanonically : nfa -> nfa
    val isomorphism : nfa * nfa * SymRel.sym_rel -> bool
    val findIsomorphismOpt : nfa * nfa -> SymRel.sym_rel option
    val findIsomorphism : nfa * nfa -> SymRel.sym_rel
    val isomorphic : nfa * nfa -> bool
    val renameAlphabet : nfa * SymRel.sym_rel -> nfa
    val checkLP : nfa -> LP.lp -> unit
    val validLP : nfa -> LP.lp -> bool
    val findLPOpt : nfa
                      -> Sym.sym Set.set * Str.str * Sym.sym Set.set
                        -> LP.lp option
    val findLP : nfa -> Sym.sym Set.set * Str.str * Sym.sym Set.set -> LP.lp
    val findAcceptingLPOpt : nfa -> Str.str -> LP.lp option
    val findAcceptingLP : nfa -> Str.str -> LP.lp
    val emptyStr : nfa
    val emptySet : nfa
    val fromSym : Sym.sym -> nfa
    val simplify : nfa -> nfa
    val simplified : nfa -> bool
    val inter : nfa * nfa -> nfa
    val genInter : nfa list -> nfa
    val prefix : nfa -> nfa
    val fromEFA : EFA.efa -> nfa
  end
