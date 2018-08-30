(* dfa.sig
 *
 * COPYRIGHT (c) 2012 Alley Stoughton.
 *
 * extracted from dfa.mldoc
 *)

signature DFA =
  sig
    type dfa
    val injToFA : dfa -> FA.fa
    val injToEFA : dfa -> EFA.efa
    val injToNFA : dfa -> NFA.nfa
    val valid : FA.fa -> bool
    val projFromFA : FA.fa -> dfa
    val projFromEFA : EFA.efa -> dfa
    val projFromNFA : NFA.nfa -> dfa
    val fromString : string -> dfa
    val input : string -> dfa
    val toPP : dfa -> PP.pp
    val toString : dfa -> string
    val output : string * dfa -> unit
    val states : dfa -> Sym.sym Set.set
    val startState : dfa -> Sym.sym
    val acceptingStates : dfa -> Sym.sym Set.set
    val transitions : dfa -> Tran.tran Set.set
    val compare : dfa Sort.total_ordering
    val equal : dfa * dfa -> bool
    val numStates : dfa -> int
    val numTransitions : dfa -> int
    val alphabet : dfa -> Sym.sym Set.set
    val sub : dfa * dfa -> bool
    val transitionFun : dfa -> Sym.sym * Str.str -> Sym.sym Set.set
    val transitionFunBackwards : dfa -> Sym.sym * Str.str -> Sym.sym Set.set
    val processStr : dfa -> Sym.sym Set.set * Str.str -> Sym.sym Set.set
    val accepted : dfa -> Str.str -> bool
    val reachify : dfa -> dfa
    val reachified : dfa -> bool
    val renameStates : dfa * SymRel.sym_rel -> dfa
    val renameStatesCanonically : dfa -> dfa
    val isomorphism : dfa * dfa * SymRel.sym_rel -> bool
    val findIsomorphismOpt : dfa * dfa -> SymRel.sym_rel option
    val findIsomorphism : dfa * dfa -> SymRel.sym_rel
    val isomorphic : dfa * dfa -> bool
    val renameAlphabet : dfa * SymRel.sym_rel -> dfa
    val checkLP : dfa -> LP.lp -> unit
    val validLP : dfa -> LP.lp -> bool
    val findLPOpt : dfa
                      -> Sym.sym Set.set * Str.str * Sym.sym Set.set
                        -> LP.lp option
    val findLP : dfa -> Sym.sym Set.set * Str.str * Sym.sym Set.set -> LP.lp
    val findAcceptingLPOpt : dfa -> Str.str -> LP.lp option
    val findAcceptingLP : dfa -> Str.str -> LP.lp
    val emptyStr : dfa
    val emptySet : dfa
    val inter : dfa * dfa -> dfa
    val genInter : dfa list -> dfa
    val determTransitionFun : dfa -> Sym.sym * Sym.sym -> Sym.sym
    val determProcessStr : dfa -> Sym.sym * Str.str -> Sym.sym
    val determAccepted : dfa -> Str.str -> bool
    val fromNFA : NFA.nfa -> dfa
    val determSimplified : dfa -> bool
    val determSimplify : dfa * Sym.sym Set.set -> dfa
    val minimize : dfa -> dfa
    val complement : dfa * Sym.sym Set.set -> dfa
    val minus : dfa * dfa -> dfa
    datatype relationship
      = Equal
      | ProperSub of Str.str
      | ProperSup of Str.str
      | Incomp of Str.str * Str.str
    val relation : dfa * dfa -> relationship
    val relationship : dfa * dfa -> unit
    val subset : dfa * dfa -> bool
    val equivalent : dfa * dfa -> bool
  end
