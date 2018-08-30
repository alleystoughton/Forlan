(********************************** nfa.sml **********************************)

(* Copyright (C) 2001-2012 Alley Stoughton

   The file is part of the Forlan toolset for experimenting with
   formal languages.  See the file COPYING.txt for copying and
   usage restrictions. *)

structure NFA :> NFA =
struct

structure M  = Messages
structure L  = Lex

(********************************* Main Type *********************************)

type nfa = FA.fa

fun injToFA(nfa : nfa) : FA.fa = nfa

fun injToEFA nfa = EFA.projFromFA(injToFA nfa)

fun checkTran(_, x, _) =
      if length x <> 1
      then M.errorPP
           (fn () =>
                 [PP.fromString "invalid", PP.fromString "label",
                  PP.fromString "in", PP.fromString "transition:",
                  PP.quote(Str.toPP x)])
      else ()

fun checkTransitions nil       = ()
  | checkTransitions (t :: ts) = (checkTran t; checkTransitions ts)

fun check fa = checkTransitions(Set.toList(FA.transitions fa))

fun valid fa =
      (M.quiet(fn () => check fa); true)
        handle _ => false

fun projFromFA(fa : FA.fa) :  nfa = (check fa; fa)

val projFromEFA = projFromFA o EFA.injToFA

val fromString = projFromFA o FA.fromString

val input = projFromFA o FA.input

(************************ Functions Inherited from FA ************************)

val toPP = FA.toPP

val toString = FA.toString

val output = FA.output

val states = FA.states

val startState = FA.startState

val acceptingStates = FA.acceptingStates

val transitions = FA.transitions

val compare = FA.compare

val equal = FA.equal

val numStates = FA.numStates

val numTransitions = FA.numTransitions

val alphabet = FA.alphabet

val sub = FA.sub

val transitionFun = FA.transitionFun

val transitionFunBackwards = FA.transitionFunBackwards

val processStr = FA.processStr

val accepted = FA.accepted

val reachify = FA.reachify

val reachified = FA.reachified

val renameStates = FA.renameStates

val renameStatesCanonically = FA.renameStatesCanonically

val isomorphism = FA.isomorphism

val findIsomorphismOpt = FA.findIsomorphismOpt

val findIsomorphism = FA.findIsomorphism

val isomorphic = FA.isomorphic

val renameAlphabet = FA.renameAlphabet

val checkLP = FA.checkLP

val validLP = FA.validLP

val findLPOpt = FA.findLPOpt

val findLP = FA.findLP

val findAcceptingLPOpt = FA.findAcceptingLPOpt

val findAcceptingLP = FA.findAcceptingLP

val emptyStr = FA.emptyStr

val emptySet = FA.emptySet

val fromSym = FA.fromSym

val simplify = FA.simplify

val simplified = FA.simplified

(*********************** Functions Inherited From EFA ***********************)

fun inter(nfa1, nfa2) = projFromEFA(EFA.inter(injToEFA nfa1, injToEFA nfa2))

fun genInter nfas = projFromEFA(EFA.genInter(map injToEFA nfas))

fun prefix nfa = projFromEFA(EFA.prefix(injToEFA nfa))

(****************************** Other Functions ******************************)

fun fromEFA efa =
      let val fa        = EFA.injToFA efa
          val stats     = FA.states fa
          val start     = FA.startState fa
          val accepting = FA.acceptingStates fa
          val trans     = FA.transitions fa

          fun transitionsOf(q, x, r) =
                if null x
                then Set.empty
                else Set.times3(FA.emptyCloseBackwards fa (Set.sing q),
                                Set.sing x,
                                FA.emptyClose fa (Set.sing r))

          val stats'     = stats
          val start'     = start
          val accepting' = FA.emptyCloseBackwards fa accepting
          val trans'     = TranSet.genUnion(Set.mapToList transitionsOf trans)

          val concr =
                {stats     = stats',
                 start     = start',
                 accepting = accepting',
                 trans     = trans'}
      in FA.fromConcr concr end

end;
