(********************************** efa.sml **********************************)

(* Copyright (C) 2001-2012 Alley Stoughton

   The file is part of the Forlan toolset for experimenting with
   formal languages.  See the file COPYING.txt for copying and
   usage restrictions. *)

structure EFA :> EFA =
struct

structure M  = Messages
structure L  = Lex

(********************************* Main Type *********************************)

type efa = FA.fa

fun injToFA(efa : efa) : FA.fa = efa

fun checkTran(_, x, _) =
      if length x > 1
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

fun projFromFA(fa : FA.fa) : efa = (check fa; fa)

val fromString = projFromFA o FA.fromString

val input = projFromFA o FA.input

(************************ Functions Inherited From FA ************************)

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

val emptyClose = FA.emptyClose

val emptyCloseBackwards = FA.emptyCloseBackwards

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

val union = FA.union

val concat = FA.concat

val closure = FA.closure

val genUnion = FA.genUnion

val genConcat = FA.genConcat

val rev = FA.rev

(******************** Functions Inherited by DFA and NFA ********************)

fun inter(efa1, efa2) =
      let val stats1     = FA.states efa1
          val start1     = FA.startState efa1
          val accepting1 = FA.acceptingStates efa1
          val tranFun1   = FA.transitionFun efa1

          val stats2     = FA.states efa2
          val start2     = FA.startState efa2
          val accepting2 = FA.acceptingStates efa2
          val tranFun2   = FA.transitionFun efa2

          val alphabet =
                SymSet.inter(FA.alphabet efa1, FA.alphabet efa2)

          fun symPairToSym(q, r) =
                Sym.fromTop(Sym.Compound[SOME q, NONE, SOME r])

          fun nextEmpty(q, r) =
                SymRel.union(Set.times(Set.sing q, tranFun2(r, nil)),
                             Set.times(tranFun1(q, nil), Set.sing r))

          fun nextSym((q, r), a) =
                Set.times(tranFun1(q, [a]), tranFun2(r, [a]))

          fun next pair =
                Set.toList(nextEmpty pair) @
                List.concat(Set.mapToList (fn a =>
                                                Set.toList(nextSym(pair, a)))
                                          alphabet)

          fun closure(nil,         olds) = olds
            | closure(new :: news, olds) =
                if SymRel.memb(new, olds)
                then closure(news, olds)
                else closure(next new @ news,
                             SymRel.union(Set.sing new, olds))

          val start = (start1, start2)
          val stats = closure([start], Set.empty)

          fun isAccepting(q, r) =
                SymSet.memb(q, accepting1) andalso SymSet.memb(r, accepting2)

          fun emptyTransitionsFrom pair =
                Set.times3(Set.sing(symPairToSym pair),
                           Set.sing nil,
                           SymSet.map symPairToSym (nextEmpty pair))

          fun symTransitionsFrom(pair, a) =
                Set.times3(Set.sing(symPairToSym  pair),
                           Set.sing[a],
                           SymSet.map symPairToSym (nextSym(pair, a)))

          fun transitionsFrom pair =
                let val emptyTrans   = emptyTransitionsFrom pair
                    val symTransList =
                          Set.mapToList (fn a => symTransitionsFrom(pair, a))
                                        alphabet
                    val symTrans     = TranSet.genUnion symTransList
                in TranSet.union(emptyTrans, symTrans) end

          val start'     = symPairToSym start
          val stats'     = SymSet.map symPairToSym stats
          val accepting' =
                SymSet.map symPairToSym (Set.filter isAccepting stats)
          val trans'     =
                TranSet.genUnion(Set.mapToList transitionsFrom stats)

          val concr =
                {stats     = stats',
                 start     = start',
                 accepting = accepting',
                 trans     = trans'}
      in FA.fromConcr concr end

fun genInter nil           =
      M.errorString
      (fn () => ["attempt", "to", "intersect", "empty", "set"])
  | genInter [efa]         = efa
  | genInter (efa :: efas) = inter(efa, genInter efas)

(************************ Functions Inherited by NFA *************************)

fun prefix efa =
      let val efa = simplify efa
      in if Set.isEmpty(acceptingStates efa)
         then efa
         else let val stats     = states efa
                  val start     = startState efa
                  val accepting = stats
                  val trans     = transitions efa
                  val concr     =
                        {stats     = stats,
                         start     = start,
                         accepting = accepting,
                         trans     = trans}
              in FA.fromConcr concr end
      end

(****************************** Other Functions ******************************)

fun fromFA fa =
      let val stats     = FA.states fa
          val start     = FA.startState fa
          val accepting = FA.acceptingStates fa
          val trans     = FA.transitions fa

          fun newStateFromOld q =
                Sym.fromTop
                (Sym.Compound[SOME(Sym.fromString "1"), NONE, SOME q])

          fun newStateFromTranSplitting(q, y, z, r) =
                if null y
                  then newStateFromOld q
                else if null z
                  then newStateFromOld r
                else Sym.fromTop
                     (Sym.Compound
                      [SOME(Sym.fromString "2"),
                       NONE,
                       SOME(Sym.fromTop
                            (Sym.Compound
                             ([SOME q, NONE] @
                              map SOME y @
                              [NONE] @
                              map SOME z @
                              [NONE, SOME r])))])
              
          fun transSplittings(q, x, r) =
                map (fn (y, z) => (q, y, z, r)) (ListAux.allSplittings x)

          fun newTranFromAdjacentSplittings((q, y,  z,  r),
                                            (_, y', z', _)) =
                (newStateFromTranSplitting(q, y, z, r),
                 [hd z],
                 newStateFromTranSplitting(q, y', z', r))

          fun newTransitionsFromOldTransition(q, x, r) =
                if null x
                then Set.sing(newStateFromOld q, nil, newStateFromOld r)
                else let val adjacentSplittings =
                               ListAux.adjacentElts(transSplittings(q, x, r))
                     in TranSet.mapFromList newTranFromAdjacentSplittings
                                            adjacentSplittings
                     end

          fun statesOfTransition(q, _, r) = SymSet.fromList[q, r]

          val trans'     =
                TranSet.genUnion(Set.mapToList newTransitionsFromOldTransition
                                               trans)
          val stats'     =
                SymSet.union(SymSet.map newStateFromOld stats,
                             SymSet.genUnion(Set.mapToList statesOfTransition
                                                           trans'))
          val start'     = newStateFromOld start
          val accepting' = SymSet.map newStateFromOld accepting

          val concr =
                {stats     = stats',
                 start     = start',
                 accepting = accepting',
                 trans     = trans'}
      in FA.fromConcr concr end

end;
