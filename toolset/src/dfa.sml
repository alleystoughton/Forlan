(********************************** dfa.sml **********************************)

(* Copyright (C) 2001-2012 Alley Stoughton

   The file is part of the Forlan toolset for experimenting with
   formal languages.  See the file COPYING.txt for copying and
   usage restrictions. *)

structure DFA :> DFA =
struct

structure M  = Messages
structure L  = Lex

(********************************* Main Type *********************************)

type dfa = FA.fa

fun injToFA(dfa : dfa) : FA.fa = dfa

fun injToEFA dfa = EFA.projFromFA(injToFA dfa)

fun injToNFA dfa = NFA.projFromFA(injToFA dfa)

fun checkTran(_, x, _) =
      if length x <> 1
      then M.errorPP
           (fn () =>
                 [PP.fromString "invalid", PP.fromString "label",
                  PP.fromString "in", PP.colon(PP.fromString "transition"),
                  PP.quote(Str.toPP x)])
      else ()

fun checkTransitions nil       = ()
  | checkTransitions (t :: ts) = (checkTran t; checkTransitions ts)

fun checkState(f, _, nil)     = ()
  | checkState(f, q, b :: bs) =
      let val cs = f(q, [b])
      in if Set.size cs = 0
           then M.errorPP
                (fn () =>
                      [PP.fromString "no",
                       PP.fromString "transitions",
                       PP.fromString "for",
                       PP.fromString "state/input",
                       PP.fromString "symbol",
                       PP.fromString "pair:",
                       PP.quote(PP.block(true,
                                         [PP.comma(Sym.toPP q),
                                          Sym.toPP b]))])
         else if Set.size cs > 1
           then M.errorPP
                (fn () =>
                      [PP.fromString "more",
                       PP.fromString "than",
                       PP.fromString "one",
                       PP.fromString "transition",
                       PP.fromString "for",
                       PP.fromString "state/input",
                       PP.fromString "symbol",
                       PP.fromString "pair:",
                       PP.quote(PP.block(true,
                                         [PP.comma(Sym.toPP q),
                                          Sym.toPP b]))])
         else checkState(f, q, bs)
      end

fun checkStates(_, nil,     _)     = ()
  | checkStates(f, q :: qs, alpha) =
      (checkState(f, q, alpha); checkStates(f, qs, alpha))

fun check fa =
      (checkTransitions(Set.toList(FA.transitions fa));
       checkStates(FA.transitionFun fa,
                   Set.toList(FA.states fa),
                   Set.toList(FA.alphabet fa)))

fun valid fa =
      (M.quiet(fn () => check fa); true)
        handle _ => false

fun projFromFA(fa : FA.fa) : dfa = (check fa; fa)

val projFromEFA = projFromFA o EFA.injToFA

val projFromNFA = projFromFA o NFA.injToFA

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

(*********************** Functions Inherited From EFA ***********************)

fun inter(dfa1, dfa2) = projFromEFA(EFA.inter(injToEFA dfa1, injToEFA dfa2))

fun genInter dfas = projFromEFA(EFA.genInter(map injToEFA dfas))

(****************************** Other Functions ******************************)

fun determTransitionFun dfa =
      let val stats = states dfa
          val trans = transitions dfa
          val alpha = alphabet dfa
      in fn (p, a) =>
              if not(SymSet.memb(p, stats))
                then M.errorPP
                     (fn () =>
                           [PP.fromString "invalid",
                            PP.colon(PP.fromString "state"),
                            PP.quote(Sym.toPP p)])
              else if not(SymSet.memb(a, alpha))
                then M.errorPP
                     (fn () =>
                           [PP.fromString "symbol", PP.fromString "not",
                            PP.fromString "in",
                            PP.colon(PP.fromString "alphabet"),
                            PP.quote(Sym.toPP a)])
              else let fun find nil                  = M.cannotHappen()
                         | find ((q, y, r) :: trans) =
                             case Sym.compare(q, p) of
                                  LESS    => find trans
                                | EQUAL   =>
                                    (case Str.compare(y, [a]) of
                                          LESS    => find trans
                                        | EQUAL   => r
                                        | GREATER => M.cannotHappen())
                                | GREATER => M.cannotHappen()
                   in find(Set.toList trans) end
      end

fun determProcessStr dfa =
      let val detTranFun = determTransitionFun dfa
          val stats      = states dfa

          fun detProcStr(q, nil)     = q
            | detProcStr(q, b :: bs) = detProcStr(detTranFun(q, b), bs)
      in fn (q, x) =>
              if not(SymSet.memb(q, stats))
              then M.errorPP
                   (fn () =>
                         [PP.fromString "invalid",
                          PP.colon(PP.fromString "state"),
                          PP.quote(Sym.toPP q)])
              else detProcStr(q, x)
      end

fun determAccepted dfa =
      let val determProcStr = determProcessStr dfa
          val start         = FA.startState dfa
          val accepting     = FA.acceptingStates dfa
      in fn x =>
              M.quiet(fn () => SymSet.memb(determProcStr(start, x), accepting))
                handle _ => false
      end

fun fromNFA nfa =
      let val fa             = NFA.injToFA nfa
          val stats          = FA.states fa
          val start          = FA.startState fa
          val accepting      = FA.acceptingStates fa
          val alpha          = FA.alphabet fa
          val procStr        = FA.processStr fa
          val membSymSetSet  = Set.memb SymSet.compare
          val unionSymSetSet = Set.union SymSet.compare

          fun symSetToSym bs =
                let fun symsToSymOpts nil       = nil
                      | symsToSymOpts [b]       = [SOME b]
                      | symsToSymOpts (b :: bs) =
                          SOME b :: NONE :: symsToSymOpts bs
                in Sym.fromTop(Sym.Compound(symsToSymOpts(Set.toList bs))) end

          fun next qs = Set.mapToList (fn a => procStr(qs, [a])) alpha

          fun closure(nil,         olds) = olds
            | closure(new :: news, olds) =
                if membSymSetSet(new, olds)
                then closure(news, olds)
                else closure(next new @ news,
                             unionSymSetSet(Set.sing new, olds))

          val start' = Set.sing start
          val stats' = closure([start'], Set.empty)

          fun isAccepting qs = Set.isNonEmpty(SymSet.inter(qs, accepting))

          fun transitionsFrom qs =
                TranSet.map (fn a =>
                                  (symSetToSym qs,
                                   [a],
                                   symSetToSym(procStr(qs, [a]))))
                            alpha

          val start''     = symSetToSym start'
          val stats''     = SymSet.map symSetToSym stats'
          val accepting'' =
                SymSet.map symSetToSym (Set.filter isAccepting stats')
          val trans''     =
                TranSet.genUnion(Set.mapToList transitionsFrom stats')

          val concr =
                {stats     = stats'',
                 start     = start'',
                 accepting = accepting'',
                 trans     = trans''}
      in FA.fromConcr concr end

fun determSimplified dfa =
      FA.reachified dfa andalso
      let val numDead = numStates dfa - FA.numStates(FA.simplify dfa)
      in numDead <= 1 end

fun determSimplify(dfa, minAlpha) =
      let val fa        = FA.simplify dfa
          val stats     = FA.states fa
          val start     = FA.startState fa
          val accepting = FA.acceptingStates fa
          val trans     = FA.transitions fa
          val tranFun   = FA.transitionFun fa
          val alpha     = FA.alphabet fa
          val alpha'    = SymSet.union(alpha, minAlpha)

          fun newTransOfState(_, nil,     _)    = Set.empty
            | newTransOfState(q, b :: bs, dead) =
                if Sym.equal(q, dead) orelse Set.isEmpty(tranFun(q, [b]))
                then TranSet.union(Set.sing(q, [b], dead),
                                   newTransOfState(q, bs, dead))
                else newTransOfState(q, bs, dead)
            
          fun newTransOfStates(nil,     _)    = Set.empty
            | newTransOfStates(q :: qs, dead) =
                TranSet.union(newTransOfState(q, Set.toList alpha', dead),
                              newTransOfStates(qs, dead))
      in if SymSet.equal(alpha, alpha') andalso valid fa
         then fa
         else let val dead       =
                        if Set.isEmpty accepting  (* so fa has one state and *)
                        then start                (* no transitions *)
                        else let val dead = Sym.fromString "<dead>"
                                 val rel  =
                                       SymRel.makeBijectionFromAvoiding
                                       (Set.sing dead, stats)
                             in SymRel.applyFunction rel dead end
                  val stats'     = SymSet.union(Set.sing dead, stats)
                  val start'     = start
                  val accepting' = accepting
                  val trans'     =
                        TranSet.union(trans,
                                      newTransOfStates(Set.toList stats',
                                                       dead))
                  val concr      =
                        {stats     = stats',
                         start     = start',
                         accepting = accepting',
                         trans     = trans'}
              in FA.fromConcr concr end
      end

fun minimize dfa =
      let val dfa              = determSimplify(dfa, Set.empty)
          val stats            = FA.states dfa
          val start            = FA.startState dfa
          val accepting        = FA.acceptingStates dfa
          val nonAccepting     = SymSet.minus(stats, accepting)
          val alpha            = FA.alphabet dfa
          val detTranFun       = determTransitionFun dfa
          val tranFunBackwards = FA.transitionFunBackwards dfa
          val mapSymSetSet     = Set.map SymSet.compare

          fun symSetToSym bs =
                let fun symsToSymOpts nil       = nil
                      | symsToSymOpts [b]       = [SOME b]
                      | symsToSymOpts (b :: bs) =
                          SOME b :: NONE :: symsToSymOpts bs
                in Sym.fromTop(Sym.Compound(symsToSymOpts(Set.toList bs))) end

          fun timesToList(qs, rs) = Set.toList(Set.times(qs, rs))

          fun nextPairsOfSym((q, r), a) = 
                timesToList(tranFunBackwards(q, [a]),
                            tranFunBackwards(r, [a]))

          fun nextPairs pair =
                List.concat(Set.mapToList (fn a => nextPairsOfSym(pair, a))
                                          alpha)

          fun closure(nil,         olds) = olds
            | closure(new :: news, olds) =
                if SymRel.memb(new, olds)
                then closure(news, olds)
                else closure(nextPairs new @ news,
                             SymRel.union(Set.sing new, olds))

          val initDistinguishable =
                timesToList(accepting, nonAccepting) @
                timesToList(nonAccepting, accepting)

          val distinguishable = closure(initDistinguishable, Set.empty)

          val indistinguishable =
                SymRel.minus(Set.times(stats, stats), distinguishable)

          fun equivClass p = SymRel.apply(indistinguishable, Set.sing p)

          fun transitionsFrom qs =
                let val q = hd(Set.toList qs)
                in TranSet.map (fn a =>
                                     (symSetToSym qs,
                                      [a],
                                      let val r = detTranFun(q, a)
                                      in symSetToSym(equivClass r) end))
                               alpha
                end

          val stats'      = mapSymSetSet equivClass stats
          val stats''     = SymSet.map symSetToSym stats'
          val start''     = symSetToSym(equivClass start)
          val accepting'' = SymSet.map (symSetToSym o equivClass) accepting
          val trans''     =
                TranSet.genUnion(Set.mapToList transitionsFrom stats')

          val concr =
                {stats     = stats'',
                 start     = start'',
                 accepting = accepting'',
                 trans     = trans''}
      in FA.fromConcr concr end

fun complement(dfa, minAlpha) =
      let val dfa       = determSimplify(dfa, minAlpha)
          val stats     = FA.states dfa
          val start     = FA.startState dfa
          val accepting = FA.acceptingStates dfa
          val trans     = FA.transitions dfa
          val concr     =
              {stats     = stats,
               start     = start,
               accepting = SymSet.minus(stats, accepting),
               trans     = trans}
      in FA.fromConcr concr end

fun minus(dfa1, dfa2) = inter(dfa1, complement(dfa2, FA.alphabet dfa1))

(* the relationship between two languages:

   Equal means they are equal;
   ProperSub x means the first is a proper subset of the second, and
     that x is in the second but not the first;
   ProperSup x means the first is a proper superset of the second, and
     that x is in the first but not the second;
   Incomp(x, y) means the languages are incomparable, x is in the first
     but not the second, and y is in the second but not the first

   furthermore, the counterexamples x and y are of minimum length *)

datatype relationship = Equal
                      | ProperSub of Str.str
                      | ProperSup of Str.str
                      | Incomp    of Str.str * Str.str

fun relation(dfa1, dfa2) =
      let val alpha = SymSet.union(FA.alphabet dfa1, FA.alphabet dfa2)

          val dfa1        = determSimplify(dfa1, alpha)
          val start1      = FA.startState dfa1
          val accepting1  = FA.acceptingStates dfa1
          val detTranFun1 = determTransitionFun dfa1

          val dfa2        = determSimplify(dfa2, alpha)
          val start2      = FA.startState dfa2
          val accepting2  = FA.acceptingStates dfa2
          val detTranFun2 = determTransitionFun dfa2

          val comparePair = Set.comparePair(Sym.compare, Sym.compare)
          val lookupPair  = Tab.lookup comparePair
          val updatePair  = Tab.update comparePair

          fun incomp (Incomp _) = true
            | incomp _          = false

          fun updateRelat(((q, r), x), relat) =
                if SymSet.memb(q, accepting1)
                then if SymSet.memb(r, accepting2)
                     then relat
                     else case relat of
                               Equal       => ProperSup x
                             | ProperSub y => Incomp(x, y)
                             | _           => relat
                else if SymSet.memb(r, accepting2)
                     then case relat of
                               Equal        => ProperSub x
                             | ProperSup y  => Incomp(y, x)
                             | _            => relat
                     else relat

          fun closure(nil,                 _,    relat) = relat
            | closure(((q, r), x) :: news, olds, relat) =
                case lookupPair(olds, (q, r)) of
                     NONE   =>
                       let val binds =
                                 Set.mapToList (fn b =>
                                                     ((detTranFun1(q, b),
                                                       detTranFun2(r, b)),
                                                       x @ [b]))
                                               alpha
                           val relat = updateRelat(((q, r), x), relat)
                       in if incomp relat
                          then relat
                          else closure(news @ binds,  (* breadth-first *)
                                       updatePair(olds, [((q, r), x)]),
                                       relat)
                       end
                   | SOME _ => closure(news, olds, relat)
      in closure([((start1, start2), nil)], Tab.empty, Equal) end

fun relationship dfaPair =
      case relation dfaPair of
           Equal        =>
             M.messageString(fn () => ["languages", "are", "equal"])
         | ProperSub x  =>
             M.messageString
             (fn () =>
                   ["first", "language", "is", "a", "proper",
                    "subset", "of", "second", "language:",
                    PP.toString(PP.quote(Str.toPP x)),
                    "is", "in", "second", "language", "but",
                    "is", "not", "in", "first", "language"])
         | ProperSup x  =>
             M.messageString
             (fn () =>
                   ["first", "language", "is", "a", "proper",
                    "superset", "of", "second", "language:",
                    PP.toString(PP.quote(Str.toPP x)),
                    "is", "in", "first", "language", "but", "is",
                    "not", "in", "second", "language"])
         | Incomp(x, y) =>
             M.messageString
             (fn () =>
                   ["neither", "language", "is", "a", "subset",
                    "of", "the", "other", "language:",
                    PP.toString(PP.quote(Str.toPP x)),
                    "is", "in", "first", "language", "but", "is",
                    "not", "in", "second", "language;",
                    PP.toString(PP.quote(Str.toPP y)),
                    "is", "in", "second", "language", "but", "is",
                    "not", "in", "first", "language"])

fun equivalent dfaPair =
      case relation dfaPair of
           Equal => true
         | _     => false

fun subset dfaPair =
      case relation dfaPair of
           ProperSub _ => true
         | Equal       => true
         | _           => false

end;
