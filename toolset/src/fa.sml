(*********************************** fa.sml **********************************)

(* Copyright (C) 2012 Alley Stoughton

   The file is part of the Forlan toolset for experimenting with
   formal languages.  See the file COPYING.txt for copying and
   usage restrictions. *)

structure FA :> FA =
struct

structure M  = Messages
structure L  = Lex

(*********************************** Types ***********************************)

type concr =
       {stats     : Sym.sym Set.set,
        start     : Sym.sym,
        accepting : Sym.sym Set.set,
        trans     : Tran.tran Set.set}
               
type fa = concr

fun checkStart(start, stats) =
      if not(SymSet.memb(start, stats))
      then M.errorPP
           (fn () =>
                 [PP.fromString "invalid", PP.fromString "start",
                  PP.fromString "state:", PP.quote(Sym.toPP start)])
      else ()

fun checkAccepting(nil, _)         = ()
  | checkAccepting(q :: qs, stats) =
      if not(SymSet.memb(q, stats))
      then M.errorPP
           (fn () =>
                 [PP.fromString "invalid", PP.fromString "accepting",
                  PP.fromString "state:", PP.quote(Sym.toPP q)])
      else checkAccepting(qs, stats)

fun checkTransitions(nil,                _)     = ()
  | checkTransitions((q, _, r) :: trans, stats) =
      if not(SymSet.memb(q, stats))
        then M.errorPP
             (fn () =>
                   [PP.fromString "invalid", PP.fromString "state",
                    PP.fromString "in", PP.fromString "transition:",
                    PP.quote(Sym.toPP q)])
      else if not(SymSet.memb(r, stats))
        then M.errorPP
             (fn () =>
                   [PP.fromString "invalid", PP.fromString "state",
                    PP.fromString "in", PP.fromString "transition:",
                    PP.quote(Sym.toPP r)])
      else checkTransitions(trans, stats)
      
fun check{stats, start, accepting, trans} =
      (checkStart(start, stats);
       checkAccepting(Set.toList accepting, stats);
       checkTransitions(Set.toList trans, stats))

fun valid concr =
      (M.quiet(fn () => check concr); true)
        handle _ => false

fun fromConcr(concr : concr) : fa = (check concr; concr)

fun toConcr(fa : fa) : concr = fa

(*********************************** Input ***********************************)

fun inpFA lts =
      let val lts              =
                L.checkInLabToks(L.Heading "{states}", lts)
          val (stats, lts)     = SymSet.inputFromLabToks lts
          val lts              =
                L.checkInLabToks(L.Heading "{startstate}", lts)
          val (start, lts)     = Sym.inputFromLabToks lts
          val lts              =
                L.checkInLabToks(L.Heading "{acceptingstates}", lts)
          val (accepting, lts) = SymSet.inputFromLabToks lts
          val lts              =
                L.checkInLabToks(L.Heading "{transitions}", lts)
          val (trans, lts)     = TranSet.inputFromLabToks lts
          val concr            =
                {stats     = stats,
                 start     = start,
                 accepting = accepting,
                 trans     = trans}
      in (fromConcr concr, lts) end

fun fromString s =
      case inpFA(L.lexString s) of
           (fa, [(_, L.EOF)]) => fa
         | (_,  nil)          => M.cannotHappen() 
         | (_,  lt :: _)      => L.unexpectedTok lt

fun input fil =
      case inpFA(L.lexFile fil) of
           (fa, [(_, L.EOF)]) => fa
         | (_,  nil)          => M.cannotHappen() 
         | (_,  lt :: _)      => L.unexpectedTok lt

(****************** Output (Inherited by DFA, NFA and EFA) ******************)

fun toPP{stats, start, accepting, trans} =
      PP.block(true,
               [PP.block(true,
                         [PP.fromString "{states}",
                         SymSet.toPP stats]),
                PP.block(true,
                         [PP.fromString "{start state}",
                          Sym.toPP start]),
                PP.block(true,
                         [PP.fromString "{accepting states}",
                          SymSet.toPP accepting]),
                PP.block(true,
                         [PP.fromString "{transitions}",
                          TranSet.toPP trans])])

val toString = PP.toString o toPP

fun output("",  fa) = (print(toString fa); print PP.newline)
  | output(fil, fa) =
      case SOME(TextIO.openOut fil) handle _ => NONE of
           NONE     =>
             M.errorPP
             (fn () =>
                   [PP.fromString "unable", PP.fromString "to",
                    PP.fromString "open", PP.fromString "file:",
                    PP.quote(PP.fromStringSplitEscape fil)])
         | SOME stm =>
             (TextIO.output(stm, toString fa);
              TextIO.output(stm, PP.newline);
              TextIO.closeOut stm)

(****************** Functions Inherited by DFA, NFA and EFA ******************)

fun states (fa : fa) = #stats fa

fun startState (fa : fa) = #start fa

fun acceptingStates (fa : fa) = #accepting fa

fun transitions (fa : fa) = #trans fa

fun compare(fa1, fa2) =
      case SymSet.compare(states fa1, states fa2) of
           LESS    => LESS
         | EQUAL   =>
             (case Sym.compare(startState fa1, startState fa2) of
                   LESS    => LESS
                 | EQUAL   =>
                     (case SymSet.compare(acceptingStates fa1,
                                          acceptingStates fa2) of
                           LESS    => LESS
                         | EQUAL   =>
                             TranSet.compare(transitions fa1, transitions fa2)
                         | GREATER => GREATER)
                 | GREATER => GREATER)
         | GREATER => GREATER

fun equal faPair = compare faPair = EQUAL

fun numStates fa = Set.size(states fa)

fun numTransitions fa = Set.size(transitions fa)

fun alphabet fa =
      SymSet.genUnion(Set.mapToList (fn (_, bs, _) => SymSet.fromList bs)
                                    (transitions fa))

fun sub(fa1, fa2) =
      SymSet.subset(states fa1, states fa2)                   andalso
      Sym.equal(startState fa1, startState fa2)               andalso
      SymSet.subset(acceptingStates fa1, acceptingStates fa2) andalso
      TranSet.subset(transitions fa1, transitions fa2)      

fun labsOfTransitionsBetween fa (qOpt, rOpt) =
      let val trans = transitions fa

          fun labs nil                    = Set.empty
            | labs ((q', x, r') :: trans) =
                case qOpt of
                     NONE   =>
                       (case rOpt of
                             NONE   =>
                               StrSet.union(Set.sing x, labs trans)
                           | SOME r =>
                               if Sym.equal(r', r)
                               then StrSet.union(Set.sing x, labs trans)
                               else labs trans)
                   | SOME q =>
                       (case Sym.compare(q', q) of
                             LESS    => labs trans
                           | EQUAL   =>
                               (case rOpt of
                                     NONE   =>
                                       StrSet.union(Set.sing x, labs trans)
                                   | SOME r =>
                                       if Sym.equal(r', r)
                                       then StrSet.union(Set.sing x,
                                                         labs trans)
                                       else labs trans)
                           | GREATER => Set.empty)
      in labs(Set.toList trans) end

fun transitionFun fa (p, x) =
      let val stats = states fa
          val trans = transitions fa

          fun find nil                  = Set.empty
            | find ((q, y, r) :: trans) =
                case Sym.compare(p, q) of
                     LESS    => Set.empty
                   | EQUAL   =>
                       (case Str.compare(x, y) of
                             LESS    => Set.empty
                           | EQUAL   => SymSet.union(Set.sing r, find trans)
                           | GREATER => find trans)
                   | GREATER => find trans
      in if not(SymSet.memb(p, stats))
         then M.errorString
              (fn () => ["symbol", "is", "not", "a", "state"])
         else find(Set.toList trans)
      end

fun transitionFunBackwards fa (p, x) =
      let val stats = states fa
          val trans = transitions fa

          fun find nil                  = Set.empty
            | find ((q, y, r) :: trans) =
                if Sym.equal(p, r) andalso Str.equal(x, y)
                then SymSet.union(Set.sing q, find trans)
                else find trans
      in if not(SymSet.memb(p, stats))
         then M.errorString
              (fn () => ["symbol", "is", "not", "a", "state"])
         else find(Set.toList trans)
      end

fun processStr fa =
      let val stats = states fa
          val trans = transitions fa

          (* resid is short for residual *)
          type resid = Sym.sym * Str.str

          val compareResid : resid * resid -> order =
                Set.comparePair(Sym.compare, Str.compare)
          val unionResidSet                         = Set.union compareResid
          val membResidSet                          = Set.memb compareResid

          fun newResids(nil,                _)      = nil
            | newResids((p, y, r) :: trans, (q, x)) =
                case Sym.compare(q, p) of
                     LESS    => nil
                   | EQUAL   =>
                       (case Str.removePrefix(y, x) of
                             NONE   => newResids(trans, (q, x))
                           | SOME z => (r, z) :: newResids(trans, (q, x)))
                   | GREATER => newResids(trans, (q, x))
          
          fun closure(nil,         olds) = olds
            | closure(new :: news, olds) =
                if membResidSet(new, olds)
                then closure(news, olds)
                else closure(newResids(Set.toList trans, new) @ news,
                             unionResidSet(Set.sing new, olds))

          local
            fun res nil                = nil
              | res ((q, x) :: resids) =
                  if null x then q :: res resids else res resids
          in
            fun results resids = SymSet.fromList(res(Set.toList resids))
          end

          fun processStr(qs, x) =
                if Set.exists (fn q => not(SymSet.memb(q, stats))) qs
                then M.errorString
                     (fn () =>
                           ["element", "of", "set", "of", "symbols",
                            "is", "not", "a", "state"])
                else let val init = Set.toList(Set.times(qs, Set.sing x))
                     in results(closure(init, Set.empty)) end
      in processStr end

fun accepted fa =
      let val procStr   = processStr fa
          val start     = startState fa
          val accepting = acceptingStates fa
          val alpha     = alphabet fa

          fun accepted x =
                Set.isNonEmpty(SymSet.inter(procStr(Set.sing start, x),
                                            accepting))
      in accepted end

(* not included in NFA and DFA, because would be identity *)

fun emptyClose fa =
      let val stats = states fa
          val trans = transitions fa

          fun newStates(nil,                  _) = nil
            | newStates((q, nil, r) :: trans, p) =
                (case Sym.compare(p, q) of
                      LESS    => nil
                    | EQUAL   => r :: newStates(trans, p)
                    | GREATER => newStates(trans, p))
            | newStates(_ :: trans,           p) = newStates(trans, p)

          fun closure(nil,         olds) = olds
            | closure(new :: news, olds) =
                if SymSet.memb(new, olds)
                then closure(news, olds)
                else closure
                     (newStates(Set.toList trans, new) @ news,
                      SymSet.union(Set.sing new, olds))

          fun emptyClose qs =
                if Set.exists (fn q => not(SymSet.memb(q, stats))) qs
                then M.errorString
                     (fn () =>
                           ["element", "of", "set", "of", "symbols",
                            "is", "not", "a", "state"])
                else closure(Set.toList qs, Set.empty)
      in emptyClose end

(* not included in NFA and DFA, because would be identity *)

fun emptyCloseBackwards fa =
      let val stats = states fa
          val trans = transitions fa

          fun newStates(nil,                  _) = nil
            | newStates((q, nil, r) :: trans, p) =
                if Sym.equal(r, p)
                then q :: newStates(trans, p)
                else newStates(trans, p)
            | newStates(_ :: trans,           p) = newStates(trans, p)

          fun closure(nil,         olds) = olds
            | closure(new :: news, olds) =
                if SymSet.memb(new, olds)
                then closure(news, olds)
                else closure
                     (newStates(Set.toList trans, new) @ news,
                      SymSet.union(Set.sing new, olds))

          fun emptyCloseBackwards qs =
                if Set.exists (fn q => not(SymSet.memb(q, stats))) qs
                then M.errorString
                     (fn () =>
                           ["element", "of", "set", "of", "symbols",
                            "is", "not", "a", "state"])
                else closure(Set.toList qs, Set.empty)
      in emptyCloseBackwards end

fun reachableFrom fa =
      let val trans = transitions fa

          fun newStates(nil,                _) = nil
            | newStates((q, x, r) :: trans, p) =
                (case Sym.compare(p, q) of
                      LESS    => nil
                    | EQUAL   => r :: newStates(trans, p)
                    | GREATER => newStates(trans, p))

          fun closure(nil,         olds) = olds
            | closure(new :: news, olds) =
                if SymSet.memb(new, olds)
                then closure(news, olds)
                else closure(newStates(Set.toList trans, new) @ news,
                             SymSet.union(Set.sing new, olds))

          fun reachFrom qs = closure(Set.toList qs, Set.empty)
      in reachFrom end

fun reachableFromBackwards fa =
      let val trans = transitions fa

          fun newStates(nil,                _) = nil
            | newStates((q, x, r) :: trans, p) =
                if Sym.equal(r, p)
                then q :: newStates(trans, p)
                else newStates(trans, p)

          fun closure(nil,         olds) = olds
            | closure(new :: news, olds) =
                if SymSet.memb(new, olds)
                then closure(news, olds)
                else closure(newStates(Set.toList trans, new) @ news,
                             SymSet.union(Set.sing new, olds))

          fun reachFromBackwards qs = closure(Set.toList qs, Set.empty)
      in reachFromBackwards end

fun reachify fa =
      let val stats     = states fa
          val start     = startState fa
          val accepting = acceptingStates fa
          val trans     = transitions fa

          val stats'     = reachableFrom fa (Set.sing start)
          val start'     = start
          val accepting' =
                Set.filter (fn q => SymSet.memb(q, stats')) accepting
          val trans'     =
                Set.filter (fn (q, _, _) => SymSet.memb(q, stats'))
                           trans
      in {stats     = stats',
          start     = start',
          accepting = accepting',
          trans     = trans'}
      end

fun reachified fa = equal(reachify fa, fa)

fun validStatesRenaming(fa, rel) =
      let val stats = states fa
      in SymRel.bijectionFromAvoiding(rel, stats, Set.empty) end

fun renameStates(fa as {stats, start, accepting, trans}, rel) =
      if validStatesRenaming(fa, rel)
      then let val renam      = SymRel.applyFunction rel
               val stats'     = SymSet.map renam stats
               val start'     = renam start
               val accepting' = SymSet.map renam accepting
               val trans'     =
                     TranSet.map (fn (q, x, r) => (renam q, x, renam r)) trans
           in {stats     = stats',
               start     = start',
               accepting = accepting',
               trans     = trans'}
           end
      else M.errorString
           (fn () => ["invalid", "states", "renaming", "for", "FA"])

fun renameStatesCanonically fa =
      let val stats = states fa

          fun position q =
                valOf(Set.position (fn p => Sym.equal(p, q)) stats)

          val renam =
                if Set.size stats <= 26
                then fn q =>
                          Sym.fromString(str(chr(ord #"A" + position q - 1)))
                else fn q =>
                          let val s =
                                    String.concat["<",
                                                  Int.toString(position q),
                                                  ">"]
                          in Sym.fromString s end
          val rel   = SymRel.mlFunctionToFunction(renam, stats)
      in renameStates(fa, rel) end

fun isomorphism(fa1, fa2, rel) =
      validStatesRenaming(fa1, rel)      andalso
      equal(fa2, renameStates(fa1, rel))

fun findIsomorphismOpt(fa1, fa2) =
      let val stats1             = states fa1
          val start1             = startState fa1
          val accepting1         = acceptingStates fa1
          val trans1             = transitions fa1
          val rejecting1         = SymSet.minus(stats1, accepting1)
          val acceptingNonStart1 = SymSet.minus(accepting1, Set.sing start1)
          val rejectingNonStart1 = SymSet.minus(rejecting1, Set.sing start1)
          val tranFun1           = transitionFun fa1
          val labsOfTransBetw1   = labsOfTransitionsBetween fa1

          val stats2             = states fa2
          val start2             = startState fa2
          val accepting2         = acceptingStates fa2
          val trans2             = transitions fa2
          val rejecting2         = SymSet.minus(stats2, accepting2)
          val acceptingNonStart2 = SymSet.minus(accepting2, Set.sing start2)
          val rejectingNonStart2 = SymSet.minus(rejecting2, Set.sing start2)
          val tranFun2           = transitionFun fa2
          val labsOfTransBetw2   = labsOfTransitionsBetween fa2

          fun findIso(rel, _,   _,   nil)                   = SOME rel
            | findIso(rel, dom, ran, (qs, rs) :: constrnts) =
                case Set.size qs of
                     0 => findIso(rel, dom, ran, constrnts)
                   | 1 =>
                       let val q = hd(Set.toList qs)
                           val r = hd(Set.toList rs)
                       in simple(rel, dom, ran, (q, r), constrnts) end
                   | _ => complex(rel, dom, ran, (qs, rs), constrnts)

          and simple(rel, dom, ran, (q, r), constrnts) =
                if SymRel.memb((q, r), rel)
                  then findIso(rel, dom, ran, constrnts)
                else if SymSet.memb(q, dom) orelse SymSet.memb(r, ran)
                  then NONE
                else let val rel   = SymRel.union(Set.sing(q, r), rel)
                         val dom   = SymSet.union(Set.sing q, dom)
                         val ran   = SymSet.union(Set.sing r, ran)
                         val labs1 = labsOfTransBetw1(SOME q, NONE)
                         val labs2 = labsOfTransBetw2(SOME r, NONE)

                         fun newConstraints nil           = SOME nil
                           | newConstraints (lab :: labs) =
                               let val qs = tranFun1(q, lab)
                                   val rs = tranFun2(r, lab)
                               in if Set.size qs <> Set.size rs
                                  then NONE
                                  else case newConstraints labs of
                                            NONE      => NONE
                                          | SOME news => SOME((qs, rs) :: news)
                               end
                     in if not(StrSet.equal(labs1, labs2))
                        then NONE
                        else case newConstraints(Set.toList labs1) of
                                  NONE      => NONE
                                | SOME news =>
                                    findIso(rel, dom, ran, news @ constrnts)
                     end

          and complex(rel, dom, ran, (qs, rs), constrnts) =
                let val q  = Set.hd qs
                    val qs = Set.tl qs

                    fun tryPairings nil       = NONE
                      | tryPairings (p :: ps) =
                          let val rs        = SymSet.minus(rs, Set.sing p)
                              val constrnts =
                                    [(Set.sing q, Set.sing p), (qs, rs)] @
                                    constrnts
                          in case findIso(rel, dom, ran, constrnts) of
                                  NONE   => tryPairings ps
                                | relOpt => relOpt
                          end
                in tryPairings(Set.toList rs) end
      in if Set.size stats1 <> Set.size stats2         orelse
            Set.size accepting1 <> Set.size accepting2 orelse
            Set.size trans1 <> Set.size trans2         orelse
            SymSet.memb(start1, accepting1) <>
            SymSet.memb(start2, accepting2)
         then NONE
         else let val constrnts =
                        [(Set.sing start1, Set.sing start2),
                         (acceptingNonStart1, acceptingNonStart2),
                         (rejectingNonStart1, rejectingNonStart2)]
              in findIso(Set.empty, Set.empty, Set.empty, constrnts) end
      end

fun findIsomorphism(fa1, fa2) =
      case findIsomorphismOpt(fa1, fa2) of
           NONE    => M.errorString(fn () => ["not", "isomorphic"])
        | SOME rel => rel

fun isomorphic(fa1, fa2) =
      case findIsomorphismOpt(fa1, fa2) of
           NONE   => false
         | SOME _ => true

fun renameAlphabet(fa as {stats, start, accepting, trans}, rel) =
      if SymRel.bijectionFromSupersetAvoiding(rel, alphabet fa, Set.empty)
      then let val renam  = SymRel.applyFunction rel
               val trans' =
                     TranSet.map (fn (q, x, r) => (q, map renam x, r)) trans
           in {stats     = stats,
               start     = start,
               accepting = accepting,
               trans     = trans'}
           end
      else M.errorString
           (fn () => ["invalid", "alphabet", "renaming", "for", "FA"])

fun checkLP fa =
      let val stats = states fa
          val trans = transitions fa

          fun err tran =
                M.errorPP
                (fn () =>
                      [PP.fromString "invalid",
                       PP.fromString "transition:",
                       PP.quote(TranSet.toPP(Set.sing tran))])

          fun check(LP.Sym q)          =
                if SymSet.memb(q, stats)
                then ()
                else M.errorPP
                     (fn () =>
                           [PP.fromString "invalid", PP.fromString "state:",
                            PP.quote(Sym.toPP q)])
            | check(LP.Cons(q, x, lp)) =
                let val r = LP.startState(LP.fromConcr lp)
                in if not(TranSet.memb((q, x, r), trans))
                   then err(q, x, r)
                   else check lp
                end
      in fn lp => check(LP.toConcr lp) end

fun validLP fa =
      let val checkLP = checkLP fa
      in fn lp =>
              (M.quiet(fn () => checkLP lp); true)
                handle _ => false
      end

fun findLPOpt fa (qs, x, rs) =
      let val stats = states fa
          val trans = transitions fa
      
          val _ =
                if Set.exists (fn q => not(SymSet.memb(q, stats))) qs
                  then M.errorString
                       (fn () =>
                             ["element", "of", "first", "set", "of",
                              "symbols", "has", "non", "variable"])
                else if Set.exists (fn r => not(SymSet.memb(r, stats))) rs
                  then M.errorString
                       (fn () =>
                             ["element", "of", "second", "set", "of",
                              "symbols", "has", "non", "variable"])
                else ()

          (* resid is short for residual *)
          type resid = Sym.sym * Str.str

          val compareResid : resid * resid -> order =
                Set.comparePair(Sym.compare, Str.compare)
          val lookupResid  = Tab.lookup compareResid
          val updateResid  = Tab.update compareResid

          (* bindings of labeled paths to residuals *)
          type binding = resid * LP.lp

          fun newBindings(nil,                _) : binding list = nil
            | newBindings((q, y, r) :: trans, ((p, x), lp))     =
                if Sym.equal(p, q)
                then case Str.removePrefix(y, x) of
                          NONE   => newBindings(trans, ((p, x), lp))
                        | SOME z =>
                            let val lp' = LP.join(lp, LP.cons(p, y, LP.sym r))
                            in ((r, z), lp') ::
                               newBindings(trans, ((p, x), lp))
                            end
                else newBindings(trans, ((p, x), lp))

          fun newBindings(nil,                _) : binding list = nil
            | newBindings((q, y, r) :: trans, ((p, x), lp))     =
                (case Sym.compare(p, q) of
                      LESS    => nil
                    | EQUAL   =>
                        (case Str.removePrefix(y, x) of
                              NONE   => newBindings(trans, ((p, x), lp))
                            | SOME z =>
                                let val lp' =
                                          LP.join(lp, LP.cons(p, y, LP.sym r))
                                in ((r, z), lp') ::
                                   newBindings(trans, ((p, x), lp))
                                end)
                    | GREATER => newBindings(trans, ((p, x), lp)))

          fun answer(((q, x), _) : binding) = SymSet.memb(q, rs) andalso null x

          type tab = (resid, LP.lp)Tab.tab

          fun search(nil : binding list, _ : tab) = NONE
            | search(new :: news,        olds)    =
                if answer new
                then SOME(#2 new)
                else case lookupResid(olds, #1 new) of
                          NONE   =>
                            search(news @  (* breadth-first *)
                                   newBindings(Set.toList trans, new),
                                   updateResid(olds, [new]))
                        | SOME _ => search(news, olds)

          val initBindings = Set.mapToList (fn q => ((q, x), LP.sym q)) qs
      in search(initBindings, Tab.empty) end

fun findLP fa (qs, x, rs) =
      case findLPOpt fa (qs, x, rs) of
           NONE    =>
             M.errorString
             (fn () => ["no", "such", "labeled", "path", "exists"])
         | SOME lp => lp

fun findAcceptingLPOpt fa =
      let val findLPOpt = findLPOpt fa
          val start     = startState fa
          val accepting = acceptingStates fa
      in fn x => findLPOpt(Set.sing start, x, accepting) end

fun findAcceptingLP fa =
      let val findLP    = findLP fa
          val start     = startState fa
          val accepting = acceptingStates fa
      in fn x => findLP(Set.sing start, x, accepting) end

val emptyStr : fa =
      {stats     = Set.sing(Sym.fromString "A"),
       start     = Sym.fromString "A",
       accepting = Set.sing(Sym.fromString "A"),
       trans     = Set.empty}

val emptySet : fa =
      {stats     = Set.sing(Sym.fromString "A"),
       start     = Sym.fromString "A",
       accepting = Set.empty,
       trans     = Set.empty}

(******************** Functions Inherited by NFA and EFA ********************)

fun fromSym a =
      {stats     = SymSet.fromList[Sym.fromString "A", Sym.fromString "B"],
       start     = Sym.fromString "A",
       accepting = Set.sing(Sym.fromString "B"),
       trans     = Set.sing(Sym.fromString "A", [a], Sym.fromString "B")}

fun simplify fa =
      let val fa        = reachify fa
          val stats     = states fa
          val start     = startState fa
          val accepting = acceptingStates fa
          val trans     = transitions fa
      in if Set.isEmpty accepting
         then {stats     = Set.sing start,
               start     = start,
               accepting = Set.empty,
               trans     = Set.empty}
         else let val stats'     = reachableFromBackwards fa accepting
                  val start'     = start
                  val accepting' = accepting
                  val trans'     =
                        Set.filter (fn (_, _, r) => SymSet.memb(r, stats'))
                                   trans

                  fun implicit((p, x, q), fa) =
                        SymSet.memb(q, processStr fa (Set.sing p, x))

                  (* in a call remRedun(trans1, trans2), the FA built
                     from the union of trans1 and trans2 has the same
                     implicit transitions as that built from trans';
                     but none of the elements of trans1 are redundant
                     in the FA built from trans1 *)

                  fun remRedun(trans, nil)            = trans
                    | remRedun(trans, tran :: trans') =
                        if implicit
                           (tran,
                            {stats     = stats',
                             start     = start',
                             accepting = accepting',
                             trans     =
                               TranSet.union(trans, TranSet.fromList trans')})
                        then remRedun(trans, trans')
                        else remRedun
                             (TranSet.union(trans, Set.sing tran), trans')

                  (* favor keeping earlier transitions *)

                  fun removeRedundant trans =
                        remRedun(Set.empty, rev(Set.toList trans'))
              in {stats     = stats',
                  start     = start',
                  accepting = accepting',
                  trans     = removeRedundant trans'}
              end
      end

fun simplified fa = equal(simplify fa, fa)

(************************ Functions Inherited by EFA ************************)

fun union(fa1, fa2) =
      let fun renam1 a =
                Sym.fromTop
                (Sym.Compound[SOME(Sym.fromString "1"), NONE, SOME a])

          fun renam2 a =
                Sym.fromTop
                (Sym.Compound[SOME(Sym.fromString "2"), NONE, SOME a])

          val rel1 = SymRel.mlFunctionToFunction(renam1, states fa1)
          val rel2 = SymRel.mlFunctionToFunction(renam2, states fa2)

          val fa1        = renameStates(fa1, rel1)
          val stats1     = states fa1
          val start1     = startState fa1
          val accepting1 = acceptingStates fa1
          val trans1     = transitions fa1

          val fa2        = renameStates(fa2, rel2)
          val stats2     = states fa2
          val start2     = startState fa2
          val accepting2 = acceptingStates fa2
          val trans2     = transitions fa2

          val start     = Sym.fromString "A"
          val stats     =
                SymSet.union(Set.sing start, SymSet.union(stats1, stats2))
          val accepting = SymSet.union(accepting1, accepting2)
          val trans     =
                TranSet.union(TranSet.fromList
                              [(start, nil, start1),
                               (start, nil, start2)],
                              TranSet.union(trans1, trans2))
      in {stats     = stats,
          start     = start,
          accepting = accepting,
          trans     = trans}
      end

fun concat(fa1, fa2) =
      let fun renam1 a =
                Sym.fromTop
                (Sym.Compound[SOME(Sym.fromString "1"), NONE, SOME a])

          fun renam2 a =
                Sym.fromTop
                (Sym.Compound[SOME(Sym.fromString "2"), NONE, SOME a])

          val rel1 = SymRel.mlFunctionToFunction(renam1, states fa1)
          val rel2 = SymRel.mlFunctionToFunction(renam2, states fa2)

          val fa1        = renameStates(fa1, rel1)
          val stats1     = states fa1
          val start1     = startState fa1
          val accepting1 = acceptingStates fa1
          val trans1     = transitions fa1

          val fa2        = renameStates(fa2, rel2)
          val stats2     = states fa2
          val start2     = startState fa2
          val accepting2 = acceptingStates fa2
          val trans2     = transitions fa2

          val stats     = SymSet.union(stats1, stats2)
          val start     = start1
          val accepting = accepting2
          val linking   =
                TranSet.fromList 
                (map (fn q => (q, nil, start2))
                     (Set.toList accepting1))
          val trans     =
                TranSet.union(linking, TranSet.union(trans1, trans2))
      in {stats     = stats,
          start     = start,
          accepting = accepting,
          trans     = trans}
      end

fun closure fa =
      let fun renam a = Sym.fromTop(Sym.Compound[SOME a])

          val rel = SymRel.mlFunctionToFunction(renam, states fa)
          val fa  = renameStates(fa, rel)

          val stats     = states fa
          val start     = startState fa
          val accepting = acceptingStates fa
          val trans     = transitions fa

          val start'     = Sym.fromString "A"
          val stats'     = SymSet.union(Set.sing start', stats)
          val accepting' = Set.sing start'
          val looping'   =
                TranSet.fromList
                (map (fn q => (q, nil, start'))
                     (Set.toList accepting))
          val trans'     =
                TranSet.union(Set.sing(start', nil, start),
                              TranSet.union(looping', trans))
      in {stats     = stats',
          start     = start',
          accepting = accepting',
          trans     = trans'}
      end

fun genUnion nil         = emptySet
  | genUnion [fa]        = fa
  | genUnion (fa :: fas) = union(fa, genUnion fas)

fun genConcat nil         = emptyStr
  | genConcat [fa]        = fa
  | genConcat (fa :: fas) = concat(fa, genConcat fas)

fun rev fa =
      let fun renam a = Sym.fromTop(Sym.Compound[SOME a])

          val rel = SymRel.mlFunctionToFunction(renam, states fa)
          val fa  = renameStates(fa, rel)

          val stats     = states fa
          val start     = startState fa
          val accepting = acceptingStates fa
          val trans     = transitions fa

          fun revTran(a, x, b) = (b, List.rev x, a)

          val start'     = Sym.fromString "A"
          val stats'     = SymSet.union(Set.sing start', stats)
          val accepting' = Set.sing start
          val trans'     =
                TranSet.union
                (TranSet.map revTran trans,
                 TranSet.map (fn a => (start', nil, a)) accepting)
      in {stats     = stats',
          start     = start',
          accepting = accepting',
          trans     = trans'}
      end

(****************************** Other Functions ******************************)

fun fromStr x =
      {stats     = SymSet.fromList[Sym.fromString "A", Sym.fromString "B"],
       start     = Sym.fromString "A",
       accepting = Set.sing(Sym.fromString "B"),
       trans     = Set.sing(Sym.fromString "A", x, Sym.fromString "B")}

fun fromConcrReg Reg.EmptyStr                    = emptyStr
  | fromConcrReg Reg.EmptySet                    = emptySet
  | fromConcrReg (Reg.Sym a)                     = fromStr[a]
  | fromConcrReg (Reg.Closure reg)               = closure(fromConcrReg reg)
  | fromConcrReg (reg as Reg.Concat(reg1, reg2)) =
      (case Reg.split(Reg.fromConcr reg) of
            (bs,  NONE)      => fromStr bs
          | (nil, SOME _)    => concat(fromConcrReg reg1, fromConcrReg reg2)
          | (bs,  SOME reg') =>
              concat(fromStr bs, fromConcrReg(Reg.toConcr reg')))
  | fromConcrReg (Reg.Union(reg1, reg2))         =
      union(fromConcrReg reg1, fromConcrReg reg2)

fun fromReg reg = fromConcrReg(Reg.toConcr reg)

(*************************** Interface with JForlan ***************************)

fun jforlanNew() =
      let val file   = System.makeTempFile()
          val status = System.runJForlan("fa new " ^ file)
      in if OS.Process.isSuccess status
         then let val fa = input file
                  val _  = OS.FileSys.remove file
              in fa end
         else (OS.FileSys.remove file;
               M.errorString(fn () => ["creation", "of", "FA", "aborted"]))
      end

fun jforlanEdit fa =
      let val file   = System.makeTempFile()
          val _      = output(file, fa)
          val status = System.runJForlan("fa edit " ^ file)
      in if OS.Process.isSuccess status
         then let val fa = input file
                  val _  = OS.FileSys.remove file
              in fa end
         else (OS.FileSys.remove file;
               M.errorString(fn () => ["editing", "of", "FA", "aborted"]))
      end

(* if called with a string that isn't legal Forlan syntax for an FA,
   prints "error" on a line, followed by one or more lines of parsing
   errors

   if called with a string that is legal Forlan syntax for an FA,
   prints "valid" on a line, followed by a single line consisting of
   the FA in fully abbreviated form, but with no whitespace *)

fun jforlanValidate s =
      let val fa = Messages.quiet(fn () => fromString s)
      in print "valid"; print PP.newline;
         print(StringAux.removeWhiteSpace(toString fa));
         print PP.newline
      end
        handle _ =>
                 (print "error"; print PP.newline;
                  (fromString s; ())
                    handle _ => ())

(* when called with an FA in Forlan syntax, pretty prints on the
   standard output that FA in fully abbreviated form *)

fun jforlanPretty s = (print(toString(fromString s)); print PP.newline)

end;
