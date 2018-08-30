(********************************** rfa.sml **********************************)

(* Copyright (C) 2012 Alley Stoughton

   The file is part of the Forlan toolset for experimenting with
   formal languages.  See the file COPYING.txt for copying and
   usage restrictions. *)

structure RFA :> RFA =
struct

structure M  = Messages
structure L  = Lex

(*********************************** Types ***********************************)

type concr =
       {stats     : Sym.sym Set.set,
        start     : Sym.sym,
        accepting : Sym.sym Set.set,
        trans     : TranReg.tran_reg Set.set}
               
type rfa = concr

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

fun existsTranFromTo(q, r, nil)                  = false
  | existsTranFromTo(q, r, (q', _, r') :: trans) =
      Sym.equal(q, q') andalso Sym.equal(r, r') orelse
      existsTranFromTo(q, r, trans)

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
      else if existsTranFromTo(q, r, trans)
        then M.errorPP
             (fn () =>
                   [PP.fromString "there", PP.fromString "are",
                    PP.fromString "multiple", PP.fromString "transitions",
                    PP.fromString "from", PP.fromString "state",
                    PP.quote(Sym.toPP q), PP.fromString "to",
                    PP.fromString "state", PP.quote(Sym.toPP r)])
      else checkTransitions(trans, stats)
      
fun check{stats, start, accepting, trans} =
      (checkStart(start, stats);
       checkAccepting(Set.toList accepting, stats);
       checkTransitions(Set.toList trans, stats))

fun valid concr =
      (M.quiet(fn () => check concr); true)
        handle _ => false

fun fromConcr (concr : concr) : rfa = (check concr; concr)

fun toConcr(rfa : rfa) : concr = rfa

(*********************************** Input ***********************************)

fun inpRFA lts =
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
          val (trans, lts)     = TranRegSet.inputFromLabToks lts
          val concr              =
                {stats     = stats,
                 start     = start,
                 accepting = accepting,
                 trans     = trans}
      in (fromConcr concr, lts) end

fun fromString s =
      case inpRFA(L.lexString s) of
           (rfa, [(_, L.EOF)]) => rfa
         | (_,   nil)          => M.cannotHappen() 
         | (_,   lt :: _)      => L.unexpectedTok lt

fun input fil =
      case inpRFA(L.lexFile fil) of
           (rfa, [(_, L.EOF)]) => rfa
         | (_,   nil)          => M.cannotHappen() 
         | (_,   lt :: _)      => L.unexpectedTok lt

(*********************************** Output **********************************)

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
                          TranRegSet.toPP trans])])

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

(****************************** Other Functions ******************************)

fun states (rfa : rfa) = #stats rfa

fun startState (rfa : rfa) = #start rfa

fun acceptingStates (rfa : rfa) = #accepting rfa

fun transitions (rfa : rfa) = #trans rfa

fun compare(rfa1, rfa2) =
      case SymSet.compare(states rfa1, states rfa2) of
           LESS    => LESS
         | EQUAL   =>
             (case Sym.compare(startState rfa1, startState rfa2) of
                   LESS    => LESS
                 | EQUAL   =>
                     (case SymSet.compare(acceptingStates rfa1,
                                          acceptingStates rfa2) of
                           LESS    => LESS
                         | EQUAL   =>
                             TranRegSet.compare
                             (transitions rfa1, transitions rfa2)
                         | GREATER => GREATER)
                 | GREATER => GREATER)
         | GREATER => GREATER

fun equal rfaPair = compare rfaPair = EQUAL

fun numStates rfa = Set.size(states rfa)

fun numTransitions rfa = Set.size(transitions rfa)

fun alphabet rfa =
      SymSet.genUnion(Set.mapToList (fn (_, reg, _) => Reg.alphabet reg)
                                    (transitions rfa))

fun sub(rfa1, rfa2) =
      SymSet.subset(states rfa1, states rfa2)                   andalso
      Sym.equal(startState rfa1, startState rfa2)               andalso
      SymSet.subset(acceptingStates rfa1, acceptingStates rfa2) andalso
      TranRegSet.subset(transitions rfa1, transitions rfa2)      

fun validStatesRenaming(rfa, rel) =
      let val stats = states rfa
      in SymRel.bijectionFromAvoiding(rel, stats, Set.empty) end

fun renameStates(rfa as {stats, start, accepting, trans}, rel) =
      if validStatesRenaming(rfa, rel)
      then let val renam      = SymRel.applyFunction rel
               val stats'     = SymSet.map renam stats
               val start'     = renam start
               val accepting' = SymSet.map renam accepting
               val trans'     =
                     TranRegSet.map
                     (fn (q, reg, r) => (renam q, reg, renam r))
                     trans
           in {stats     = stats',
               start     = start',
               accepting = accepting',
               trans     = trans'}
           end
      else M.errorString
           (fn () => ["invalid", "states", "renaming", "for", "RFA"])

fun renameStatesCanonically rfa =
      let val stats = states rfa

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
      in renameStates(rfa, rel) end

fun checkLP(memb, fa) =
      let val stats = states fa
          val trans = transitions fa

          fun findTran(_, _, nil)                    = NONE
            | findTran(q, r, (q', reg, r') :: trans) =
                if Sym.equal(q, q') andalso Sym.equal(r, r')
                then SOME reg
                else findTran(q, r, trans)

          fun missing(q, x, r) =
                M.errorPP
                (fn () =>
                      [PP.fromString "missing", PP.fromString "transition",
                       PP.fromString "from", PP.quote(Sym.toPP q),
                       PP.fromString "to", PP.quote(Sym.toPP r),
                       PP.fromString "whose", PP.fromString "regular",
                       PP.fromString "expression", PP.fromString "generates",
                       PP.quote(Str.toPP x)])
              
          fun wrong(q, x, r) =
                M.errorPP
                (fn () =>
                      [PP.fromString "transition", PP.fromString "from",
                       PP.quote(Sym.toPP q), PP.fromString "to",
                       PP.quote(Sym.toPP r), PP.fromString "has",
                       PP.fromString "regular", PP.fromString "expression",
                       PP.fromString "that", PP.fromString "doesn't",
                       PP.fromString "generate", PP.quote(Str.toPP x)])
              
          fun check(LP.Sym q)          =
                if SymSet.memb(q, stats)
                then ()
                else M.errorPP
                     (fn () =>
                           [PP.fromString "invalid", PP.fromString "state:",
                            PP.quote(Sym.toPP q)])
            | check(LP.Cons(q, x, lp)) =
                let val r = LP.startState(LP.fromConcr lp)
                in case findTran(q, r, Set.toList trans) of
                        NONE     => missing(q, x, r)
                      | SOME reg =>
                          (if memb(x, reg)
                           then check lp
                           else wrong(q, x, r))
                end
      in fn lp => check(LP.toConcr lp) end

fun validLP(memb, rfa) =
      let val checkLP = checkLP(memb, rfa)
      in fn lp =>
              (M.quiet(fn () => checkLP lp); true)
                handle _ => false
      end

fun standard rfa =
      let val stats     = states rfa
          val start     = startState rfa
          val accepting = acceptingStates rfa
          val trans     = transitions rfa
      in not(SymSet.memb(start, accepting))                             andalso
         Set.size accepting = 1                                         andalso
         not
         (Set.exists (fn (_, _, r) => Sym.equal(r, start)) trans)       andalso
         not
         (Set.exists (fn (q, _, _) => SymSet.memb(q, accepting)) trans)
      end

fun standardize rfa =
      let fun renam a = Sym.fromTop(Sym.Compound[SOME a])

          val rel = SymRel.mlFunctionToFunction(renam, states rfa)
          val rfa = renameStates(rfa, rel)

          val stats     = states rfa
          val start     = startState rfa
          val accepting = acceptingStates rfa
          val trans     = transitions rfa

          val start'     = Sym.fromString "A"
          val acc'       = Sym.fromString "B"
          val stats'     =
              SymSet.union
              (Set.sing start',
               SymSet.union(Set.sing acc', stats))
          val accepting' = Set.sing acc'
          val finals     =
                TranRegSet.map
                (fn q => (q, Reg.emptyStr, acc'))
                accepting
          val trans'     =
                TranRegSet.union
                (Set.sing(start', Reg.emptyStr, start),
                 TranRegSet.union(trans, finals))
      in {stats     = stats',
          start     = start',
          accepting = accepting',
          trans     = trans'}
      end

fun combine(simp, trans) =
      let fun altCompare((q, reg, r), (q', reg', r')) =
                case Sym.compare(q, q') of
                     LESS    => LESS
                   | EQUAL   =>
                       (case Sym.compare(r, r') of
                             LESS    => LESS
                           | EQUAL   => Reg.compare(reg, reg')
                           | GREATER => GREATER)
                   | GREATER => GREATER

          val trans = Sort.sort (false, altCompare) (Set.toList trans)

          fun comb(nil,                                                _)     =
                nil
            | comb([(q, reg, r)],                                      reg's) =
                [(q,
                  simp
                  (Reg.genUnion
                   (Set.toList
                    (Set.union Reg.compare (Set.sing reg, reg's)))),
                  r)]
            | comb((q, reg, r) :: (trans' as (q', reg', r') :: trans), reg''s) =
                if Sym.equal(q, q') andalso Sym.equal(r, r')
                then comb(trans', Set.union Reg.compare (Set.sing reg, reg''s))
                else (q,
                      simp
                      (Reg.genUnion
                       (Set.toList
                        (Set.union Reg.compare (Set.sing reg, reg''s)))),
                      r) ::
                     comb(trans', Set.empty)
      in TranRegSet.fromList(comb(trans, Set.empty)) end

fun fromFA simp fa =
      let val stats     = FA.states fa
          val start     = FA.startState fa
          val accepting = FA.acceptingStates fa
          val trans     = FA.transitions fa
          val trans'    =
                combine(simp,
                        TranRegSet.map (fn (q, x, r) => (q, Reg.fromStr x, r))
                                       trans)
      in {stats     = stats,
          start     = start,
          accepting = accepting,
          trans     = trans'}
      end

(* transInvolving(rfa, p) returns a partitioning of the transitions of
   rfa (self, into, from, other), where self are the transitions from
   p to p (at most one), into are the non-self transitions that lead into
   p, from are the non-self transitions that go from p, and other
   are the remaining transitions *)

fun transInvolving(rfa, p) =
      let fun loop(self, into, from, other, nil)                  =
                (self, into, from, other)
            | loop(self, into, from, other, (q, reg, r) :: trans) =
                if Sym.equal(p, q) andalso Sym.equal(p, r)
                  then loop(Set.sing(p, reg, p), into, from, other, trans)
                else if Sym.equal(p, r)
                  then loop(self,
                            TranRegSet.union(Set.sing(q, reg, r), into),
                            from, other, trans)
                else if Sym.equal(p, q)
                  then loop(self, into,
                            TranRegSet.union(Set.sing(q, reg, r), from),
                            other, trans)
                else loop(self, into, from,
                          TranRegSet.union(Set.sing(q, reg, r), other),
                          trans)
      in loop(Set.empty, Set.empty, Set.empty, Set.empty,
              Set.toList(transitions rfa))
      end

(* p must be a state of rfa that isn't its start state or an accepting
   state *)

fun elimState(simp, rfa, p) =
      let val stats     = states rfa
          val start     = startState rfa
          val accepting = acceptingStates rfa
          val trans     = transitions rfa

          val (self, into, from, other) = transInvolving(rfa, p)
          val selfReg                   =
                case Set.toList self of
                     nil           => Reg.emptyStr
                   | [(_, reg, _)] => reg
                   | _             => M.cannotHappen()

          fun join((q, reg, _), (_, reg', r)) =
                let val reg'' =
                          simp
                          (Reg.concat
                           (reg,
                            Reg.concat(Reg.closure selfReg, reg')))
                in (q, simp reg'', r) end

          val trans' =
                combine
                (simp,
                 TranRegSet.union
                 (other,
                  TranRegSet.map join (Set.times(into, from))))
      in {stats     = SymSet.minus(stats, Set.sing p),
          start     = start,
          accepting = accepting,
          trans     = trans'}
      end

fun eliminateState simp (rfa, p) =
      let val stats     = states rfa
          val start     = startState rfa
          val accepting = acceptingStates rfa
      in if not(SymSet.memb(p, stats))
           then M.errorPP
                (fn () =>
                      [PP.fromString "invalid", PP.fromString "state:",
                       PP.quote(Sym.toPP p)])
         else if Sym.equal(p, start)
           then M.errorPP
                (fn () =>
                      [PP.fromString "cannot", PP.fromString "eliminate",
                       PP.fromString "start", PP.fromString "state:",
                       PP.quote(Sym.toPP p)])
         else if SymSet.memb(p, accepting)
           then M.errorPP
                (fn () =>
                      [PP.fromString "cannot", PP.fromString "eliminate",
                       PP.fromString "accepting", PP.fromString "state:",
                       PP.quote(Sym.toPP p)])
         else elimState(simp, rfa, p)
      end

fun toReg simp rfa =
      let (* in a call to loop, rfa is standard *)

          fun loop rfa =
                if numStates rfa = 2
                then case Set.toList(transitions rfa) of
                          [(_, reg, _)] => reg
                        | _             => Reg.emptySet
                else let val choices =
                               SymSet.minus
                               (states rfa,
                                SymSet.union(Set.sing(startState rfa),
                                             acceptingStates rfa))
                         val p       = List.hd(Set.toList choices)
                     in loop(elimState(simp, rfa, p)) end
      in loop(standardize rfa) end

fun faToReg simp = toReg simp o fromFA simp

(* in a call faToRegPermsCommon(nOpt, simp, fa), nOpt is NONE
   (infinite) or positive (SOME n for n >= 1) *)

fun faToRegPermsCommon(nOpt, simp, fa) =
      let fun decr NONE     = NONE
            | decr (SOME n) = SOME(n - 1)

          fun simplest(regOpt, reg') =
                case regOpt of
                     NONE     => reg'
                   | SOME reg =>
                       if Reg.compareComplexityTotal(reg', reg) = LESS
                       then reg'
                       else reg

          (* val find :
                   int option * Reg.reg option * SymRel.sym_rel * Sym.sym list *
                   Sym.sym list ->
                   int option * Reg.reg

             in a call find(nOpt, regOpt, rel, qs, rs):

               nOpt is NONE or SOME n, for n >= 1;

               Set.toList(SymRel.domain rel) @ qs = Set.toList(FA.states fa)
               (hence qs is in strictly ascending order);

               rs is in strictly ascending order;

               SymRel.range rel and SymSet.fromList rs are disjoint
               and when unioned together form FA.states fa;

               (hence length qs = length rs)

             find(NONE, regOpt, rel, qs, rs) works through all of the
             bijections rel' over FA.states fa that are supersets of
             rel, in order, computing each faToReg simp
             (FA.renameStates(fa, rel')); at each stage, it keeps
             track of the simplest regular expression found so far
             (ties are broken by ordering the candidates using our
             total ordering on regular expressions, and selecting the
             smallest element); initially, this is the one in regOpt,
             if any; it returns (NONE, reg'), where reg' is the
             current regular expression when the bijections have been
             exhausted

             find(SOME n, regOpt, rel, qs, rs) works as in the previous
             case, except that at most n bijections are generated and
             used; if n >= the number m of bijections over FA.states fa
             that are supersets of rel, then (SOME(n - m), reg') is
             returned; otherwise, the user is told that the process
             was curtailed, and (SOME ~1, reg') is returned *)

          fun find(nOpt, regOpt, rel, nil,     _)  =
                let val _   =
                          M.messagePP
                          (fn () =>
                                [PP.fromString "using",
                                 PP.fromString "renaming",
                                 PP.quote(SymRel.toPP rel)])
                    val reg =
                          faToReg simp (FA.renameStates(fa, rel))
                    val _   =
                          M.messagePP
                          (fn () =>
                                [PP.fromString "found",
                                 PP.fromString "regular",
                                 PP.fromString "expression",
                                 PP.quote(Reg.toPP reg)])
                    val reg = simplest(regOpt, reg)
                    val _   =
                          M.messagePP
                          (fn () =>
                              [PP.fromString "simplest",
                               PP.fromString "regular",
                               PP.fromString "expression",
                               PP.fromString "so",
                               PP.fromString "far",
                               PP.fromString "is",
                               PP.quote(Reg.toPP reg)])
                in (decr nOpt, reg) end
            | find(nOpt, regOpt, rel, q :: qs, rs) =
                let (* val loop :
                             int option * Reg.reg option * Sym.sym list *
                             Sym.sym list ->
                             int option * Reg.reg

                       in a call loop(nOpt, regOpt, r''s, r's):

                         nOpt = NONE or nOpt = SOME n, for n >= 1;

                         rev r''s @ r's = rs;

                         null r's => regOpt <> NONE

                       loop(nOpt, regOpt, r''s, r's) is like
                       find(nOpt, regOpt, rel, q :: qs, rs) except that
                       the bijections considered must map q to an
                       element of r's *)

                    fun loop(nOpt, regOpt, _,    nil)       =
                          (nOpt, valOf regOpt)
                      | loop(nOpt, regOpt, r''s, r' :: r's) =
                          let val (nOpt, reg) =
                                    find(nOpt, regOpt,
                                         SymRel.union(rel,
                                                      Set.sing(q, r')),
                                         qs, rev r''s @ r's)
                          in if case nOpt of
                                     NONE   => false
                                   | SOME n => n <= 0
                             then case nOpt of
                                       SOME n =>
                                         if n = 0
                                         then if null r's
                                              then (nOpt, reg)
                                              else (M.messageString
                                                    (fn () => ["curtailed"]);
                                                    (SOME ~1, reg))
                                         else (nOpt, reg)
                                     | _      => M.cannotHappen()
                             else loop(nOpt, SOME reg, r' :: r''s, r's)
                          end
                in loop(nOpt, regOpt, nil, rs) end

          val (_, reg) =
                find(nOpt, NONE, Set.empty, Set.toList(FA.states fa),
                     Set.toList(FA.states fa))
      in reg end

fun faToRegPermsTrace(nOpt, simp) =
      if case nOpt of
              NONE   => false
            | SOME n => n < 1
      then M.errorString
           (fn () =>
                 ["limit", "on", "number", "of", "renamings", "must", "be",
                  "at", "least", "1"])
      else fn fa => faToRegPermsCommon(nOpt, simp, fa)

fun faToRegPerms(nOpt, simp) =
      if case nOpt of
              NONE   => false
            | SOME n => n < 1
      then M.errorString
           (fn () =>
                 ["limit", "on", "number", "of", "renamings", "must", "be",
                  "at", "least", "1"])
      else fn fa => M.quiet(fn () => faToRegPermsCommon(nOpt, simp, fa))

(*************************** Interface with JForlan ***************************)

fun jforlanNew() =
      let val file   = System.makeTempFile()
          val status = System.runJForlan("rfa new " ^ file)
      in if OS.Process.isSuccess status
         then let val rfa = input file
                  val _   = OS.FileSys.remove file
              in rfa end
         else (OS.FileSys.remove file;
               M.errorString(fn () => ["creation", "of", "RFA", "aborted"]))
      end

fun jforlanEdit rfa =
      let val file   = System.makeTempFile()
          val _      = output(file, rfa)
          val status = System.runJForlan("rfa edit " ^ file)
      in if OS.Process.isSuccess status
         then let val rfa = input file
                  val _   = OS.FileSys.remove file
              in rfa end
         else (OS.FileSys.remove file;
               M.errorString(fn () => ["editing", "of", "RFA", "aborted"]))
      end

(* if called with a string that isn't legal Forlan syntax for an RFA,
   prints "error" on a line, followed by one or more lines of parsing
   errors

   if called with a string that is legal Forlan syntax for an RFA,
   prints "valid" on a line, followed by a single line consisting of
   the RFA in fully abbreviated form, but with no whitespace *)

fun jforlanValidate s =
      let val rfa = Messages.quiet(fn () => fromString s)
      in print "valid"; print PP.newline;
         print(StringAux.removeWhiteSpace(toString rfa));
         print PP.newline
      end
        handle _ =>
                 (print "error"; print PP.newline;
                  (fromString s; ())
                    handle _ => ())

(* when called with an RFA in Forlan syntax, pretty prints on the
   standard output that RFA in fully abbreviated form *)

fun jforlanPretty s = (print(toString(fromString s)); print PP.newline)

end;
