(********************************** lp.sml ***********************************)

(* Copyright (C) 2001-2012 Alley Stoughton

   The file is part of the Forlan toolset for experimenting with
   formal languages.  See the file COPYING.txt for copying and
   usage restrictions. *)

structure LP :> LP =
struct

structure M  = Messages
structure L  = Lex

(******************************** Basic Types ********************************)

datatype concr = Sym  of Sym.sym
               | Cons of Sym.sym * Str.str * concr

type lp = concr

fun fromConcr(concr : concr) : lp = concr

fun toConcr(lp : lp) : concr = lp

(*********************************** Input ***********************************)

fun inpLabPath lts =
      let val (q, lts) = Sym.inputFromLabToks lts
      in case lts of
              (_, L.Comma) :: lts =>
                let val (x, lts)  = Str.inputFromLabToks lts
                    val lts       = L.checkInLabToks(L.DoubArr, lts)
                    val (lp, lts) = inpLabPath lts
                in (Cons(q, x, lp), lts) end
            | _                   => (Sym q, lts)
      end

fun fromString s =
      case inpLabPath(L.lexString s) of
           (lp, [(_, L.EOF)]) => lp
         | (_,  nil)          => M.cannotHappen() 
         | (_,  lt :: _)      => L.unexpectedTok lt

fun input fil =
      case inpLabPath(L.lexFile fil) of
           (lp, [(_, L.EOF)]) => lp
         | (_,  nil)          => M.cannotHappen() 
         | (_,  lt :: _)      => L.unexpectedTok lt

(********************************** Output ***********************************)

fun labPathToPPList(Sym q)          = [Sym.toPP q]
  | labPathToPPList(Cons(q, x, lp)) =
      PP.decorate("",
                  PP.block(true,
                           [PP.comma(Sym.toPP q), Str.toPP x]),
                  " =>") ::
      labPathToPPList lp

fun toPP lp = PP.block(true, labPathToPPList lp)

fun toString lp = PP.toString(toPP lp)

fun output("",  lp) = (print(toString lp); print PP.newline)
  | output(fil, lp) =
      case SOME(TextIO.openOut fil) handle _ => NONE of
           NONE     =>
             M.errorPP
             (fn () =>
                   [PP.fromString "unable", PP.fromString "to",
                    PP.fromString "open", PP.fromString "file:",
                    PP.quote(PP.fromStringSplitEscape fil)])
         | SOME stm =>
             (TextIO.output(stm, toString lp);
              TextIO.output(stm, PP.newline);
              TextIO.closeOut stm)

(****************************** Other Functions ******************************)

fun compare(Sym a1,            Sym a2)            = Sym.compare(a1, a2)
  | compare(Sym _,             _)                 = LESS
  | compare(_,                 Sym _)             = GREATER
  | compare(Cons(a1, x1, lp1), Cons(a2, x2, lp2)) =
      case Sym.compare(a1, a2) of
           LESS    => LESS
         | EQUAL   =>
             (case Str.compare(x1, x2) of
                   LESS    => LESS
                 | EQUAL   => compare(lp1, lp2)
                 | GREATER => GREATER)
         | GREATER => GREATER

fun equal lpPair = compare lpPair = EQUAL

val sym = Sym

val cons = Cons

fun startState(Sym q)         = q
  | startState(Cons(q, _, _)) = q

fun endState(Sym q)          = q
  | endState(Cons(_, _, lp)) = endState lp

fun label(Sym _)           = nil
  | label(Cons(_, xs, lp)) = xs @ label lp

fun length(Sym _)          = 0
  | length(Cons(_, _, lp)) = 1 + length lp

fun join(Sym q,          lp)  =
      if Sym.equal(q, startState lp)
      then lp
      else M.errorString(fn () => ["incompatible", "labeled", "paths"])
  | join(Cons(q, x, lp), lp') = Cons(q, x, join(lp, lp'))

fun splitAt(lp, n) =
      let fun err() = M.errorString(fn () => ["integer", "out", "of", "range"])

          fun divAft(lp,             0) = (Sym(startState lp), lp)
            | divAft(Sym _,          _) = err()
            | divAft(Cons(q, x, lp), n) =
                let val (lp1, lp2) = divAft(lp, n - 1)
                in (Cons(q, x, lp1), lp2) end
      in if n < 0 then err() else divAft(lp, n) end

type pumping_division = lp * lp * lp

fun checkPumpingDivision(lp1, lp2, lp3) =
      if not(Sym.equal(endState lp1, startState lp2))
        then M.errorString
             (fn () =>
                   ["first", "two", "labeled", "paths", "are",
                    "incompatible"])
      else if not(Sym.equal(startState lp2, endState lp2))
        then M.errorString
             (fn () =>
                   ["middle", "labeled", "path", "has",
                    "different", "starting", "and", "ending",
                    "states"])
      else if not(Sym.equal(endState lp2, startState lp3))
         then M.errorString
              (fn () =>
                    ["last", "two", "labeled", "paths", "are",
                     "incompatible"])
      else if List.length(label lp2) = 0
         then M.errorString
              (fn () =>
                    ["label", "of", "middle", "labeled", "path", "is",
                     "empty"])
      else ()

fun validPumpingDivision pumpDiv =
      (M.quiet(fn () => checkPumpingDivision pumpDiv); true)
        handle _ => false

fun strsOfValidPumpingDivision(pumpDiv as (lp1, lp2, lp3)) =
      (checkPumpingDivision pumpDiv; (label lp1, label lp2, label lp3))

fun pumpValidPumpingDivision(pumpDiv as (lp1, lp2, lp3), n) =
      (checkPumpingDivision pumpDiv;
       if n < 0
       then M.errorString(fn () => ["negative", "argument"])
       else let fun pow 0 = sym(startState lp2)
                  | pow n = join(lp2, pow(n - 1))
            in join(lp1, join(pow n, lp3)) end)

fun findValidPumpingDivision lp =
      let val lookupSym = Tab.lookup Sym.compare
          val updateSym = Tab.update Sym.compare

          fun findRepetition(lp, n, tab) =
                let val q = startState lp
                in case lookupSym(tab, q) of
                        NONE   =>
                          (case lp of
                                Sym _           =>
                                  M.errorString
                                  (fn () => ["no", "state", "repetition"])
                              | Cons(_, _, lp') =>
                                  findRepetition(lp', n + 1,
                                                 updateSym(tab, [(q, n)])))
                      | SOME m => (m, n)
                end

          val (m, n)     = findRepetition(lp, 0, Tab.empty)
          val (lp', lp3) = splitAt(lp, n)
          val (lp1, lp2) = splitAt(lp', m)
      in if List.length(label lp2) = 0
         then M.errorString
              (fn () =>
                    ["label", "of", "middle", "labeled", "path", "of",
                     "found", "pumping", "division", "is", "empty"])
         else (lp1, lp2, lp3)
      end

fun findValidPumpingDivisionOpt lp =
      SOME(M.quiet(fn () => findValidPumpingDivision lp))
        handle _ => NONE

end;
