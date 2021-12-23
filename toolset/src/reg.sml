(********************************** reg.sml **********************************)

(* Copyright (C) 2001-2012 Alley Stoughton

   The file is part of the Forlan toolset for experimenting with
   formal languages.  See the file COPYING.txt for copying and
   usage restrictions. *)

structure Reg :> REG =
struct

structure M  = Messages
structure L  = Lex
structure LA = ListAux

(******************************** Main Types *********************************)

datatype concr =
             EmptyStr
           | EmptySet
           | Sym      of Sym.sym
           | Closure  of concr
           | Concat   of concr * concr
           | Union    of concr * concr

type reg = concr

fun fromConcr(concr : concr) : reg = concr

fun toConcr(reg : reg) : concr = reg

(*********************************** Input ***********************************)

fun inpRegExp0 lts =
      let val (reg1, lts) = inpRegExp1 lts
      in case lts of
              (_, L.Plus) :: lts =>
                let val (reg2, lts) = inpRegExp0 lts
                in (Union(reg1, reg2), lts) end
            | _                  => (reg1, lts)
      end

and inpRegExp1 lts =
      let val (reg1, lts) = inpRegExp2 lts
      in if case lts of
                 (_, L.Dollar) :: _  => true
               | (_, L.Perc) :: _    => true
               | (_, L.Sym _) :: _   => true
               | (_, L.OpenPar) :: _ => true
               | _                   => false
         then let val (reg2, lts) = inpRegExp1 lts
              in (Concat(reg1, reg2), lts) end
         else (reg1, lts)
      end

and inpRegExp2 lts =
      let fun loop(reg, lts) =
                case lts of
                     (_, L.Star) :: lts => loop(Closure reg, lts)
                   | _                  => (reg, lts)
      in loop(inpRegExp3 lts) end

and inpRegExp3 nil                     = Lex.errorNotEOFTerminated()
  | inpRegExp3 ((_, L.Dollar) :: lts)  = (EmptySet, lts)
  | inpRegExp3 ((_, L.Perc) :: lts)    = (EmptyStr, lts)
  | inpRegExp3 ((_, L.Sym a) :: lts)   = (Sym a, lts)
  | inpRegExp3 ((_, L.OpenPar) :: lts) =
      let val (reg, lts) = inpRegExp0 lts
      in case lts of
              nil                   => Lex.errorNotEOFTerminated()
            | (_, L.ClosPar) :: lts => (reg, lts)
            | lt :: _               => L.unexpectedTok lt
      end
  | inpRegExp3 (lt :: _)               = L.unexpectedTok lt

val inputFromLabToks = inpRegExp0

fun fromString s =
      case inpRegExp0(L.lexString s) of
           (reg, [(_, L.EOF)]) => reg
         | (_,   nil)          => M.cannotHappen() 
         | (_,   lt :: _)      => L.unexpectedTok lt

fun input fil =
      case inpRegExp0(L.lexFile fil) of
           (reg, [(_, L.EOF)]) => reg
         | (_,   nil)          => M.cannotHappen() 
         | (_,   lt :: _)      => L.unexpectedTok lt

(*********************************** Output **********************************)

fun regToPP0 reg = PP.block(true, regToPPList0 reg)

and regToPPList0 (Union(reg1, reg2)) =
      PP.decorate("", regToPP1 reg1, " +") :: regToPPList0 reg2
  | regToPPList0 reg                 = [regToPP1 reg]

and regToPP1 reg = PP.block(false, regToPPList1 reg)

and regToPPList1 (Concat(reg1, reg2)) = regToPP2 reg1 :: regToPPList1 reg2
  | regToPPList1 reg                  = [regToPP2 reg]

and regToPP2 (Closure reg) = PP.decorate("", regToPP2 reg, "*")
  | regToPP2 reg           = regToPP3 reg

and regToPP3 EmptyStr = PP.fromString "%"
  | regToPP3 EmptySet = PP.fromString "$"
  | regToPP3 (Sym a)  = Sym.toPP a
  | regToPP3 reg      = PP.decorate("(", regToPP0 reg, ")")

val toPP = regToPP0

fun toString reg = PP.toString(toPP reg)

fun output("",  reg) = (print(toString reg); print PP.newline)
  | output(fil, reg) =
      case SOME(TextIO.openOut fil) handle _ => NONE of
           NONE     =>
             M.errorPP
             (fn () =>
                   [PP.fromString "unable", PP.fromString "to",
                    PP.fromString "open", PP.fromString "file:",
                    PP.quote(PP.fromStringSplitEscape fil)])
         | SOME stm =>
             (TextIO.output(stm, toString reg);
              TextIO.output(stm, PP.newline);
              TextIO.closeOut stm)

(******************************* Tree Functions *******************************)

fun validPath(_,              nil)     = true
  | validPath(Closure reg,    1 :: ns) = validPath(reg, ns)
  | validPath(Concat(reg, _), 1 :: ns) = validPath(reg, ns)
  | validPath(Concat(_, reg), 2 :: ns) = validPath(reg, ns)
  | validPath(Union(reg, _),  1 :: ns) = validPath(reg, ns)
  | validPath(Union(_, reg),  2 :: ns) = validPath(reg, ns)
  | validPath(_,              _ :: _)  = false

fun height EmptyStr             = 0
  | height EmptySet             = 0
  | height (Sym _)              = 0
  | height (Closure reg)        = 1 + height reg
  | height (Concat(reg1, reg2)) = 1 + Int.max(height reg1, height reg2)
  | height (Union(reg1, reg2))  = 1 + Int.max(height reg1, height reg2)

fun size EmptyStr             = 1
  | size EmptySet             = 1
  | size (Sym _)              = 1
  | size (Closure reg)        = 1 + size reg
  | size (Concat(reg1, reg2)) = 1 + size reg1 + size reg2
  | size (Union(reg1, reg2))  = 1 + size reg1 + size reg2

fun numLeaves EmptyStr             = 1
  | numLeaves EmptySet             = 1
  | numLeaves (Sym _)              = 1
  | numLeaves (Closure reg)        = numLeaves reg
  | numLeaves (Concat(reg1, reg2)) = numLeaves reg1 + numLeaves reg2
  | numLeaves (Union(reg1, reg2))  = numLeaves reg1 + numLeaves reg2

fun select (reg,            nil)     = reg
  | select (Closure reg,    1 :: ns) = select(reg, ns)
  | select (Concat(reg, _), 1 :: ns) = select(reg, ns)
  | select (Concat(_, reg), 2 :: ns) = select(reg, ns)
  | select (Union(reg, _),  1 :: ns) = select(reg, ns)
  | select (Union(_, reg),  2 :: ns) = select(reg, ns)
  | select _                         =
      M.errorString
      (fn () => ["invalid", "path", "for", "regular", "expression"])

fun update (_,                  nil,     reg') = reg'
  | update (Closure reg,        1 :: ns, reg') = Closure(update(reg, ns, reg'))
  | update (Concat(reg1, reg2), 1 :: ns, reg') =
      Concat(update(reg1, ns, reg'), reg2)
  | update (Concat(reg1, reg2), 2 :: ns, reg') =
      Concat(reg1, update(reg2, ns, reg'))
  | update (Union(reg1, reg2),  1 :: ns, reg') =
      Union(update(reg1, ns, reg'), reg2)
  | update (Union(reg1, reg2),  2 :: ns, reg') =
      Union(reg1, update(reg2, ns, reg'))
  | update _                                   =
      M.errorString
      (fn () => ["invalid", "path", "for", "regular", "expression"])

fun maximumLengthPath (Closure reg)       = 1 :: maximumLengthPath reg
  | maximumLengthPath(Concat(reg1, reg2)) =
      if height reg1 >= height reg2
      then 1 :: maximumLengthPath reg1
      else 2 :: maximumLengthPath reg2
  | maximumLengthPath (Union(reg1, reg2)) =
      if height reg1 >= height reg2
      then 1 :: maximumLengthPath reg1
      else 2 :: maximumLengthPath reg2
  | maximumLengthPath _                   = nil

fun validLeafPath(reg, ns) =
      validPath(reg, ns) andalso
      case select(reg, ns) of
           EmptyStr => true
         | EmptySet => true
         | Sym _    => true
         | _        => false

(****************************** Other Functions ******************************)

val emptyStr = EmptyStr

val emptySet = EmptySet

val fromSym = Sym

val closure = Closure

val concat = Concat

val union = Union

fun isEmptyStr EmptyStr = true
  | isEmptyStr _        = false

fun isEmptySet EmptySet = true
  | isEmptySet _        = false

fun isSym (Sym _) = true
  | isSym _       = false

fun isClosure (Closure _) = true
  | isClosure _           = false

fun isConcat (Concat _) = true
  | isConcat _          = false

fun isUnion (Union _) = true
  | isUnion _         = false

(* union must be the largest kind, so that unions are strictly greater
   than all other kinds of expressions, otherwise an expression like 0
   + (1 + 2) can't be turned into a standardized simplified one;
   otherwise, we just want a total ordering *)

fun kind EmptyStr    = 0
  | kind EmptySet    = 1
  | kind (Sym _)     = 2
  | kind (Closure _) = 3
  | kind (Concat _)  = 4
  | kind (Union _)   = 5

fun compare(reg, reg') =
      case Int.compare(kind reg, kind reg') of
           LESS    => LESS
         | EQUAL   =>
             (case (reg, reg') of
                   (Sym a,              Sym a')               =>
                     Sym.compare(a, a')
                 | (Closure reg,        Closure reg')         =>
                     compare(reg, reg')
                 | (Concat(reg1, reg2), Concat(reg1', reg2')) =>
                     (case compare(reg1, reg1') of
                           LESS    => LESS
                         | EQUAL   => compare(reg2, reg2')
                         | GREATER => GREATER)
                 | (Union(reg1, reg2),  Union(reg1', reg2'))  =>
                     (case compare(reg1, reg1') of
                           LESS    => LESS
                         | EQUAL   => compare(reg2, reg2')
                         | GREATER => GREATER)
                 | _                                          => EQUAL)
         | GREATER => GREATER

fun equal(reg, reg') = compare(reg, reg') = EQUAL

fun fromStr nil       = EmptyStr
  | fromStr [b]       = Sym b
  | fromStr (b :: bs) = Concat(Sym b, fromStr bs)

fun power(reg, n) =
      if n < 0
      then M.errorString(fn () => ["negative", "argument"])
      else let fun pow 0 = EmptyStr
                 | pow 1 = reg
                 | pow n = Concat(reg, pow(n - 1))
           in pow n end

fun alphabet EmptyStr             = Set.empty
  | alphabet EmptySet             = Set.empty
  | alphabet (Sym a)              = Set.sing a
  | alphabet (Closure reg)        = alphabet reg
  | alphabet (Concat(reg1, reg2)) = SymSet.union(alphabet reg1, alphabet reg2)
  | alphabet (Union(reg1, reg2))  = SymSet.union(alphabet reg1, alphabet reg2)

local
  fun splt (Sym a)              = ([a], NONE)
    | splt (Concat(Sym a, reg)) =
        let val (bs, regOpt) = splt reg
        in (a :: bs, regOpt) end
    | splt reg                  = (nil, SOME reg)
in
  fun split EmptyStr = (nil, NONE)
    | split reg      = splt reg
end

fun genConcat nil           = EmptyStr
  | genConcat [reg]         = reg
  | genConcat (reg :: regs) = Concat(reg, genConcat regs)

fun genUnion nil           = EmptySet
  | genUnion [reg]         = reg
  | genUnion (reg :: regs) = Union(reg, genUnion regs)

fun rightConcat(Concat(reg1, reg2), reg3) =
      Concat(reg1, rightConcat(reg2, reg3))
  | rightConcat(reg1, reg2)               = Concat(reg1, reg2)

fun rightUnion(Union(reg1, reg2), reg3) =
      Union(reg1, rightUnion(reg2, reg3))
  | rightUnion(reg1, reg2)              = Union(reg1, reg2)

fun concatsToList (Concat(reg1, reg2)) = reg1 :: concatsToList reg2
  | concatsToList reg                  = [reg]

fun unionsToList (Union(reg1, reg2)) = reg1 :: unionsToList reg2
  | unionsToList reg                 = [reg]

fun sortUnions reg =
      genUnion(Set.toList(Set.fromList compare (unionsToList reg)))

fun allSym bs = genUnion(map fromSym (Set.toList bs))

val allStr = Closure o allSym

fun fromStrSet xs = genUnion(map fromStr (Set.toList xs))

(******************************* Simplification *******************************)

(* regular expression complexity *)

(* a CLOSURE COMPLEXITY (CC) is a nonempty list of natural numbers
   that is sorted in (not necessarily strictly) descending order *)

type cc = int list

fun ccToList cc = cc

fun singCC n =
      if n >= 0
      then [n]
      else M.errorString(fn () => ["argument", "is", "negative"])

local
  fun intCompareRev(n, m) =
        case Int.compare(n, m) of
             LESS    => GREATER
           | EQUAL   => EQUAL
           | GREATER => LESS
in
  fun unionCC(xs, ys) = Sort.merge (true, intCompareRev) (xs, ys)
end

val succCC = map(fn n => n + 1)

(* The CLOSURE COMPLEXITY of reg is cc reg *)

fun cc EmptyStr             = [0]
  | cc EmptySet             = [0]
  | cc (Sym _)              = [0]
  | cc (Closure reg)        = succCC(cc reg)
  | cc (Concat(reg1, reg2)) = unionCC(cc reg1, cc reg2)
  | cc (Union(reg1, reg2))  = unionCC(cc reg1, cc reg2)

(* We say that:

   * reg1 has LOWER CLOSURE COMPLEXITY THAN reg2 iff
     compareCC(cc reg1, cc reg2) = LESS;

   * reg1 has EQUAL CLOSURE COMPLEXITY TO reg2 iff
     compareCC(cc reg1, cc reg2) = EQUAL;

   * reg1 has GREATER CLOSURE COMPLEXITY THAN reg2 iff
     compareCC(cc reg1, cc reg2) = GREATER; and

   * reg1 has NO GREATER CLOSURE COMPLEXITY THAN reg2 iff
     compareCC(cc reg1, cc reg2) <> GREATER *)

fun compareCC(nil,     nil)     = EQUAL
  | compareCC(_,       nil)     = GREATER
  | compareCC(nil,     _)       = LESS
  | compareCC(x :: xs, y :: ys) =
      case Int.compare(x, y) of
           LESS    => LESS
         | EQUAL   => compareCC(xs, ys)
         | GREATER => GREATER

fun numConcats EmptyStr             = 0
  | numConcats EmptySet             = 0
  | numConcats (Sym _)              = 0
  | numConcats (Closure reg)        = numConcats reg
  | numConcats (Concat(reg1, reg2)) = 1 + numConcats reg1 + numConcats reg2
  | numConcats (Union(reg1, reg2))  = numConcats reg1 + numConcats reg2

fun numSyms EmptyStr             = 0
  | numSyms EmptySet             = 0
  | numSyms (Sym _)              = 1
  | numSyms (Closure reg)        = numSyms reg
  | numSyms (Concat(reg1, reg2)) = numSyms reg1 + numSyms reg2
  | numSyms (Union(reg1, reg2))  = numSyms reg1 + numSyms reg2

local
  fun shift1 (Concat(Closure reg1, reg2)) = equal(reg1, reg2)
    | shift1 _                            = false

  fun shift2 (Concat(Closure reg1, Concat(reg2, reg3))) = equal(reg1, reg2)
    | shift2 _                                          = false

  fun shift3 (Concat(Closure(Concat(reg1, reg2)), reg3)) = equal(reg1, reg3)
    | shift3 _                                           = false

  fun shift4 (Concat(Closure(Concat(reg1, reg2)), Concat(reg3, reg4))) =
        equal(reg1, reg3)
    | shift4 _                                                         = false
in
  fun noShiftableClosuresAtTop reg =
        not(shift1 reg) andalso
        not(shift2 reg) andalso
        not(shift3 reg) andalso
        not(shift4 reg)
end

fun standardized EmptyStr                    = true
  | standardized EmptySet                    = true
  | standardized (Sym _)                     = true
  | standardized (Closure reg)               = standardized reg
  | standardized (reg as Concat(reg1, reg2)) =
      standardized reg1       andalso
      standardized reg2       andalso
      (case reg1 of
            Concat _ => false
          | _        => true) andalso
      noShiftableClosuresAtTop reg
  | standardized (Union(reg1, reg2))         =
      standardized reg1                  andalso
      standardized reg2                  andalso
      (case reg1 of
            Union _ => false
          | _       => true)             andalso
      (case reg2 of
            Union(reg2_1, reg2_2) =>
              not(compare(reg1, reg2_1) = GREATER)
          | _                     =>
              not(compare(reg1, reg2) = GREATER))

(* compareComplexity(reg, reg') compares the complexities of reg and
   reg': LESS means that reg is strictly simpler (less complex) than
   reg'; EQUAL means that reg and reg' are equally simple/complex;
   GREATER means reg is strictly more complex (less simple) than reg' *)

fun compareComplexity(reg, reg') =
      case compareCC(cc reg, cc reg') of
           LESS    => LESS
         | EQUAL   =>
             (case Int.compare(size reg, size reg') of
                   LESS    => LESS
                 | EQUAL   =>
                     (case Int.compare(numConcats reg, numConcats reg') of
                           LESS    => LESS
                         | EQUAL   =>
                             (case Int.compare(numSyms reg, numSyms reg') of
                                   LESS    => LESS
                                 | EQUAL   =>
                                     if standardized reg
                                     then if standardized reg'
                                          then EQUAL
                                          else LESS
                                     else if standardized reg'
                                          then GREATER
                                          else EQUAL
                                 | GREATER => GREATER)
                         | GREATER => GREATER)
                 | GREATER => GREATER)
         | GREATER => GREATER

fun compareComplexityTotal(reg, reg') =
      case compareComplexity(reg, reg') of
           LESS    => LESS
         | EQUAL   => compare(reg, reg')
         | GREATER => GREATER

(* weak simplification *)

fun weaklySimplified EmptyStr                    = true
  | weaklySimplified EmptySet                    = true
  | weaklySimplified (Sym _)                     = true
  | weaklySimplified (Closure reg)               =
      (case reg of
            EmptyStr  => false
          | EmptySet  => false
          | Closure _ => false
          | _         => weaklySimplified reg)
  | weaklySimplified (reg as Concat(reg1, reg2)) =
      (case reg1 of
            EmptyStr => false
          | EmptySet => false
          | Concat _ => false
          | _        =>
              (case reg2 of
                    EmptyStr => false
                  | EmptySet => false
                  | _        =>
                      weaklySimplified reg1 andalso
                      weaklySimplified reg2 andalso
                      noShiftableClosuresAtTop reg))
  | weaklySimplified (Union(reg1, reg2))         =
      (case reg1 of
            EmptySet => false
          | Union _  => false
          | _        =>
              (case reg2 of
                    EmptySet              => false
                  | Union(reg2_1, reg2_2) =>
                      compare(reg1, reg2_1) = LESS
                  | _                     =>
                      compare(reg1, reg2) = LESS) andalso
              weaklySimplified reg1               andalso
              weaklySimplified reg2)

(* reg is EmptyStr or EmptySet, or all elements of concatsToList reg
   are weakly simplified, are not EmptyStr or EmptySet, and don't have
   the form Concat(_, _) *)

(* val csm : reg * int -> int

   CLOSURE SHIFT METRIC

   reg1 <csm reg2 iff
   numLeaves reg1 = numLeaves reg2 and,
   for all n >= 0, csm(reg1, n) < csm(reg2, n)

   this relation is well founded, and is compatible with contexts

   we say that THE CLOSURE SHIFT METRIC STRICTLY DECREASES FROM reg2
   TO reg1 (or reg1 IS STRICTLY LESS THAN reg2 IN THE CLOSURE SHIFT
   METRIC) iff reg1 <csm reg2 *)

fun csm(EmptyStr, _)           = 0
  | csm(EmptySet, _)           = 0
  | csm(Sym _, _)              = 0
  | csm(Closure reg, n)        = csm(reg, n) + n
  | csm(Concat(reg1, reg2), n) = csm(reg1, numLeaves reg2 + n) + csm(reg2, n)
  | csm(Union(reg1, reg2), n)  = csm(reg1, numLeaves reg2 + n) + csm(reg2, n)

(* if reg is almost weakly simplified, then shiftClosuresRight reg is
   equivalent to reg, is weakly simplified, has the same alphabet as
   reg, has the same closure complexity as reg, has the same size as
   reg, has the same number of concatenations as reg, and has the same
   number of symbols as reg *)

local
  fun shift1 (Concat(Closure reg1, reg2)) =
        if equal(reg1, reg2)
        then SOME(rightConcat(reg1, Closure reg1))
        else NONE
    | shift1 _                            = NONE

  fun shift2 (Concat(Closure reg1, Concat(reg2, reg3))) =
        if equal(reg1, reg2)
        then SOME(Concat(reg1, Concat(Closure reg1, reg3)))
        else NONE
    | shift2 _                                          = NONE

  fun shift3 (Concat(Closure(Concat(reg1, reg2)), reg3)) =
        if equal(reg1, reg3)
        then SOME(Concat(reg1, Closure(rightConcat(reg2, reg1))))
        else NONE
    | shift3 _                                           = NONE

  fun shift4 (Concat(Closure(Concat(reg1, reg2)), Concat(reg3, reg4))) =
        if equal(reg1, reg3)
        then SOME(Concat(reg1, Concat(Closure(rightConcat(reg2, reg1)), reg4)))
        else NONE
    | shift4 _                                                         = NONE
in
  (* if reg is almost weakly simplified, then shiftClosuresRight reg
     is equivalent to reg, is weakly simplified, has the same alphabet
     as reg, has the same closure complexity as reg, has the same size
     as reg, has the same number of concatenations as reg, has the
     same number of symbols as reg, and either has strictly smaller
     closure shift metric than reg, or is equal to reg

     in a recursive call, either the size strictly decreases or the
     size is preserved and the closure shift metric strictly decreases *)

  fun shiftClosuresRight (Concat(reg1, reg2)) =
        let val reg = Concat(reg1, shiftClosuresRight reg2)
        in case shift1 reg of
                NONE      =>
                  (case shift2 reg of
                        NONE      =>
                          (case shift3 reg of
                                NONE      =>
                                  (case shift4 reg of
                                        NONE      => reg
                                      | SOME reg' => shiftClosuresRight reg')
                              | SOME reg' => shiftClosuresRight reg')
                      | SOME reg' => shiftClosuresRight reg')
              | SOME reg' => shiftClosuresRight reg'
        end
    | shiftClosuresRight reg                  = reg
end

(* deepClosure reg is equivalent to Closure reg, is weakly simplified
   if reg is weakly simplified, has an alphabet that is the same as
   the alphabet of Closure reg, has a closure complexity that is no
   greater than that of Closure reg, is no bigger than Closure reg,
   has the some number of concatenations (numConcats) as Closure reg,
   and has the same number of symbols (numSyms) as Closure reg *)

fun deepClosure EmptyStr      = EmptyStr
  | deepClosure EmptySet      = EmptyStr
  | deepClosure (Closure reg) = Closure reg
  | deepClosure reg           = Closure reg

(* deepConcat(reg1, reg2) is equivalent to Concat(reg1, reg2), is
   weakly simplified if reg1 and reg2 are weakly simplified, has an
   alphabet that is a subset of the alphabet of Concat(reg1, reg2),
   has a closure complexity that is no greater than that of
   Concat(reg1, reg2), is no bigger than Concat(reg1, reg2), has no
   more concatenations than Concat(reg1, reg2), and has no more
   symbols than Concat(reg1, reg2) *)

fun deepConcat(reg,      EmptySet) = EmptySet
  | deepConcat(EmptySet, reg)      = EmptySet
  | deepConcat(reg,      EmptyStr) = reg
  | deepConcat(EmptyStr, reg)      = reg
  | deepConcat(reg1,     reg2)     = shiftClosuresRight(rightConcat(reg1, reg2))

(* deepUnion(reg1, reg2) is equivalent to Union(reg1, reg2), is weakly
   simplified if reg1 and reg2 are weakly simplified, has the same
   alphabet as Union(reg1, reg2), has a closure complexity that is no
   greater than that of Union(reg1, reg2), is no bigger than
   Union(reg1, reg2), has no more concatenations than Union(reg1,
   reg2), and has no more symbols than Union(reg1, reg2) *)

fun deepUnion(reg,      EmptySet) = reg
  | deepUnion(EmptySet, reg)      = reg
  | deepUnion(reg1,     reg2)     = sortUnions(rightUnion(reg1, reg2))

(* weaklySimplify reg is equivalent to reg, is weakly simplified, has
   an alphabet that is a subset of the alphabet of reg, has a closure
   complexity that is no greater than that of reg, is no bigger than
   reg, has no more concatenations than reg, and has no more symbols
   than reg *)

fun weaklySimplify EmptyStr             = EmptyStr
  | weaklySimplify EmptySet             = EmptySet
  | weaklySimplify (Sym a)              = Sym a
  | weaklySimplify (Closure reg)        = deepClosure(weaklySimplify reg)
  | weaklySimplify (Concat(reg1, reg2)) =
      deepConcat(weaklySimplify reg1, weaklySimplify reg2)
  | weaklySimplify (Union(reg1, reg2))  =
      deepUnion(weaklySimplify reg1, weaklySimplify reg2)

fun hasClosure EmptyStr             = false
  | hasClosure EmptySet             = false
  | hasClosure (Sym _)              = false
  | hasClosure (Closure _)          = true
  | hasClosure (Concat(reg1, reg2)) = hasClosure reg1 orelse hasClosure reg2
  | hasClosure (Union(reg1, reg2))  = hasClosure reg1 orelse hasClosure reg2

fun toStrSetOpt reg =
      let fun toSS EmptyStr             = Set.sing nil
            | toSS EmptySet             = Set.empty
            | toSS (Sym a)              = Set.sing[a]
            | toSS (Closure _)          = M.cannotHappen()
            | toSS (Concat(reg1, reg2)) = StrSet.concat(toSS reg1, toSS reg2)
            | toSS (Union(reg1, reg2))  = StrSet.union(toSS reg1, toSS reg2)

          val reg' = weaklySimplify reg
      in if hasClosure reg'
         then NONE
         else SOME(toSS reg')
      end

fun toStrSet reg =
      case toStrSetOpt reg of
           NONE    => M.errorString(fn () => ["language", "is", "infinite"])
         | SOME xs => xs

(* auxliary functions for local and global simplification *)

fun hasEmp EmptyStr             = true
  | hasEmp EmptySet             = false
  | hasEmp (Sym _)              = false
  | hasEmp (Closure reg)        = true
  | hasEmp (Concat(reg1, reg2)) =
      hasEmp reg1 andalso hasEmp reg2
  | hasEmp (Union(reg1, reg2))  =
      hasEmp reg1 orelse hasEmp reg2

fun hasSym(_, EmptyStr)           = false
  | hasSym(_, EmptySet)           = false
  | hasSym(a, Sym b)              = Sym.equal(a, b)
  | hasSym(a, Closure reg)        = hasSym(a, reg)
  | hasSym(a, Concat(reg1, reg2)) =
      hasSym(a, reg1) andalso hasEmp reg2 orelse
      hasEmp reg1 andalso hasSym(a, reg2)
  | hasSym(a, Union(reg1, reg2))  =
      hasSym(a, reg1) orelse hasSym(a, reg2)

local
  (* in recursive calls, sum of sizes of args goes down

     loss means loses precision;
     ass  means, assuming that regular expressions are
          weakly simplified, won't (directly) lose precision *)

  fun obviSub(reg, reg') =
        equal(reg, reg') orelse
        case reg of
             EmptyStr           => hasEmp reg'
           | EmptySet           => true
           | Sym a              => hasSym(a, reg')
           | Closure reg1       =>
               (case reg' of
                     EmptyStr            => false  (* ass *)
                   | EmptySet            => false
                   | Sym _               => false
                   | Closure _           => obviSub(reg1, reg')
                   | Concat(reg'1, reg'2) =>  (* loss *)
                       hasEmp reg'1 andalso obviSub(reg, reg'2) orelse
                       hasEmp reg'2 andalso obviSub(reg, reg'1)
                   | Union(reg'1, reg'2) =>  (* loss *)
                       obviSub(reg, reg'1)  orelse
                       obviSub(reg, reg'2))
           | Concat(reg1, reg2) =>
               (case reg' of
                     EmptyStr             => false  (* ass *)
                   | EmptySet             => false  (* ass *)
                   | Sym _                => false  (* ass *)
                   | Closure reg'1        =>  (* loss *)
                       obviSub(reg, reg'1) orelse
                       obviSub(reg1, reg') andalso obviSub(reg2, reg')
                   | Concat(reg'1, reg'2) =>  (* loss *)
                       obviSub(reg1, reg'1) andalso obviSub(reg2, reg'2) orelse
                       hasEmp reg'1 andalso obviSub(reg, reg'2)          orelse
                       hasEmp reg'2 andalso obviSub(reg, reg'1)          orelse
                       (if isClosure reg'1 andalso not(isClosure reg'2)
                          then obviSub(reg1, reg'1) andalso
                               obviSub(reg2, reg')
                        else if isClosure reg'2 andalso not(isClosure reg'1)
                          then obviSub(reg1, reg') andalso
                               obviSub(reg2, reg'2)
                        else if isClosure reg'1 andalso isClosure reg'2
                          then (obviSub(reg1, reg'1) andalso
                                obviSub(reg2, reg'))
                               orelse
                               (obviSub(reg1, reg') andalso
                                obviSub(reg2, reg'2))
                        else false)
                   | Union(reg'1, reg'2)  =>  (* loss *)
                       obviSub(reg, reg'1) orelse obviSub(reg, reg'2))
           | Union(reg1, reg2)  =>
               obviSub(reg1, reg') andalso obviSub(reg2, reg')
in
  fun obviousSubset(reg, reg') =
        obviSub(weaklySimplify reg, weaklySimplify reg')
end

(* localSimplificationRelations(reg, reg') returns:

   LESS if reg is related to reg' in the local simplification well-ordering;

   EQUAL if reg is related to reg' in the local simplification equivalence
     relation; and

   GREATER if reg' is related to reg in the local simplification
     well-ordering. *)

fun localSimplificationRelations(reg, reg') =
      case compareCC(cc reg, cc reg') of
           LESS    => LESS
         | EQUAL   =>
             (case Int.compare(size reg, size reg') of
                   LESS    => LESS
                 | EQUAL   =>
                     (case Int.compare(numConcats reg, numConcats reg') of
                           LESS    => LESS
                         | EQUAL   => Int.compare(numSyms reg, numSyms reg')
                         | GREATER => GREATER)
                 | GREATER => GREATER)
         | GREATER => GREATER

(* simplification rules *)

datatype rule_type = WeakSimpRule
                   | StructRule of int
                   | ReductRule of int

fun ruleTypeToPPList WeakSimpRule   = [PP.fromString "weak simplification"]
  | ruleTypeToPPList (StructRule n) =
      [PP.fromString "structural", PP.fromString "rule",
       PP.fromString(Int.toString n)]
  | ruleTypeToPPList (ReductRule n) = 
      [PP.fromString "reduction", PP.fromString "rule",
       PP.fromString(Int.toString n)]

fun ruleTypeKind WeakSimpRule   = 1
  | ruleTypeKind (StructRule _) = 2
  | ruleTypeKind (ReductRule _) = 3

fun compareRuleType(rt, rt') =
      case Int.compare(ruleTypeKind rt, ruleTypeKind rt') of
           LESS    => LESS
         | EQUAL   =>
             (case (rt, rt') of
                   (WeakSimpRule, _)             => EQUAL
                 | (StructRule n, StructRule n') => Int.compare(n, n')
                 | (ReductRule n, ReductRule n') => Int.compare(n, n')
                 | _                             => M.cannotHappen())
         | GREATER => GREATER

(* weak simplification rule *)

fun weakSimpRule reg =
      if weaklySimplified reg
      then NONE
      else SOME(WeakSimpRule, weaklySimplify reg)

(* structural rules *)

(* (alpha + beta) + gamma -> alpha + (beta + gamma) *)

fun structRule1 reg =
      case reg of
           Union(Union(reg1, reg2), reg3) =>
             SOME(StructRule 1, Union(reg1, Union(reg2, reg3)))
         | _                              => NONE

(* alpha + (beta + gamma) -> (alpha + beta) + gamma *)

fun structRule2 reg =
      case reg of
           Union(reg1, Union(reg2, reg3)) =>
             SOME(StructRule 2, Union(Union(reg1, reg2), reg3))
         | _                              => NONE

(* alpha(beta gamma) -> (alpha beta)gamma *)

fun structRule3 reg =
      case reg of
           Concat(Concat(reg1, reg2), reg3) =>
             SOME(StructRule 3, Concat(reg1, Concat(reg2, reg3)))
         | _                                => NONE

(* (alpha beta)gamma -> alpha(beta gamma) *)

fun structRule4 reg =
      case reg of
           Concat(reg1, Concat(reg2, reg3)) =>
             SOME(StructRule 4, Concat(Concat(reg1, reg2), reg3))
         | _                                => NONE

(* alpha + beta -> beta + alpha *)

fun structRule5 reg =
      case reg of
           Union(reg1, reg2) => SOME(StructRule 5, Union(reg2, reg1))
         | _                 => NONE

(* alpha* alpha -> alpha alpha* *)

fun structRule6 reg =
      case reg of
           Concat(Closure reg1, reg2) =>
             if equal(reg1, reg2)
             then SOME(StructRule 6, Concat(reg1, Closure reg1))
             else NONE
         | _                          => NONE

(* alpha alpha* -> alpha* alpha *)

fun structRule7 reg =
      case reg of
           Concat(reg1, Closure reg2) =>
             if equal(reg1, reg2)
             then SOME(StructRule 7, Concat(Closure reg1, reg1))
             else NONE
         | _                          => NONE

(* alpha(beta alpha)* -> (alpha beta)* alpha *)

fun structRule8 reg =
      case reg of
           Concat(reg1, Closure(Concat(reg2, reg3))) =>
             if equal(reg1, reg3)
             then SOME(StructRule 8, Concat(Closure(Concat(reg1, reg2)), reg1))
             else NONE
         | _                                         => NONE

(* (alpha beta)*alpha -> alpha(beta alpha)* *)

fun structRule9 reg =
      case reg of
           Concat(Closure(Concat(reg1, reg2)), reg3) =>
             if equal(reg1, reg3)
             then SOME(StructRule 9, Concat(reg1, Closure(Concat(reg2, reg1))))
             else NONE
         | _                                         => NONE

val structRules =
      [structRule1, structRule2, structRule3, structRule4, structRule5,
       structRule6, structRule7, structRule8, structRule9]

(* reduction rules *)

(* if sub(alpha, beta), then alpha + beta -> beta *)

fun reductRule1 sub reg =
      case reg of
           Union(reg1, reg2) =>
             if sub(reg1, reg2)
             then SOME(ReductRule 1, reg2)
             else NONE
         | _                 => NONE

(* alpha beta1 + alpha beta2 -> alpha(beta1 + beta2) *)

fun reductRule2 reg =
      case reg of
           Union(Concat(reg1, reg2), Concat(reg3, reg4)) =>
             if equal(reg1, reg3)
             then SOME(ReductRule 2, Concat(reg1, Union(reg2, reg4)))
             else NONE
         | _                                             => NONE

(* alpha1 beta + alpha2 beta -> (alpha1 + alpha2)beta *)

fun reductRule3 reg =
      case reg of
           Union(Concat(reg1, reg2), Concat(reg3, reg4)) =>
             if equal(reg2, reg4)
             then SOME(ReductRule 3, Concat(Union(reg1, reg3), reg2))
             else NONE
         | _                                             => NONE

(* if hasEmp(alpha) and sub( alpha, beta* ), then alpha beta* -> beta* *)

fun reductRule4 sub reg =
      case reg of
           Concat(reg1, Closure reg2) =>
             if hasEmp reg1             andalso
                sub(reg1, Closure reg2)
             then SOME(ReductRule 4, Closure reg2)
             else NONE
         | _                          => NONE

(* if hasEmp(beta) and sub( beta, alpha* ), then alpha* beta -> alpha* *)

fun reductRule5 sub reg =
      case reg of
           Concat(Closure reg1, reg2) =>
             if hasEmp reg2             andalso
                sub(reg2, Closure reg1)
             then SOME(ReductRule 5, Closure reg1)
             else NONE
         | _                          => NONE

(* if sub( alpha, beta* ), then (alpha + beta)* -> beta* *)

fun reductRule6 sub reg =
      case reg of
           Closure(Union(reg1, reg2)) =>
             if sub(reg1, Closure reg2)
             then SOME(ReductRule 6, Closure reg2)
             else NONE
         | _                          => NONE

(* (alpha* + beta)* -> (alpha + beta)* *)

fun reductRule7 reg =
      case reg of
           Closure(Union(Closure reg1, reg2)) =>
             SOME(ReductRule 7, Closure(Union(reg1, reg2)))
         | _                                  => NONE

(* if hasEmp(alpha) and hasEmp(beta), then (alpha beta)* ->
   (alpha + beta)* *)

fun reductRule8 reg =
      case reg of
           Closure(Concat(reg1, reg2)) =>
             if hasEmp reg1 andalso hasEmp reg2
             then SOME(ReductRule 8, Closure(Union(reg1, reg2)))
             else NONE
         | _                           => NONE

(* if hasEmp(alpha) and hasEmp(beta), then (alpha beta + gamma)* ->
   (alpha + beta + gamma)* *)

fun reductRule9 reg =
      case reg of
           Closure(Union(Concat(reg1, reg2), reg3)) =>
             if hasEmp reg1 andalso hasEmp reg2
             then SOME(ReductRule 9, Closure(Union(reg1, Union(reg2, reg3))))
             else NONE
         | _                                        => NONE

(* if hasEmp(alpha) and sub( alpha, beta* ), then (alpha beta)* -> beta* *)

fun reductRule10 sub reg =
      case reg of
           Closure(Concat(reg1, reg2)) =>
             if hasEmp reg1             andalso
                sub(reg1, Closure reg2)
             then SOME(ReductRule 10, Closure reg2)
             else NONE
         | _                           => NONE

(* if hasEmp(beta) and sub( beta, alpha* ), then (alpha beta)* -> alpha* *)

fun reductRule11 sub reg =
      case reg of
           Closure(Concat(reg1, reg2)) =>
             if hasEmp reg2             andalso
                sub(reg2, Closure reg1)
             then SOME(ReductRule 11, Closure reg1)
             else NONE
         | _                           => NONE

(* if hasEmp(alpha) and sub( alpha, (beta + gamma)* ), then
   (alpha beta + gamma)* -> (beta + gamma)* *)

fun reductRule12 sub reg =
      case reg of
           Closure(Union(Concat(reg1, reg2), reg3)) =>
             if hasEmp reg1                           andalso
                sub(reg1, Closure(Union(reg2, reg3)))
             then SOME(ReductRule 12, Closure(Union(reg2, reg3)))
             else NONE
         | _                                        => NONE

(* if hasEmp(beta) and sub( beta, (alpha + gamma)* ), then
   (alpha beta + gamma)* -> (alpha + gamma)* *)

fun reductRule13 sub reg =
      case reg of
           Closure(Union(Concat(reg1, reg2), reg3)) =>
             if hasEmp reg2                           andalso
                sub(reg2, Closure(Union(reg1, reg3)))
             then SOME(ReductRule 13, Closure(Union(reg1, reg3)))
             else NONE
         | _                                        => NONE

(* if not(hasEmp alpha) and 
   compareCC
   (succCC(succCC(cc beta)),
    unionCC(cc alpha, succCC(cc beta))) = GREATER, then
   ( alpha beta* )* -> %  +  alpha(alpha + beta)* *)

fun reductRule14 reg =
      case reg of
           Closure(Concat(reg1, Closure reg2)) =>
             if not(hasEmp reg1) andalso
                compareCC
                (unionCC(cc reg1, succCC(cc reg2)),
                 succCC(succCC(cc reg2))) = LESS
             then SOME
                  (ReductRule 14,
                   Union(EmptyStr, Concat(reg1, Closure(Union(reg1, reg2)))))
             else NONE
         | _                                   => NONE

(* if not(hasEmp beta) and
   compareCC
   (succCC(succCC(cc alpha)),
    unionCC(succCC(cc alpha), cc beta)) = GREATER, then
   (alpha* beta)* -> %  +  (alpha + beta)* beta *)

fun reductRule15 reg =
      case reg of
           Closure(Concat(Closure reg1, reg2)) =>
             if not(hasEmp reg2) andalso
                compareCC
                (unionCC(succCC(cc reg1), cc reg2),
                 succCC(succCC(cc reg1))) = LESS
             then SOME
                  (ReductRule 15,
                   Union(EmptyStr, Concat(Closure(Union(reg1, reg2)), reg2)))
             else NONE
         | _                                   => NONE

(* if not(hasEmp alpha) orelse not(hasEmp gamma), and
   compareCC
   (succCC(succCC(cc beta)),
    unionCC(cc alpha, unionCC(succCC(cc beta), cc gamma))) = GREATER, then
   (alpha beta* gamma)* -> %  +  alpha (beta + gamma alpha)* gamma *)

fun reductRule16 reg =
      case reg of
           Closure(Concat(reg1, Concat(Closure reg2, reg3))) =>
             if (not(hasEmp reg1) orelse not(hasEmp reg3)) andalso
                compareCC
                (unionCC(cc reg1, unionCC(succCC(cc reg2), cc reg1)),
                 succCC(succCC(cc reg2))) = LESS
             then SOME
                  (ReductRule 16,
                   Union(EmptyStr,
                         Concat(reg1,
                                Concat(Closure(Union(reg2, Concat(reg3, reg1))),
                                       reg3))))
             else NONE
         | _                                                 => NONE

(* if sub(alpha alpha*, beta), then alpha* + beta -> % + beta *)

fun reductRule17 sub reg =
      case reg of
           Union(Closure reg1, reg2) =>
             if sub(Concat(reg1, Closure reg1), reg2)
             then SOME(ReductRule 17, Union(EmptyStr, reg2))
             else NONE
         | _                         => NONE

(* if hasEmp(beta) and sub(alpha alpha alpha*, beta),
   then alpha* + beta -> alpha + beta *)

fun reductRule18 sub reg =
      case reg of
           Union(Closure reg1, reg2) =>
             if hasEmp reg1                                         andalso
                sub(Concat(reg1, Concat(reg1, Closure reg1)), reg2)
             then SOME(ReductRule 18, Union(reg1, reg2))
             else NONE
         | _                         => NONE

(* (alphabet) if alpha not in {%, $} and sub(power(alpha, n), beta),
   then power(alpha, n + 1) alpha* + beta ->
   power(alpha, n) alpha* + beta *)

fun reductRule19 sub reg =
      case reg of
           Union(Concat(reg1, Closure reg2), reg3) =>
             let fun pow reg' =
                       if equal(reg', reg2)
                       then SOME 1
                       else case reg' of
                                 Concat(reg'1, reg'2) =>
                                   if equal(reg'1, reg2)
                                   then case pow reg'2 of
                                             NONE   => NONE
                                           | SOME n => SOME(n + 1)
                                   else NONE
                               | _                    => NONE
             in if equal(reg1, EmptyStr) orelse equal(reg1, EmptySet)
                then NONE
                else case pow reg1 of
                          NONE   => NONE
                        | SOME n =>
                            let val reg' = power(reg2, n - 1)
                            in if sub(reg', reg3)
                               then SOME
                                    (ReductRule 19,
                                     Union(Concat(reg', Closure reg2), reg3))
                               else NONE
                            end
             end
         | _                                       => NONE

(* if n >= 2, l >= 0 and 2n - 1 < m_1 < ... < m_l, then (power(alpha,
  n) + power(alpha, n+1) + ... + power(alpha, 2n - 1) + power(alpha,
  m_1) + ... + power(alpha, m_l))* -> % + power(alpha, n) alpha* *)

local
  (* if there is no n >= 1 such that reg = power(base, n), then
     findPower(base, reg) returns NONE; otherwise, it returns SOME n,
     where n >= 1 is unique such that reg = power(base, n) *)

  fun findPower(base, reg) =
        if equal(reg, base)
        then SOME 1
        else case reg of
                  Concat(reg1, reg2) =>
                    if equal(reg1, base)
                    then case findPower(base, reg2) of
                              NONE   => NONE
                            | SOME n => SOME(n + 1)
                    else NONE
                | _                  => NONE

  fun findPowers(base, reg) =
        case reg of
             Union(reg1, reg2) =>
               (case findPower(base, reg1) of
                     NONE   => NONE
                   | SOME n =>
                       (case findPowers(base, reg2) of
                             NONE    => NONE
                           | SOME ms => SOME(n :: ms)))
           | _                 =>
               (case findPower(base, reg) of
                     NONE   => NONE
                   | SOME n => SOME[n])

  (* in a call of complete(n, ms), n >= 2 and n :: ms is sorted in
     strictly ascending order; tests whether n + 1, ..., 2n - 1 are
     in ms *)

  fun complete(n, ms) =
        let val l = n * 2

            fun comp(j, nil)     = j = l
              | comp(j, m :: ms) = j = l orelse m = j andalso comp(j + 1, ms)
        in comp(n + 1, ms) end
in
  fun reductRule20 reg =
        case reg of
             Closure(reg' as Union(Concat(reg1, _), _)) =>
               (case findPowers(reg1, reg') of
                     NONE    => NONE
                   | SOME ms =>
                       if Sort.sorted (false, Int.compare) ms andalso
                          complete(hd ms, tl ms)
                       then SOME
                            (ReductRule 20,
                             Union
                             (EmptyStr,
                              Concat(power(reg1, hd ms), Closure reg1)))
                       else NONE)
           | _                                          => NONE
end

(* (alphabet) if alpha not in {%, $}, then alpha + alpha beta ->
   alpha(% + beta) *)

fun reductRule21 reg =
      case reg of
           Union(reg1, Concat(reg2, reg3)) =>
             if equal(reg1, reg2)          andalso
                not(equal(reg1, EmptyStr)) andalso
                not(equal(reg1, EmptySet))
             then SOME(ReductRule 21, Concat(reg1, Union(EmptyStr, reg3)))
             else NONE
         | _                               => NONE

(* (alphabet) if alpha not in {%, $}, then alpha + beta alpha ->
   (% + beta)alpha *)

fun reductRule22 reg =
      case reg of
           Union(reg1, Concat(reg2, reg3)) =>
             if equal(reg1, reg3)          andalso
                not(equal(reg1, EmptyStr)) andalso
                not(equal(reg1, EmptySet))
             then SOME(ReductRule 22, Concat(Union(EmptyStr, reg2), reg1))
             else NONE
         | _                               => NONE

(* alpha*( % + beta (alpha + beta)* ) -> (alpha + beta)* *)

fun reductRule23 reg =
      case reg of
           Concat
           (Closure reg1,
            Union(EmptyStr, Concat(reg2, Closure(Union(reg3, reg4))))) =>
             if equal(reg1, reg3) andalso equal(reg2, reg4)
             then SOME(ReductRule 23, Closure(Union(reg1, reg2)))
             else NONE
         | _                                                           => NONE

(* (% + (alpha + beta)* alpha) beta* -> (alpha + beta)* *)

fun reductRule24 reg =
      case reg of
           Concat
           (Union(EmptyStr, Concat(Closure(Union(reg1, reg2)), reg3)),
            Closure reg4)                                              =>
             if equal(reg1, reg3) andalso equal(reg2, reg4)
             then SOME(ReductRule 24, Closure(Union(reg1, reg2)))
             else NONE
         | _                                                           => NONE

(* if sub( alpha, beta* ) and sub(beta, alpha), then
   % + alpha beta* -> beta* *)

fun reductRule25 sub reg =
      case reg of
           Union(EmptyStr, Concat(reg1, Closure reg2)) =>
             if sub(reg1, Closure reg2) andalso sub(reg2, reg1)
             then SOME(ReductRule 25, Closure reg2)
             else NONE
         | _                                           => NONE

(* if sub( beta, alpha* ) and sub(alpha, beta), then
   % + alpha* beta -> alpha* *)

fun reductRule26 sub reg =
      case reg of
           Union(EmptyStr, Concat(Closure reg1, reg2)) =>
             if sub(reg2, Closure reg1) andalso sub(reg1, reg2)
             then SOME(ReductRule 26, Closure reg1)
             else NONE
         | _                                           => NONE

fun reductRules sub =
      [reductRule1 sub, reductRule2, reductRule3, reductRule4 sub,
       reductRule5 sub, reductRule6 sub, reductRule7, reductRule8,
       reductRule9, reductRule10 sub, reductRule11 sub,
       reductRule12 sub, reductRule13 sub, reductRule14, reductRule15,
       reductRule16, reductRule17 sub, reductRule18 sub, reductRule19
       sub, reductRule20, reductRule21, reductRule22, reductRule23,
       reductRule24, reductRule25 sub, reductRule26 sub]

fun simpRules sub = [weakSimpRule] @ structRules @ reductRules sub

(* auxiliary functions for local and global simplification *)

local
  fun applicSimpRuleTopLevel(nil,       _)   = false
    | applicSimpRuleTopLevel(sr :: srs, reg) =
        case sr reg of
             NONE   => applicSimpRuleTopLevel(srs, reg)
           | SOME _ => true
in
  fun applicableSimpRule(srs, reg) =
        applicSimpRuleTopLevel(srs, reg) orelse
        (case reg of
              Closure reg'       => applicableSimpRule(srs, reg')
            | Concat(reg1, reg2) =>
                applicableSimpRule(srs, reg1) orelse
                applicableSimpRule(srs, reg2)
            | Union(reg1, reg2)  =>
                applicableSimpRule(srs, reg1) orelse
                applicableSimpRule(srs, reg2)
            | _                  => false)
end

fun pathToPP ns =
      if null ns
      then PP.fromString "[]"
      else let fun toPPList [n]       = [PP.fromString(Int.toString n)]
                 | toPPList (n :: ns) =
                     PP.comma(PP.fromString(Int.toString n)) :: toPPList ns
                 | toPPList _         = M.cannotHappen()
           in PP.decorate("[", PP.block(true, toPPList ns), "]") end
      
val comparePath = Set.compareList Int.compare

local
  fun multipleSimpRulesTopLevel(nil,       _)   = nil
    | multipleSimpRulesTopLevel(sr :: srs, reg) =
        case sr reg of
             NONE           => multipleSimpRulesTopLevel(srs, reg)
           | SOME(rt, reg') =>
               (rt, nil, reg') :: multipleSimpRulesTopLevel(srs, reg)
in
  fun multipleSimpRules(srs, reg) =
        multipleSimpRulesTopLevel(srs, reg) @
        (case reg of
              Closure reg'       =>
                map (fn (rt, ns, reg'') => (rt, [1] @ ns, Closure reg''))
                    (multipleSimpRules(srs, reg'))
            | Concat(reg1, reg2) =>
                map (fn (rt, ns, reg1') => (rt, [1] @ ns, Concat(reg1', reg2)))
                    (multipleSimpRules(srs, reg1)) @
                map (fn (rt, ns, reg2') => (rt, [2] @ ns, Concat(reg1, reg2')))
                    (multipleSimpRules(srs, reg2))
            | Union(reg1, reg2)  =>
                map (fn (rt, ns, reg1') => (rt, [1] @ ns, Union(reg1', reg2)))
                    (multipleSimpRules(srs, reg1)) @
                map (fn (rt, ns, reg2') => (rt, [2] @ ns, Union(reg1, reg2')))
                    (multipleSimpRules(srs, reg2))
            | _                  => nil)
end

(* in ([(reg1, rt1, path1), ..., (regn, rtn, pathn)], reg), each pathi
   is a valid path into regi

   if we apply rule rt1 at position path1 in reg1, this gives us reg2;
   ...; if we apply rule rtn to regn at position pathn, this gives us
   reg

   we say that this element of trace_reg TAKES US FROM reg1 TO reg;
   when n = 0, we have (nil, reg) which takes us from reg to reg *)

type trace_reg = (reg * rule_type * int list)list * reg

local
  fun stepsToPPList nil                      = nil
    | stepsToPPList ((reg, rt, ns) :: steps) =
        if (case rt of WeakSimpRule => true | _ => false) andalso null ns
        then [toPP reg, PP.fromString "weakly", PP.fromString "simplifies",
              PP.fromString "to"] @
             stepsToPPList steps
        else [toPP reg, PP.fromString "transformed", PP.fromString "by"] @
             ruleTypeToPPList rt                                         @
             [PP.fromString "at", PP.fromString "position",
              pathToPP ns]                                               @
             [PP.fromString "to"]                                        @
             stepsToPPList steps
in
  fun traceRegToPP(steps, reg) = stepsToPPList steps @ [toPP reg]

  fun explainTraceReg(steps, reg) =
        M.messagePP(fn () => traceRegToPP(steps, reg))
end

(* we treat the following as an abstract type *)

type simp_rule_closure = reg Set.set * trace_reg list

(* val simpRuleClosure :
         (reg -> (rule_type * reg)option)list * reg ->
         simp_rule_closure *
         (simp_rule_closure -> (trace_reg * simp_rule_closure)option)

   if all the elements of srs are total functions, then
   simpRuleClosure(srs, reg) returns a pair (clos, next) such that the
   sequence #1(valOf(next clos)), #1(valOf(next(#2(valOf(next
   clos))))), #1(valOf(next(#2(valOf(next(#2(valOf(next clos)))))))),
   ..., up until Option is raised, consists of a possibly infinite
   sequence of trace_reg's, (steps_1, reg_1), (steps_2, reg_2), ...,
   such that:

     reg_1, reg_2, ..., are (without duplication) the regular
     expressions that can be formed from reg by repeatedly using
     elements of srs on arbitrary subtrees;

     each steps_i explains how reg_i was formed from reg (there is
     never a repetition of the same regular expression in steps_i,
     and reg_i never appears in steps_i);
     
     length steps_1 <= length steps_2 <= ... *)

fun simpRuleClosure((srs, reg) :
                      (reg -> (rule_type * reg)option)list * reg) :
        simp_rule_closure *
        (simp_rule_closure -> (trace_reg * simp_rule_closure)option) =
      let fun next(_,   nil)                             = NONE
            | next(old, (trReg as (steps, reg)) :: news) =
                if Set.memb compare (reg, old)
                then next(old, news)
                else let val old =
                               Set.union compare (old, Set.sing reg)
                         val news   =
                               news @
                               map (fn (rt, ns, reg') =>
                                         (steps @ [(reg, rt, ns)], reg'))
                                   (multipleSimpRules(srs, reg))
                        in SOME(trReg, (old, news)) end
      in ((Set.empty, [(nil, reg)]), next) end

(* local simplification *)

fun locallySimplified sub reg =
      weaklySimplified reg andalso
      let val reductRules  = reductRules sub
          val (clos, next) = simpRuleClosure(structRules, reg)

          fun loop NONE                   = true
            | loop (SOME((_, reg), clos)) =
                not(applicableSimpRule(reductRules, reg)) andalso
                loop(next clos)
      in loop(next clos) end

(* defined by well-founded recursion on the local simplification
   well-founded relation; see localSimplificationRelations *)

fun locallySimplifyCommon(nOpt, sub, reg) =
      let val reductRules  = reductRules sub
          val (clos, next) = simpRuleClosure(structRules, reg)
              
          fun isDone NONE     = false
            | isDone (SOME n) = n = 0

          fun decr NONE     = NONE
            | decr (SOME n) = SOME(n - 1)
  
          fun compareComplexityTraceReg((steps, reg), (steps', reg')) =
                case compareComplexityTotal(reg, reg') of
                     LESS    => LESS
                   | EQUAL   =>
                       Set.compareList
                       (Set.compareTriple
                        (compare, compareRuleType, comparePath))
                       (steps, steps')
                   | GREATER => GREATER

          fun loop (_,    NONE,                       trRegOpt, m) =
                (M.messagePP
                 (fn () =>
                       [PP.fromString "considered", PP.fromString "all",
                        PP.fromString(Int.toString m),
                        PP.fromString "structural",
                        PP.fromString "reorganizations",
                        PP.fromString "of", toPP reg]);
                 case trRegOpt of
                      NONE       =>
                        (M.messagePP
                         (fn () =>
                               [toPP reg, PP.fromString "is",
                                PP.fromString "locally",
                                PP.fromString "simplified"]);
                         (true, reg))
                    | SOME trReg =>
                        (explainTraceReg trReg;
                         locallySimplifyCommon(nOpt, sub, #2 trReg)))
            | loop (mOpt, SOME((steps, reg'), clos), trRegOpt, m) =
                if isDone mOpt
                then (M.messagePP
                      (fn () =>
                            [PP.fromString "exploration", PP.fromString "of",
                             PP.fromString "structural",
                             PP.fromString "reorganizations",
                             PP.fromString "of",
                             toPP reg,
                             PP.fromString "curtailed"]);
                      case trRegOpt of
                           NONE       =>
                             (M.messagePP
                              (fn () =>
                                    [toPP reg, PP.fromString "may",
                                     PP.fromString "not", PP.fromString "be",
                                     PP.fromString "locally",
                                     PP.fromString "simplified"]);
                              (false, reg))
                         | SOME trReg =>
                             (explainTraceReg trReg;
                              locallySimplifyCommon(nOpt, sub, #2 trReg)))
                else let val simpResults = multipleSimpRules(reductRules, reg')
                         val trRegs      =
                               Sort.sort
                               (false, compareComplexityTraceReg)
                               ((case trRegOpt of
                                     NONE       => nil
                                   | SOME trReg => [trReg]) @
                                (map (fn (rt, ns, reg'') =>
                                           case weakSimpRule reg'' of
                                                NONE             =>
                                                  (steps @ [(reg', rt, ns)],
                                                   reg'')
                                              | SOME(_, reg''') =>
                                                  (steps @
                                                   [(reg', rt, ns),
                                                    (reg'', WeakSimpRule,
                                                     nil)],
                                                   reg''')))
                                     simpResults)
                     in if null trRegs
                        then loop(decr mOpt, next clos, NONE, m + 1)
                        else loop(decr mOpt, next clos, SOME(hd trRegs),
                                  m + 1)
                     end
      in loop(nOpt, next clos, NONE, 0) end

fun locallySimplifyTrace (nOpt, sub) reg =
      if case nOpt of
              NONE   => false
            | SOME n => n < 1
      then M.errorString
           (fn () => ["limit", "must", "be", "at", "least", "1"])
      else let val reg' =
                     case weakSimpRule reg of
                          NONE          => reg
                        | SOME(_, reg') =>
                            (M.messagePP
                             (fn () =>
                                   [toPP reg, PP.fromString "weakly",
                                    PP.fromString "simplifies",
                                    PP.fromString "to", toPP reg']);
                             reg')
           in locallySimplifyCommon(nOpt, sub, reg') end

fun locallySimplify(nOpt, sub) reg =
      if case nOpt of
              NONE   => false
            | SOME n => n < 1
      then M.errorString
           (fn () => ["limit", "must", "be", "at", "least", "1"])
      else let val reg' =
                     case weakSimpRule reg of
                          NONE          => reg
                        | SOME(_, reg') => reg'
           in M.quiet(fn () => locallySimplifyCommon(nOpt, sub, reg')) end

(* global simplification *)

fun globallySimplified sub reg =
      let val (clos, next) = simpRuleClosure(simpRules sub, reg)

          fun loop NONE                    = true
            | loop (SOME((_, reg'), clos)) =
                compareComplexity(reg', reg) <> LESS andalso
                loop(next clos)
      in loop(next clos) end

fun globallySimplifyCommon(nOpt, sub, reg) =
      let fun isDone NONE     = false
            | isDone (SOME n) = n = 0

          fun decr NONE     = NONE
            | decr (SOME n) = SOME(n - 1)

          fun simplerThan(_,        NONE)          = true
            | simplerThan((_, reg), SOME(_, reg')) =
                compareComplexityTotal(reg, reg') = LESS

          val (clos, next) = simpRuleClosure(simpRules sub, reg)

          fun loop(nOpt, NONE, trRegOpt, trlen, n, maxSiz)               =
                (M.messagePP
                 (fn () =>
                       [PP.fromString "search", PP.fromString "completed",
                        PP.fromString "after", PP.fromString "considering",
                        PP.fromString(Int.toString n),
                        PP.fromString "candidates", PP.fromString "with",
                        PP.fromString "maximum", PP.fromString "size",
                        PP.fromString(Int.toString maxSiz)]);
                 M.messagePP
                 (fn () =>
                       traceRegToPP(valOf trRegOpt) @
                       [PP.fromString "is", PP.fromString "globally",
                        PP.fromString "simplified"]);
                 (true, #2(valOf trRegOpt)))
            | loop(nOpt, SOME(trReg', clos), trRegOpt, trlen, n, maxSiz) =
                if isDone nOpt
                then (M.messagePP
                      (fn () =>
                            [PP.fromString "search", PP.fromString "curtailed",
                             PP.fromString "after", PP.fromString "considering",
                             PP.fromString(Int.toString n),
                             PP.fromString "candidates", PP.fromString "with",
                             PP.fromString "maximum", PP.fromString "size",
                             PP.fromString(Int.toString maxSiz)]);
                      M.messagePP
                      (fn () =>
                            traceRegToPP(valOf trRegOpt) @
                            [PP.fromString "may", PP.fromString "not",
                             PP.fromString "be", PP.fromString "globally",
                             PP.fromString "simplified"]);
                      (false, #2(valOf trRegOpt)))
                else let val trlen =
                               if length(#1 trReg') > trlen
                               then (M.messageString
                                     (fn () =>
                                           ["considering", "candidates", "with",
                                            "explanations", "of", "length",
                                            Int.toString(trlen + 1)]);
                                     trlen + 1)
                               else trlen
                     in if simplerThan(trReg', trRegOpt)
                        then (M.messagePP
                              (fn () =>
                                    [PP.fromString "simplest",
                                     PP.fromString "result",
                                     PP.fromString "now:"] @
                                    traceRegToPP trReg');
                              loop(decr nOpt, next clos, SOME trReg', trlen,
                                   n + 1, Int.max(maxSiz, size(#2 trReg'))))
                        else loop(decr nOpt, next clos, trRegOpt, trlen,
                                  n + 1, Int.max(maxSiz, size(#2 trReg')))
                     end
      in loop(nOpt, next clos, NONE, ~1, 0, 0) end

fun globallySimplifyTrace(nOpt, sub) =
      if case nOpt of
              NONE   => false
            | SOME n => n < 1
      then M.errorString
           (fn () => ["limit", "must", "be", "at", "least", "1"])
      else fn reg => globallySimplifyCommon(nOpt, sub, reg)

fun globallySimplify(nOpt, sub) =
      if case nOpt of
              NONE   => false
            | SOME n => n < 1
      then M.errorString
           (fn () => ["limit", "must", "be", "at", "least", "1"])
      else fn reg =>
                M.quiet(fn () => globallySimplifyCommon(nOpt, sub, reg))

(***************************** Closure Algorithms *****************************)

fun renameAlphabet(reg, rel) =
      if SymRel.bijectionFromAvoiding(rel, alphabet reg, Set.empty)
      then let fun ren EmptyStr             = EmptyStr
                 | ren EmptySet             = EmptySet
                 | ren (Sym a)              = Sym(SymRel.applyFunction rel a)
                 | ren (Closure reg)        = Closure(ren reg)
                 | ren (Concat(reg1, reg2)) = Concat(ren reg1, ren reg2)
                 | ren (Union(reg1, reg2))  = Union(ren reg1, ren reg2)
           in ren reg end
      else M.errorString
           (fn () =>
                 ["invalid", "alphabet", "renaming", "for",
                  "regular", "expression"])

fun rev EmptyStr             = EmptyStr
  | rev EmptySet             = EmptySet
  | rev (Sym a)              = Sym a
  | rev (Closure reg)        = Closure(rev reg)
  | rev (Concat(reg1, reg2)) = Concat(rev reg2, rev reg1)
  | rev (Union(reg1, reg2))  = Union(rev reg1, rev reg2)

fun prefix reg =
      let fun pref EmptyStr             = EmptyStr
            | pref EmptySet             = EmptySet
            | pref (Sym a)              = Union(EmptyStr, Sym a)
            | pref (Closure reg)        = Concat(Closure reg, pref reg)
            | pref (Concat(reg1, reg2)) =
                Union(pref reg1, Concat(reg1, pref reg2))
            | pref (Union(reg1, reg2))  = Union(pref reg1, pref reg2)
      in pref(weaklySimplify reg) end

(*************************** Interface with JForlan ***************************)

fun jforlanNew() =
      let val file   = System.makeTempFile()
          val status = System.runJForlan("reg new " ^ file)
      in if OS.Process.isSuccess status
         then let val reg = input file
                  val _   = OS.FileSys.remove file
              in reg end
         else (OS.FileSys.remove file;
               M.errorString
               (fn () =>
                     ["creation", "of", "regular", "expression", "aborted"]))
      end

fun jforlanEdit reg =
      let val file   = System.makeTempFile()
          val _      = output(file, reg)
          val status = System.runJForlan("reg edit " ^ file)
      in if OS.Process.isSuccess status
         then let val reg = input file
                  val _   = OS.FileSys.remove file
              in reg end
         else (OS.FileSys.remove file;
               M.errorString
               (fn () => ["editing", "of", "regular", "expression", "aborted"]))
      end

(* if called with a string that isn't legal Forlan syntax for a regular
   expression, prints "error" on a line, followed by one or more lines
   of parsing errors

   if called with a string that is legal Forlan syntax for a regular
   expression, prints "valid" on a line, followed by a single line
   consisting of the translation of the regular expression into the
   following fully parenthesized syntax, with no whitespace:

       %             empty string
       $             empty set
       sym           symbol
       *(reg)        closure
       @(reg,reg)    concatenation
       +(reg,reg)    union *)

local
  fun paren EmptyStr             = "%"
    | paren EmptySet             = "$"
    | paren (Sym a)              = Sym.toString a
    | paren (Closure reg)        = "*(" ^ paren reg ^ ")"
    | paren (Concat(reg1, reg2)) = "@(" ^ paren reg1 ^ "," ^ paren reg2 ^ ")"
    | paren (Union(reg1, reg2))  = "+(" ^ paren reg1 ^ "," ^ paren reg2 ^ ")"
in
  fun jforlanValidate s =
        let val reg = Messages.quiet(fn () => fromString s)
        in print "valid"; print PP.newline;
           print(paren reg); print PP.newline
        end
          handle _ =>
                   (print "error"; print PP.newline;
                    (fromString s; ())
                      handle _ => ())
end

(* when called with a regular expression in Forlan syntax, pretty
   prints on the standard output the result of fully abbreviating that
   regular expression *)

fun jforlanPretty s = (print(toString(fromString s)); print PP.newline)

end;
