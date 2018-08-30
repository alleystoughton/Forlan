(********************************** prog.sml *********************************)

(* Copyright (C) 2001-2012 Alley Stoughton

   The file is part of the Forlan toolset for experimenting with
   formal languages.  See the file COPYING.txt for copying and
   usage restrictions. *)

structure Prog :> PROG =
struct

structure L = Lex
structure M = Messages

(******************************** Basic Types ********************************)

datatype const =
           True
         | False
         | Nil

fun stringToConstOpt "nil"   = SOME Nil
  | stringToConstOpt "true"  = SOME True
  | stringToConstOpt "false" = SOME False
  | stringToConstOpt _       = NONE

fun constToString Nil   = "nil"
  | constToString True  = "true"
  | constToString False = "false"

fun constKind Nil   = 1
  | constKind True  = 2
  | constKind False = 3

fun compareConst(con, con') = Int.compare(constKind con, constKind con')

datatype oper =
           IsNil
         | IsInt
         | IsNeg
         | IsZero
         | IsPos
         | IsSym
         | IsStr
         | IsPair
         | IsLam
         | Plus
         | Minus
         | Compare
         | Fst
         | Snd
         | ConsSym
         | DeconsSym
         | SymListToStr
         | StrToSymList

fun stringToOperOpt "isNil"        = SOME IsNil
  | stringToOperOpt "isInt"        = SOME IsInt
  | stringToOperOpt "isNeg"        = SOME IsNeg
  | stringToOperOpt "isZero"       = SOME IsZero
  | stringToOperOpt "isPos"        = SOME IsPos
  | stringToOperOpt "isSym"        = SOME IsSym
  | stringToOperOpt "isStr"        = SOME IsStr
  | stringToOperOpt "isPair"       = SOME IsPair
  | stringToOperOpt "isLam"        = SOME IsLam
  | stringToOperOpt "plus"         = SOME Plus
  | stringToOperOpt "minus"        = SOME Minus
  | stringToOperOpt "compare"      = SOME Compare
  | stringToOperOpt "fst"          = SOME Fst
  | stringToOperOpt "snd"          = SOME Snd
  | stringToOperOpt "consSym"      = SOME ConsSym
  | stringToOperOpt "deconsSym"    = SOME DeconsSym
  | stringToOperOpt "symListToStr" = SOME SymListToStr
  | stringToOperOpt "strToSymList" = SOME StrToSymList
  | stringToOperOpt _              = NONE

fun operToString IsNil        = "isNil"
  | operToString IsInt        = "isInt"
  | operToString IsNeg        = "isNeg"
  | operToString IsZero       = "isZero"
  | operToString IsPos        = "isPos"
  | operToString IsSym        = "isSym"
  | operToString IsStr        = "isStr"
  | operToString IsPair       = "isPair"
  | operToString IsLam        = "isLam"
  | operToString Plus         = "plus"
  | operToString Minus        = "minus"
  | operToString Compare      = "compare"
  | operToString Fst          = "fst"
  | operToString Snd          = "snd"
  | operToString ConsSym      = "consSym"
  | operToString DeconsSym    = "deconsSym"
  | operToString SymListToStr = "symListToStr"
  | operToString StrToSymList = "strToSymList"

fun operKind IsNil        = 1
  | operKind IsInt        = 2
  | operKind IsNeg        = 3
  | operKind IsZero       = 4
  | operKind IsPos        = 5
  | operKind IsSym        = 6
  | operKind IsStr        = 7
  | operKind IsPair       = 8
  | operKind IsLam        = 9
  | operKind Plus         = 10
  | operKind Minus        = 11
  | operKind Compare      = 12
  | operKind Fst          = 13
  | operKind Snd          = 14
  | operKind ConsSym      = 15
  | operKind DeconsSym    = 16
  | operKind SymListToStr = 17
  | operKind StrToSymList = 18

fun compareOper(oper, oper') = Int.compare(operKind oper, operKind oper')

datatype concr =
           Var     of Var.var
         | Const   of const
         | Int     of IntInf.int
         | Sym     of Sym.sym
         | Str     of Str.str
         | Pair    of concr * concr
         | Calc    of oper * concr
         | Cond    of concr * concr * concr
         | App     of concr * concr
         | Lam     of Var.var * concr
         | LetSimp of Var.var * concr * concr
         | LetRec  of Var.var * Var.var * concr * concr

type prog = concr

fun fromConcr(concr : concr) : prog = concr

fun toConcr(pr : prog) : concr = pr

val var     = Var
val const   = Const
val int     = Int
val sym     = Sym
val str     = Str
val pair    = Pair
val calc    = Calc
val cond    = Cond
val app     = App
val lam     = Lam
val letSimp = LetSimp
val letRec  = LetRec

(*********************************** Input ***********************************)

fun inpNonNegInt(_,              nil)                         =
      L.errorNotEOFTerminated()
  | inpNonNegInt(m : IntInf.int, lts' as (_, L.Sym a) :: lts) =
      (case Sym.toTop a of
            Sym.Basic b    =>
              let val c = Sym.basicToChar b
              in if Char.isDigit c
                 then inpNonNegInt
                      (m * 10 + IntInf.fromInt(ord c - ord #"0"),
                       lts)
                 else (m, lts')
              end
          | Sym.Compound _ => (m, lts'))
  | inpNonNegInt(m,              lts)                         = (m, lts)

fun inpPosInt nil                   = L.errorNotEOFTerminated()
  | inpPosInt ((n, L.Sym a) :: lts) =
      (case Sym.toTop a of
            Sym.Basic b    =>
              let val c = Sym.basicToChar b
              in if Char.isDigit c andalso c <> #"0"
                 then inpNonNegInt(IntInf.fromInt(ord c - ord #"0"), lts)
                 else L.error
                      (n, n,
                       [PP.fromString "positive", PP.fromString "numeral",
                        PP.fromString "expected"])
              end
          | Sym.Compound _ =>
              L.error
              (n, n,
               [PP.fromString "positive", PP.fromString "numeral",
                PP.fromString "expected"]))
  | inpPosInt ((n, _) :: _)         =
      L.error
      (n, n,
       [PP.fromString "positive", PP.fromString "numeral",
        PP.fromString "expected"])

fun inpInt nil                   = L.errorNotEOFTerminated()
  | inpInt ((n, L.Sym a) :: lts) =
      (case Sym.toTop a of
            Sym.Basic b    =>
              let val c = Sym.basicToChar b
              in if c = #"0"
                   then (0 : IntInf.int, lts)
                 else if Char.isDigit c
                   then inpNonNegInt(IntInf.fromInt(ord c - ord #"0"), lts)
                 else L.error
                      (n, n,
                       [PP.fromString "numeral", PP.fromString "expected"])
              end
          | Sym.Compound _ =>
              L.error
              (n, n, [PP.fromString "numeral", PP.fromString "expected"]))
  | inpInt ((_, L.Tilde) :: lts) =
      let val (n, lts) = inpPosInt lts
      in (~n, lts) end
  | inpInt ((n, _) :: _)         =
      L.error(n, n, [PP.fromString "numeral", PP.fromString "expected"])

fun inpPr lts =
      let val (n, v, lts) = Var.inputLabFromLabToks lts
      in case Var.toString v of
              "var"     =>
                let val lts          = L.checkInLabToks(L.OpenPar, lts)
                    val (_, v', lts) = Var.inputLabFromLabToks lts
                    val lts          = L.checkInLabToks(L.ClosPar, lts)
                in (Var v', lts) end
            | "const"   =>
                let val lts          = L.checkInLabToks(L.OpenPar, lts)
                    val (n, v', lts) = Var.inputLabFromLabToks lts
                    val lts          = L.checkInLabToks(L.ClosPar, lts)
                in case stringToConstOpt(Var.toString v') of
                        NONE     =>
                          L.error(n, n,
                                  [PP.fromString "bad",
                                   PP.fromString "constant:",
                                   PP.quote(Var.toPP v')])
                      | SOME con => (Const con, lts)
                end
            | "int"     =>
                let val lts      = L.checkInLabToks(L.OpenPar, lts)
                    val (n, lts) = inpInt lts
                    val lts      = L.checkInLabToks(L.ClosPar, lts)
                in (Int n, lts) end
            | "sym"     =>
                let val lts      = L.checkInLabToks(L.OpenPar, lts)
                    val (a, lts) = Sym.inputFromLabToks lts
                    val lts      = L.checkInLabToks(L.ClosPar, lts)
                in (Sym a, lts) end
            | "str"     =>
                let val lts      = L.checkInLabToks(L.OpenPar, lts)
                    val (x, lts) = Str.inputFromLabToks lts
                    val lts      = L.checkInLabToks(L.ClosPar, lts)
                in (Str x, lts) end
            | "pair"    =>
                let val lts        = L.checkInLabToks(L.OpenPar, lts)
                    val (pr1, lts) = inpPr lts
                    val lts        = L.checkInLabToks(L.Comma, lts)
                    val (pr2, lts) = inpPr lts
                    val lts        = L.checkInLabToks(L.ClosPar, lts)
                in (Pair(pr1, pr2), lts) end
            | "calc"    =>
                let val lts          = L.checkInLabToks(L.OpenPar, lts)
                    val (n, v', lts) = Var.inputLabFromLabToks lts
                    val lts          = L.checkInLabToks(L.Comma, lts)
                    val (pr, lts)    = inpPr lts
                    val lts          = L.checkInLabToks(L.ClosPar, lts)
                in case stringToOperOpt(Var.toString v') of
                        NONE     =>
                          L.error(n, n,
                                  [PP.fromString "bad",
                                   PP.fromString "operation:",
                                   PP.quote(Var.toPP v')])
                      | SOME oper => (Calc(oper, pr), lts)
                end
            | "cond"    =>
                let val lts        = L.checkInLabToks(L.OpenPar, lts)
                    val (pr1, lts) = inpPr lts
                    val lts        = L.checkInLabToks(L.Comma, lts)
                    val (pr2, lts) = inpPr lts
                    val lts        = L.checkInLabToks(L.Comma, lts)
                    val (pr3, lts) = inpPr lts
                    val lts        = L.checkInLabToks(L.ClosPar, lts)
                in (Cond(pr1, pr2, pr3), lts) end
            | "app"     =>
                let val lts        = L.checkInLabToks(L.OpenPar, lts)
                    val (pr1, lts) = inpPr lts
                    val lts        = L.checkInLabToks(L.Comma, lts)
                    val (pr2, lts) = inpPr lts
                    val lts        = L.checkInLabToks(L.ClosPar, lts)
                in (App(pr1, pr2), lts) end
            | "lam"     =>
                let val lts         = L.checkInLabToks(L.OpenPar, lts)
                    val (_, v, lts) = Var.inputLabFromLabToks lts
                    val lts         = L.checkInLabToks(L.Comma, lts)
                    val (pr, lts)   = inpPr lts
                    val lts         = L.checkInLabToks(L.ClosPar, lts)
                in (Lam(v, pr), lts) end
            | "letSimp" =>
                let val lts         = L.checkInLabToks(L.OpenPar, lts)
                    val (_, v, lts) = Var.inputLabFromLabToks lts
                    val lts         = L.checkInLabToks(L.Comma, lts)
                    val (pr1, lts)  = inpPr lts
                    val lts         = L.checkInLabToks(L.Comma, lts)
                    val (pr2, lts)  = inpPr lts
                    val lts         = L.checkInLabToks(L.ClosPar, lts)
                in (LetSimp(v, pr1, pr2), lts) end
            | "letRec"  =>
                let val lts          = L.checkInLabToks(L.OpenPar, lts)
                    val (_, v1, lts) = Var.inputLabFromLabToks lts
                    val lts          = L.checkInLabToks(L.Comma, lts)
                    val (_, v2, lts) = Var.inputLabFromLabToks lts
                    val lts          = L.checkInLabToks(L.Comma, lts)
                    val (pr1, lts)   = inpPr lts
                    val lts          = L.checkInLabToks(L.Comma, lts)
                    val (pr2, lts)   = inpPr lts
                    val lts          = L.checkInLabToks(L.ClosPar, lts)
                in (LetRec(v1, v2, pr1, pr2), lts) end
            | _         =>
                L.error(n, n,
                        [PP.fromString "bad", PP.fromString "constructor:",
                         PP.quote(Var.toPP v)])
      end

fun fromString s =
      case inpPr(L.lexString s) of
           (pr, [(_, L.EOF)]) => pr
         | (_,   nil)         => M.cannotHappen() 
         | (_,   lt :: _)     => L.unexpectedTok lt

fun input fil =
      case inpPr(L.lexFile fil) of
           (pr, [(_, L.EOF)]) => pr
         | (_,   nil)         => M.cannotHappen() 
         | (_,   lt :: _)     => L.unexpectedTok lt

(*********************************** Output ***********************************)

fun toPP(Var v)                    = PP.decorate("var(", Var.toPP v, ")")
  | toPP(Const con)                =
      PP.decorate("const(", PP.fromString(constToString con), ")")
  | toPP(Int n)                    =
      PP.decorate("int(", PP.fromString(IntInf.toString n), ")")
  | toPP(Sym a)                    = PP.decorate("sym(", Sym.toPP a, ")")
  | toPP(Str x)                    = PP.decorate("str(", Str.toPP x, ")")
  | toPP(Pair(pr1, pr2))           =
      PP.decorate("pair(",
                  PP.block(true, [PP.comma(toPP pr1), toPP pr2]),
                  ")")
  | toPP(Calc(oper, pr))           =
      PP.decorate("calc(",
                  PP.block(true,
                           [PP.comma(PP.fromString(operToString oper)),
                            toPP pr]),
                  ")")
  | toPP(Cond(pr1, pr2, pr3))      =
      PP.decorate("cond(",
                  PP.block(true,
                           [PP.comma(toPP pr1), PP.comma(toPP pr2), toPP pr3]),
                  ")")
  | toPP(App(pr1, pr2))            =
      PP.decorate("app(",
                  PP.block(true, [PP.comma(toPP pr1), toPP pr2]),
                  ")")
  | toPP(Lam(v, pr))               =
      PP.decorate("lam(",
                  PP.block(true, [PP.comma(Var.toPP v), toPP pr]),
                  ")")
  | toPP(LetSimp(v, pr1, pr2))     =
      PP.decorate("letSimp(",
                  PP.block(true,
                           [PP.comma(Var.toPP v), PP.comma(toPP pr1),
                            toPP pr2]),
                  ")")
  | toPP(LetRec(v1, v2, pr1, pr2)) =
      PP.decorate("letRec(",
                  PP.block(true,
                           [PP.comma(Var.toPP v1), PP.comma(Var.toPP v2),
                            PP.comma(toPP pr1), toPP pr2]),
                  ")")

fun toString pr = PP.toString(toPP pr)

fun output("",  pr) = (print(toString pr); print PP.newline)
  | output(fil, pr) =
      case SOME(TextIO.openOut fil) handle _ => NONE of
           NONE     =>
             M.errorPP
             (fn () =>
                   [PP.fromString "unable", PP.fromString "to",
                    PP.fromString "open", PP.fromString "file:",
                    PP.quote(PP.fromStringSplitEscape fil)])
         | SOME stm =>
             (TextIO.output(stm, toString pr);
              TextIO.output(stm, PP.newline);
              TextIO.closeOut stm)

(******************************** Comparision ********************************)

fun kind(Var _)     = 1
  | kind(Const _)   = 2
  | kind(Int _)     = 3
  | kind(Sym _)     = 4
  | kind(Str _)     = 5
  | kind(Pair _)    = 6
  | kind(Calc _)    = 7
  | kind(Cond _)    = 8
  | kind(App _)     = 9
  | kind(Lam _)     = 10
  | kind(LetSimp _) = 11
  | kind(LetRec _)  = 12

fun compare(pr, pr') =
      case Int.compare(kind pr, kind pr') of
           LESS    => LESS
         | EQUAL   =>
             (case (pr, pr') of
                   (Var v,                    Var v')                       =>
                     Var.compare(v, v')
                 | (Const con,                Const con')                   =>
                     compareConst(con, con')
                 | (Int n,                    Int n')                       =>
                     IntInf.compare(n, n')
                 | (Sym a,                    Sym a')                       =>
                     Sym.compare(a, a')
                 | (Str x,                    Str x')                       =>
                     Str.compare(x, x')
                 | (Pair(pr1, pr2),           Pair(pr1', pr2'))             =>
                     (case compare(pr1, pr1') of
                           LESS    => LESS
                         | EQUAL   => compare(pr2, pr2')
                         | GREATER => GREATER)
                 | (Calc(oper, pr),           Calc(oper', pr'))             =>
                     (case compareOper(oper, oper') of
                           LESS    => LESS
                         | EQUAL   => compare(pr, pr')
                         | GREATER => GREATER)
                 | (Cond(pr1, pr2, pr3),      Cond(pr1', pr2', pr3'))       =>
                     (case compare(pr1, pr1') of
                           LESS    => LESS
                         | EQUAL   =>
                             (case compare(pr2, pr2') of
                                   LESS    => LESS
                                 | EQUAL   => compare(pr3, pr3')
                                 | GREATER => GREATER)
                         | GREATER => GREATER)
                 | (App(pr1, pr2),            App(pr1', pr2'))              =>
                     (case compare(pr1, pr1') of
                           LESS    => LESS
                         | EQUAL   => compare(pr2, pr2')
                         | GREATER => GREATER)
                 | (Lam(v, pr),               Lam(v', pr'))                 =>
                     (case Var.compare(v, v') of
                           LESS    => LESS
                         | EQUAL   => compare(pr, pr')
                         | GREATER => GREATER)
                 | (LetSimp(v, pr1, pr2),     LetSimp(v', pr1', pr2'))      =>
                     (case Var.compare(v, v') of
                           LESS    => LESS
                         | EQUAL   =>
                             (case compare(pr1, pr1') of
                                   LESS    => LESS
                                 | EQUAL   => compare(pr2, pr2')
                                 | GREATER => GREATER)
                         | GREATER => GREATER)
                 | (LetRec(v1, v2, pr1, pr2), LetRec(v1', v2', pr1', pr2')) =>
                     (case Var.compare(v1, v1') of
                           LESS    => LESS
                         | EQUAL   =>
                             (case Var.compare(v2, v2') of
                                   LESS    => LESS
                                 | EQUAL   =>
                                     (case compare(pr1, pr1') of
                                           LESS    => LESS
                                         | EQUAL   => compare(pr2, pr2')
                                         | GREATER => GREATER)
                                 | GREATER => GREATER)
                         | GREATER => GREATER)
                 | _                                                        =>
                     M.cannotHappen())
         | GREATER => GREATER

fun equal prPair = compare prPair = EQUAL

(****************************** Programs as Strs ******************************)

(* alphabet of programs as strs is the following symbols plus letters
   and digits *)

val commaSym   = Sym.fromString "<comma>"
val percSym    = Sym.fromString "<perc>"
val tildeSym   = Sym.fromString "<tilde>"
val openParSym = Sym.fromString "<openPar>"
val closParSym = Sym.fromString "<closPar>"
val lessSym    = Sym.fromString "<less>"
val greatSym   = Sym.fromString "<great>"

local
  fun toStr nil          = nil
    | toStr (#"," :: cs) = commaSym :: toStr cs
    | toStr (#"%" :: cs) = percSym :: toStr cs
    | toStr (#"~" :: cs) = tildeSym :: toStr cs
    | toStr (#"(" :: cs) = openParSym :: toStr cs
    | toStr (#")" :: cs) = closParSym :: toStr cs
    | toStr (#"<" :: cs) = lessSym :: toStr cs
    | toStr (#">" :: cs) = greatSym :: toStr cs
    | toStr (c :: cs)    =
        if Char.isSpace c
          then toStr cs
        else if Char.isAlpha c orelse Char.isDigit c
          then Sym.fromString(String.str c) :: toStr cs
        else M.cannotHappen()
in
  val stringToStr = toStr o explode
end

local
  fun conv a =
        if Sym.equal(a, commaSym)
          then #","
        else if Sym.equal(a, percSym)
          then #"%"
        else if Sym.equal(a, tildeSym)
          then #"~"
        else if Sym.equal(a, openParSym)
          then #"("
        else if Sym.equal(a, closParSym)
          then #")"
        else if Sym.equal(a, lessSym)
          then #"<"
        else if Sym.equal(a, greatSym)
          then #">"
        else case Sym.toTop a of
                  Sym.Basic b    => Sym.basicToChar b
                | Sym.Compound _ => raise General.Fail "bad"
in
  fun strToString x = implode(map conv x)
end

val toStr = stringToStr o toString

fun fromStr x =
      M.quiet(fn () => fromString(strToString x))
        handle _ =>
                 M.errorString(fn () => ["illegal", "program"])

(******************************* Tree Functions *******************************)

fun validPath(_,                    nil)     = true
  | validPath(Pair(pr, _),          1 :: ns) = validPath(pr, ns)
  | validPath(Pair(_, pr),          2 :: ns) = validPath(pr, ns)
  | validPath(Calc(_, pr),          1 :: ns) = validPath(pr, ns)
  | validPath(Cond(pr, _, _),       1 :: ns) = validPath(pr, ns)
  | validPath(Cond(_, pr, _),       2 :: ns) = validPath(pr, ns)
  | validPath(Cond(_, _, pr),       3 :: ns) = validPath(pr, ns)
  | validPath(App(pr, _),           1 :: ns) = validPath(pr, ns)
  | validPath(App(_, pr),           2 :: ns) = validPath(pr, ns)
  | validPath(Lam(_, pr),           1 :: ns) = validPath(pr, ns)
  | validPath(LetSimp(_, pr, _),    1 :: ns) = validPath(pr, ns)
  | validPath(LetSimp(_, _, pr),    2 :: ns) = validPath(pr, ns)
  | validPath(LetRec(_, _, pr, _),  1 :: ns) = validPath(pr, ns)
  | validPath(LetRec(_, _, _, pr),  2 :: ns) = validPath(pr, ns)
  | validPath(_,                    _ :: _)  = false

fun height(Var _)                  = 0
  | height(Const _)                = 0
  | height(Int _)                  = 0
  | height(Sym _)                  = 0
  | height(Str _)                  = 0
  | height(Pair(pr1, pr2))         = 1 + Int.max(height pr1, height pr2)
  | height(Calc(_, pr))            = 1 + height pr
  | height(Cond(pr1, pr2, pr3))    =
      1 + Int.max(height pr1, Int.max(height pr2, height pr3))
  | height(App(pr1, pr2))          = 1 + Int.max(height pr1, height pr2)
  | height(Lam(_, pr))             = 1 + height pr
  | height(LetSimp(_, pr1, pr2))   = 1 + Int.max(height pr1, height pr2)
  | height(LetRec(_, _, pr1, pr2)) = 1 + Int.max(height pr1, height pr2)

fun size(Var _)                  = 1
  | size(Const _)                = 1
  | size(Int _)                  = 1
  | size(Sym _)                  = 1
  | size(Str _)                  = 1
  | size(Pair(pr1, pr2))         = 1 + size pr1 + size pr2
  | size(Calc(_, pr))            = 1 + size pr
  | size(Cond(pr1, pr2, pr3))    = 1 + size pr1 + size pr2 + size pr3
  | size(App(pr1, pr2))          = 1 + size pr1 + size pr2
  | size(Lam(_, pr))             = 1 + size pr
  | size(LetSimp(_, pr1, pr2))   = 1 + size pr1 + size pr2
  | size(LetRec(_, _, pr1, pr2)) = 1 + size pr1 + size pr2

fun numLeaves(Var _)                  = 1
  | numLeaves(Const _)                = 1
  | numLeaves(Int _)                  = 1
  | numLeaves(Sym _)                  = 1
  | numLeaves(Str _)                  = 1
  | numLeaves(Pair(pr1, pr2))         = numLeaves pr1 + numLeaves pr2
  | numLeaves(Calc(_, pr))            = numLeaves pr
  | numLeaves(Cond(pr1, pr2, pr3))    =
      numLeaves pr1 + numLeaves pr2 + numLeaves pr3
  | numLeaves(App(pr1, pr2))          = numLeaves pr1 + numLeaves pr2
  | numLeaves(Lam(_, pr))             = numLeaves pr
  | numLeaves(LetSimp(_, pr1, pr2))   = numLeaves pr1 + numLeaves pr2
  | numLeaves(LetRec(_, _, pr1, pr2)) = numLeaves pr1 + numLeaves pr2

fun select(pr,                   nil)     = pr
  | select(Pair(pr, _),          1 :: ns) = select(pr, ns)
  | select(Pair(_, pr),          2 :: ns) = select(pr, ns)
  | select(Calc(_, pr),          1 :: ns) = select(pr, ns)
  | select(Cond(pr, _, _),       1 :: ns) = select(pr, ns)
  | select(Cond(_, pr, _),       2 :: ns) = select(pr, ns)
  | select(Cond(_, _, pr),       3 :: ns) = select(pr, ns)
  | select(App(pr, _),           1 :: ns) = select(pr, ns)
  | select(App(_, pr),           2 :: ns) = select(pr, ns)
  | select(Lam(_, pr),           1 :: ns) = select(pr, ns)
  | select(LetSimp(_, pr, _),    1 :: ns) = select(pr, ns)
  | select(LetSimp(_, _, pr),    2 :: ns) = select(pr, ns)
  | select(LetRec(_, _, pr, _),  1 :: ns) = select(pr, ns)
  | select(LetRec(_, _, _, pr),  2 :: ns) = select(pr, ns)
  | select _                              =
      M.errorString
      (fn () => ["invalid", "path", "for", "program"])

fun update (pr,                       nil,     pr') = pr'
  | update (Pair(pr1, pr2),           1 :: ns, pr') =
      Pair(update(pr1, ns, pr'), pr2)
  | update (Pair(pr1, pr2),           2 :: ns, pr') =
      Pair(pr1, update(pr2, ns, pr'))
  | update (Calc(oper, pr),           1 :: ns, pr') =
      Calc(oper, update(pr, ns, pr'))
  | update (Cond(pr1, pr2, pr3),      1 :: ns, pr') =
      Cond(update(pr1, ns, pr'), pr2, pr3)
  | update (Cond(pr1, pr2, pr3),      2 :: ns, pr') =
      Cond(pr1, update(pr2, ns, pr'), pr3)
  | update (Cond(pr1, pr2, pr3),      3 :: ns, pr') =
      Cond(pr1, pr2, update(pr3, ns, pr'))
  | update (App(pr1, pr2),            1 :: ns, pr') =
      App(update(pr1, ns, pr'), pr2)
  | update (App(pr1, pr2),            2 :: ns, pr') =
      App(pr1, update(pr2, ns, pr'))
  | update (Lam(v, pr),               1 :: ns, pr') =
      Lam(v, update(pr, ns, pr'))
  | update (LetSimp(v, pr1, pr2),     1 :: ns, pr') =
      LetSimp(v, update(pr1, ns, pr'), pr2)
  | update (LetSimp(v, pr1, pr2),     2 :: ns, pr') =
      LetSimp(v, pr1, update(pr2, ns, pr'))
  | update (LetRec(v1, v2, pr1, pr2), 1 :: ns, pr') =
      LetRec(v1, v2, update(pr1, ns, pr'), pr2)
  | update (LetRec(v1, v2, pr1, pr2), 2 :: ns, pr') =
      LetRec(v1, v2, pr1, update(pr2, ns, pr'))
  | update _                                        =
      M.errorString
      (fn () => ["invalid", "path", "for", "program"])

fun maximumLengthPath (Pair(pr1, pr2))         =
      if height pr1 >= height pr2
      then 1 :: maximumLengthPath pr1
      else 2 :: maximumLengthPath pr2
  | maximumLengthPath (Calc(_, pr))            = 1 :: maximumLengthPath pr
  | maximumLengthPath (Cond(pr1, pr2, pr3))    =
      let val n1 = height pr1
          val n2 = height pr2
          val n3 = height pr3
      in if n1 >= n2 andalso n1 >= n3
           then 1 :: maximumLengthPath pr1
         else if n2 >= n3
           then 2 :: maximumLengthPath pr2
         else 3 :: maximumLengthPath pr3
      end
  | maximumLengthPath (App(pr1, pr2))          =
      if height pr1 >= height pr2
      then 1 :: maximumLengthPath pr1
      else 2 :: maximumLengthPath pr2
  | maximumLengthPath (Lam(_, pr))             = 1 :: maximumLengthPath pr
  | maximumLengthPath (LetSimp(_, pr1, pr2))   =
      if height pr1 >= height pr2
      then 1 :: maximumLengthPath pr1
      else 2 :: maximumLengthPath pr2
  | maximumLengthPath (LetRec(_, _, pr1, pr2)) =
      if height pr1 >= height pr2
      then 1 :: maximumLengthPath pr1
      else 2 :: maximumLengthPath pr2
  | maximumLengthPath _                        = nil

fun validLeafPath(pr, ns) =
      validPath(pr, ns) andalso
      case select(pr, ns) of
           Var _   => true
         | Const _ => true
         | Int _   => true
         | Sym _   => true
         | Str _   => true
         | _       => false

(****************************** Closed Programs ******************************)

fun free(Var v)                    = Set.sing v
  | free(Const _)                  = Set.empty
  | free(Int _)                    = Set.empty
  | free(Sym _)                    = Set.empty
  | free(Str _)                    = Set.empty
  | free(Pair(pr1, pr2))           = VarSet.union(free pr1, free pr2)
  | free(Calc(_, pr))              = free pr
  | free(Cond(pr1, pr2, pr3))      =
      VarSet.union(free pr1, VarSet.union(free pr2, free pr3))
  | free(App(pr1, pr2))            = VarSet.union(free pr1, free pr2)
  | free(Lam(v, pr))               = VarSet.minus(free pr, Set.sing v)
  | free(LetSimp(v, pr1, pr2))     =
      VarSet.union(free pr1, VarSet.minus(free pr2, Set.sing v))
  | free(LetRec(v1, v2, pr1, pr2)) =
      VarSet.union(VarSet.minus(VarSet.minus(free pr1, Set.sing v2),
                                Set.sing v1),
                   VarSet.minus(free pr2, Set.sing v1))

type cp = prog

fun toClosed(pr : prog) : cp =
      let val vs = free pr
      in if Set.isEmpty vs
         then pr
         else M.errorPP
              (fn () =>
                    [PP.fromString "program", PP.fromString "has",
                     PP.fromString "free", PP.fromString "variables:",
                     PP.quote(VarSet.toPP vs)])
      end

fun fromClosed(pr : cp) : prog = pr

fun subst(pr', v', pr as Var v)                      =
      if Var.equal(v', v) then pr' else pr
  | subst(_,   _,  pr as Const _)                    = pr
  | subst(_,   _,  pr as Int _)                      = pr
  | subst(_,   _,  pr as Sym _)                      = pr
  | subst(_,   _,  pr as Str _)                      = pr
  | subst(pr', v', Pair(pr1, pr2))                   =
      Pair(subst(pr', v', pr1), subst(pr', v', pr2))
  | subst(pr', v', Calc(oper, pr))                   =
      Calc(oper, subst(pr', v', pr))
  | subst(pr', v', Cond(pr1, pr2, pr3))              =
      Cond(subst(pr', v', pr1), subst(pr', v', pr2), subst(pr', v', pr3))
  | subst(pr', v', App(pr1, pr2))                    =
      App(subst(pr', v', pr1), subst(pr', v', pr2))
  | subst(pr', v', pr'' as Lam(v, pr))               =
      if Var.equal(v', v) then pr'' else Lam(v, subst(pr', v', pr))
  | subst(pr', v', LetSimp(v, pr1, pr2))             =
      LetSimp(v, subst(pr', v', pr1),
              if Var.equal(v', v) then pr2 else subst(pr', v', pr2))
  | subst(pr', v', pr'' as LetRec(v1, v2, pr1, pr2)) =
      if Var.equal(v', v1)
        then pr''
      else if Var.equal(v', v2)
        then LetRec(v1, v2, pr1, subst(pr', v', pr2))
      else LetRec(v1, v2, subst(pr', v', pr1), subst(pr', v', pr2))

(********************************** Values ***********************************)

fun isValue(Var _)      = M.cannotHappen()
  | isValue(Const _)    = true
  | isValue(Int _)      = true
  | isValue(Sym _)      = true
  | isValue(Str _)      = true
  | isValue(Pair(x, y)) = isValue x andalso isValue y
  | isValue(Calc _)     = false
  | isValue(Cond _)     = false
  | isValue(App _)      = false
  | isValue(Lam _)      = true
  | isValue(LetSimp _)  = false
  | isValue(LetRec _)   = false

(******************************** Operations *********************************)

(* val listProgToSymOptsOpt : cp -> Sym.sym option list option *)

fun listProgToSymOptsOpt (Const Nil)           = SOME nil
  | listProgToSymOptsOpt (Pair(Const Nil, pr)) =
      (case listProgToSymOptsOpt pr of
            NONE    => NONE
          | SOME bs => SOME(NONE :: bs))
  | listProgToSymOptsOpt (Pair(Sym a, pr))     =
      (case listProgToSymOptsOpt pr of
            NONE    => NONE
          | SOME bs => SOME(SOME a :: bs))
  | listProgToSymOptsOpt _                     = NONE

(* val consSym : cp -> Sym.sym option *)

fun consSym (Int n) =
      if n >= 1 andalso n <= 10
        then SOME
             (Sym.fromString(String.str(chr(ord #"0" + (IntInf.toInt n - 1)))))
      else if n >= 11 andalso n <= 36
        then SOME
             (Sym.fromString(String.str(chr(ord #"a" + (IntInf.toInt n - 11)))))
      else if n >= 37 andalso n <= 62
        then SOME
             (Sym.fromString(String.str(chr(ord #"A" + (IntInf.toInt n - 37)))))
      else NONE
  | consSym pr      =
      (case listProgToSymOptsOpt pr of
            NONE    => NONE
          | SOME bs => SOME(Sym.fromTop(Sym.Compound bs)))

(* val symOptsToListProg : Sym.sym option list -> cp *)

fun symOptsToListProg nil            = Const Nil
  | symOptsToListProg (NONE :: bs)   = Pair(Const Nil, symOptsToListProg bs)
  | symOptsToListProg (SOME a :: bs) = Pair(Sym a, symOptsToListProg bs)

(* val deconsSym : Sym.sym -> cp *)

fun deconsSym a =
      case Sym.toTop a of
           Sym.Basic b     =>
             let val c = Sym.basicToChar b
             in if Char.isDigit c
                  then Int(IntInf.fromInt(ord c - ord #"0" + 1))
                else if Char.isLower c
                  then Int(IntInf.fromInt(ord c - ord #"a" + 11))
                else (* Char.isUpper c *)
                     Int(IntInf.fromInt(ord c - ord #"A" + 37))
             end
         | Sym.Compound bs => symOptsToListProg bs

(* val listProgToStrOpt : cp -> Str.str option *)

fun listProgToStrOpt (Const Nil)       = SOME nil
  | listProgToStrOpt (Pair(Sym a, pr)) =
      (case listProgToStrOpt pr of
            NONE    => NONE
          | SOME bs => SOME(a :: bs))
  | listProgToStrOpt _                 = NONE

(* val strToListProg : Str.str -> cp *)

fun strToListProg nil       = Const Nil
  | strToListProg (b :: bs) = Pair(Sym b, strToListProg bs)

(* val calculate : oper * cp -> cp option *)

fun calculate (IsNil, Const Nil)            = SOME(Const True)
  | calculate (IsNil, _)                    = SOME(Const False)
  | calculate (IsInt, Int _)                = SOME(Const True)
  | calculate (IsInt, _)                    = SOME(Const False)
  | calculate (IsNeg, Int n)                =
      SOME(Const(if n < 0 then True else False))
  | calculate (IsZero, Int n)               =
      SOME(Const(if n = 0 then True else False))
  | calculate (IsPos, Int n)                =
      SOME(Const(if n > 0 then True else False))
  | calculate (IsSym, Sym _)                = SOME(Const True)
  | calculate (IsSym, _)                    = SOME(Const False)
  | calculate (IsStr, Str _)                = SOME(Const True)
  | calculate (IsStr, _)                    = SOME(Const False)
  | calculate (IsPair, Pair _)              = SOME(Const True)
  | calculate (IsPair, _)                   = SOME(Const False)
  | calculate (IsLam, Lam _)                = SOME(Const True)
  | calculate (IsLam, _)                    = SOME(Const False)
  | calculate (Plus, Pair(Int m, Int n))    = SOME(Int(m + n))
  | calculate (Minus, Pair(Int m, Int n))   = SOME(Int(m - n))
  | calculate (Compare, Pair(Int m, Int n)) =
      (case IntInf.compare(m, n) of
            LESS    => SOME(Int ~1)
          | EQUAL   => SOME(Int 0)
          | GREATER => SOME(Int 1))
  | calculate (Compare, Pair(Sym a, Sym b)) =
      (case Sym.compare(a, b) of
            LESS    => SOME(Int ~1)
          | EQUAL   => SOME(Int 0)
          | GREATER => SOME(Int 1))
  | calculate (Compare, Pair(Str x, Str y)) =
      (case Str.compare(x, y) of
            LESS    => SOME(Int ~1)
          | EQUAL   => SOME(Int 0)
          | GREATER => SOME(Int 1))
  | calculate (Fst, Pair(pr, _))            = SOME pr
  | calculate (Snd, Pair(_, pr))            = SOME pr
  | calculate (ConsSym, pr)                 =
      (case consSym pr of
            NONE   => NONE
          | SOME a => SOME(Sym a))
  | calculate (DeconsSym, Sym a)            = SOME(deconsSym a)
  | calculate (SymListToStr, pr)            =
      (case listProgToStrOpt pr of
            NONE   => NONE
          | SOME x => SOME(Str x))
  | calculate (StrToSymList, Str x)         = SOME(strToListProg x)
  | calculate _                             = NONE

datatype step = Value
              | Error
              | Next of cp

fun step(Var _)                    = M.cannotHappen()
  | step(Const _)                  = Value
  | step(Int _)                    = Value
  | step(Sym _)                    = Value
  | step(Str _)                    = Value
  | step(Pair(pr1, pr2))           =
      (case step pr1 of
            Error     => Error
          | Value     =>
              (case step pr2 of
                    Error     => Error
                  | Value     => Value
                  | Next pr2' => Next(Pair(pr1, pr2')))
          | Next pr1' => Next(Pair(pr1', pr2)))
  | step(Calc(oper, pr))           =
      (case step pr of
            Error    => Error
          | Value    =>
              (case calculate(oper, pr) of
                    NONE     => Error
                  | SOME pr' => Next pr')
          | Next pr' => Next(Calc(oper, pr')))
  | step(Cond(pr1, pr2, pr3))      =
      (case step pr1 of
            Error     => Error
          | Value     =>
              (case pr1 of
                    Const True  => Next pr2
                  | Const False => Next pr3
                  | _           => Error)
          | Next pr1' => Next(Cond(pr1', pr2, pr3)))
  | step(App(pr1, pr2))            =
      (case step pr1 of
            Error     => Error
          | Value     =>
              (case step pr2 of
                    Error     => Error
                  | Value     =>
                      (case pr1 of
                            Lam(v, pr1') => Next(subst(pr2, v, pr1'))
                          | _            => Error)
                  | Next pr2' => Next(App(pr1, pr2')))
          | Next pr1' => Next(App(pr1', pr2)))
  | step(Lam _)                    = Value
  | step(LetSimp(v, pr1, pr2))     =
      (case step pr1 of
            Error     => Error
          | Value     => Next(subst(pr1, v, pr2))
          | Next pr1' => Next(LetSimp(v, pr1', pr2)))
  | step(LetRec(v1, v2, pr1, pr2)) =
      Next
      (subst
       (subst
        (LetRec(v1, v2, pr1, Var v1),
        v1,
        Lam(v2, pr1)),
       v1,
       pr2))

datatype run = Ans      of cp
             | Fail     of cp
             | Intermed of cp

local
  fun rn(pr, 0) = Intermed pr
    | rn(pr, n) =
        (case step pr of
              Error    => Fail pr
            | Value    => Ans pr
            | Next pr' => rn(pr', n - 1))
in
  fun run(pr, n) =
        if n < 0
        then M.errorString
             (fn () =>
                   ["limit", "on", "number", "of", "steps", "must", "be",
                    "at", "least", "0"])
        else rn(pr, n)
end

fun evaluate(pr, n) =
      case run(toClosed pr, n) of
           Ans  pr'     =>
             M.messagePP
             (fn () =>
                   [PP.fromString "terminated", PP.fromString "with",
                    PP.fromString "value", PP.quote(toPP pr')])
         | Fail pr'     =>
             M.messagePP
             (fn () =>
                   [PP.fromString "terminated", PP.fromString "with",
                    PP.fromString "error", PP.quote(toPP pr')])
         | Intermed pr' =>
             M.messagePP
             (fn () =>
                   [PP.fromString "intermediate", PP.fromString "result",
                    PP.quote(toPP pr')])

datatype accept =
           Accept
         | RejectWithFalse
         | RejectOtherwise
         | Unknown of cp

fun accept pr (x, n) =
      let val pr' = App(pr, Str x)
      in case run(pr', n) of
              Ans (Const True)  => Accept
            | Ans (Const False) => RejectWithFalse
            | Ans _             => RejectOtherwise
            | Fail _            => RejectOtherwise
            | Intermed pr''     => Unknown pr''
      end

fun accepted pr =
      let val pr' = toClosed pr
      in fn (x, n) =>
              case accept pr' (x, n) of
                   Accept          =>
                     M.messagePP
                     (fn () =>
                           [PP.fromString "accepted"])
                 | RejectWithFalse =>
                     M.messagePP
                     (fn () =>
                           [PP.fromString "rejected", PP.fromString "with",
                            PP.fromString "false"])
                 | RejectOtherwise =>
                     M.messagePP
                     (fn () =>
                           [PP.fromString "rejected", PP.fromString "not",
                            PP.fromString "with", PP.fromString "false"])
                 | Unknown _       =>
                     M.messagePP
                     (fn () =>
                           [PP.fromString "unknown", PP.fromString "if",
                            PP.fromString "accepted", PP.fromString "or",
                            PP.fromString "rejected"])
      end

(************************** Program Representations ***************************)

fun toRep(Var v)                    =
      Pair(Str(Str.fromString "var"), Str(Str.fromString(Var.toString v)))
  | toRep(Const con)                =
      Pair(Str(Str.fromString "const"), Const con)
  | toRep(Int n)                    =
      Pair(Str(Str.fromString "int"), Int n)
  | toRep(Sym a)                    =
      Pair(Str(Str.fromString "sym"), Sym a)
  | toRep(Str x)                    =
      Pair(Str(Str.fromString "str"), Str x)
  | toRep(Pair(pr1, pr2))           =
      Pair(Str(Str.fromString "pair"), Pair(toRep pr1, toRep pr2))
  | toRep(Calc(oper, pr))           =
      Pair(Str(Str.fromString "calc"),
           Pair(Str(Str.fromString(operToString oper)), toRep pr))
  | toRep(Cond(pr1, pr2, pr3))      =
      Pair(Str(Str.fromString "cond"),
           Pair(toRep pr1,
                Pair(toRep pr2, toRep pr3)))
  | toRep(App(pr1, pr2))            =
      Pair(Str(Str.fromString "app"), Pair(toRep pr1, toRep pr2))
  | toRep(Lam(v, pr))               =
      Pair(Str(Str.fromString "lam"),
           Pair(Str(Str.fromString(Var.toString v)), toRep pr))
  | toRep(LetSimp(v, pr1, pr2))     =
      Pair(Str(Str.fromString "letSimp"),
           Pair(Str(Str.fromString(Var.toString v)),
                Pair(toRep pr1, toRep pr2)))
  | toRep(LetRec(v1, v2, pr1, pr2)) =
      Pair(Str(Str.fromString "letRec"),
           Pair(Str(Str.fromString(Var.toString v1)),
                Pair(Str(Str.fromString(Var.toString v2)),
                     Pair(toRep pr1, toRep pr2))))

local
  fun err() = 
        M.errorString
        (fn () => ["not", "a", "program", "representation"])

  fun varFromStr x =
        M.quiet(fn () => Var.fromString(Str.toString x))
          handle _ => err()

  fun operFromStr x =
        case stringToOperOpt(Str.toString x) of
             NONE      => err()
           | SOME oper => oper
in
  fun fromRep (Pair(Str x, pr)) =
        (case Str.toString x of
              "var"     =>
                (case pr of
                      Str y => Var(varFromStr y)
                    | _     => err())
            | "const"   =>
                (case pr of
                      Const con => Const con
                    | _         => err())
            | "int"     =>
                (case pr of
                      Int n => Int n
                    | _     => err())
            | "sym"     =>
                (case pr of
                      Sym a => Sym a
                    | _     => err())
            | "str"     =>
                (case pr of
                      Str y => Str y
                    | _     => err())
            | "pair"    =>
                (case pr of
                      Pair(pr1, pr2) => Pair(fromRep pr1, fromRep pr2)
                    | _              => err())
            | "calc"    =>
                (case pr of
                      Pair(Str y, pr) => Calc(operFromStr y, fromRep pr)
                    | _               => err())
            | "cond"    =>
                (case pr of
                      Pair(pr1, Pair(pr2, pr3)) =>
                        Cond(fromRep pr1, fromRep pr2, fromRep pr3)
                    | _                         => err())
            | "app"     =>
                (case pr of
                      Pair(pr1, pr2) => App(fromRep pr1, fromRep pr2)
                    | _              => err())
            | "lam"     =>
                (case pr of
                      Pair(Str y, pr) => Lam(varFromStr y, fromRep pr)
                    | _               => err())
            | "letSimp" =>
                (case pr of
                      Pair(Str y, Pair(pr1, pr2)) =>
                        LetSimp(varFromStr y, fromRep pr1, fromRep pr2)
                    | _                           => err())
            | "letRec"  =>
                (case pr of
                      Pair(Str y1, Pair(Str y2, Pair(pr1, pr2))) =>
                        LetRec(varFromStr y1, varFromStr y2,
                               fromRep pr1, fromRep pr2)
                    | _                                          => err())
            | _         => err())
    | fromRep _                 = err()
end

fun isRep pr =
      (M.quiet(fn () => fromRep pr); true)
        handle _ => false

(*************************** Interface with JForlan ***************************)

fun jforlanNew() =
      let val file   = System.makeTempFile()
          val status = System.runJForlan("prog new " ^ file)
      in if OS.Process.isSuccess status
         then let val pr = input file
                  val _  = OS.FileSys.remove file
              in pr end
         else (OS.FileSys.remove file;
               M.errorString
               (fn () => ["creation", "of", "program", "aborted"]))
      end

fun jforlanEdit pr =
      let val file   = System.makeTempFile()
          val _      = output(file, pr)
          val status = System.runJForlan("prog edit " ^ file)
      in if OS.Process.isSuccess status
         then let val pr = input file
                  val _  = OS.FileSys.remove file
              in pr end
         else (OS.FileSys.remove file;
               M.errorString
               (fn () => ["editing", "of", "program", "aborted"]))
      end

(* if called with a string that isn't legal Forlan syntax for a
   program tree, prints "error" on a line, followed by one or more
   lines of parsing errors

   if called with a string that is legal Forlan syntax for a program
   tree, prints "valid" on a line, followed by a single line
   consisting of the program tree, with no whitespace *)

fun jforlanValidate s =
      let val prog = Messages.quiet(fn () => fromString s)
      in print "valid"; print PP.newline;
         print(StringAux.removeWhiteSpace(toString prog));
         print PP.newline
      end
        handle _ =>
                 (print "error"; print PP.newline;
                  (fromString s; ())
                    handle _ => ())

(* when called with a program tree in Forlan syntax, pretty
   prints on the standard output that program tree *)

fun jforlanPretty s = (print(toString(fromString s)); print PP.newline)

end;
