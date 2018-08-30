(********************************** lex.sml **********************************)

(* Copyright (C) 2001-2012 Alley Stoughton

   The file is part of the Forlan toolset for experimenting with
   formal languages.  See the file COPYING.txt for copying and
   usage restrictions. *)

structure Lex :> LEX =
struct

structure M  = Messages

type basic = char

fun charToBasic c =
      if Char.isDigit c orelse Char.isUpper c orelse Char.isLower c
      then c
      else M.errorPP
           (fn () =>
                 [PP.fromString "bad", PP.fromString "basic",
                  PP.fromString "character",
                  PP.quote(PP.fromString(Char.toString c))])

fun basicToChar c = c

fun basicRank c =
      if Char.isDigit c
        then 0
      else if Char.isLower c
        then 1
      else (* Char.isUpper c *)
           2

fun compareBasic(c, d) =
      case Int.compare(basicRank c, basicRank d) of
           LESS    => LESS
         | EQUAL   => Char.compare(c, d)
         | GREATER => GREATER

datatype sym = BasicSym    of basic
             | CompoundSym of int * sym option list

(* for all x : sym, there is no proper prefix s of symToString x
   such that s = symToString y for some y : sym

   this function introduces no whitespace characters *)

fun symToString(BasicSym c)         = str c
  | symToString(CompoundSym(_, xs)) =
      String.concat
      (["<"] @
       map (fn NONE   => ","
             | SOME x => symToString x)
           xs @
       [">"])

(* sizeSym x = size(symToString s) *)

fun sizeSym(BasicSym _)        = 1
  | sizeSym(CompoundSym(n, _)) = n

local
  datatype sym_tok = CommaST          (* comma *)
                   | BasicST of basic (* basic symbol *)
                   | OpenST           (* "<" *)
                   | ClosST           (* ">" *)

  fun symTokRank CommaST     = 0
    | symTokRank (BasicST _) = 1
    | symTokRank OpenST      = 2
    | symTokRank ClosST      = 3

  type sym_tok_state = sym option list list  (* always non-nil *)

  (* val next : sym_tok_state -> (sym_tok * sym_tok_state)option *)

  fun next [nil]                                     = NONE
    | next (nil :: zss)                              = SOME(ClosST, zss)
    | next ((NONE :: ys) :: zss)                     = SOME(CommaST, ys :: zss)
    | next ((SOME(BasicSym c) :: ys) :: zss)         =
        SOME(BasicST c, ys :: zss)
    | next ((SOME(CompoundSym(_, xs)) :: ys) :: zss) =
        SOME(OpenST, xs :: ys :: zss)
    | next nil                                       = M.cannotHappen()

  fun compareSymTokStates(state, state') =
        case (next state, next state') of
             (NONE,             NONE)               => EQUAL
           | (SOME(tok, state), SOME(tok', state')) =>
               (case Int.compare(symTokRank tok, symTokRank tok') of
                     LESS    => LESS
                   | EQUAL   =>
                       (case (tok, tok') of
                             (BasicST c, BasicST d) =>
                               (case compareBasic(c, d) of
                                     LESS    => LESS
                                   | EQUAL   =>
                                       compareSymTokStates(state, state')
                                   | GREATER => GREATER)
                           | _                      =>
                                 compareSymTokStates(state, state'))
                   | GREATER => GREATER)
           | _                                      => M.cannotHappen()
in
  fun compareSym(a, b) =
        case Int.compare(sizeSym a, sizeSym b) of
             LESS    => LESS
           | EQUAL   => compareSymTokStates([[SOME a]], [[SOME b]])
           | GREATER => GREATER
end

(* PP.toString(symToPP x) and symToString x are identical, except that
   the former may have whitespace, whereas the latter has no
   whitespace *)

fun symToPP(BasicSym c)         = PP.fromString(str c)
  | symToPP(CompoundSym(_, xs)) =
      PP.decorate
      ("<",
       PP.block
       (false,
        map (fn NONE   => PP.fromString ","
              | SOME x => symToPP x)
            xs),
       ">")

datatype sym_top = BasicSymTop    of basic
                 | CompoundSymTop of sym option list

fun symToSymTop(BasicSym c)         = BasicSymTop c
  | symToSymTop(CompoundSym(_, xs)) = CompoundSymTop xs

fun symTopToSym(BasicSymTop c)     = BasicSym c
  | symTopToSym(CompoundSymTop xs) =
      let val size =
                ListAux.sum(map (fn NONE   => 1
                                  | SOME x => sizeSym x)
                                xs) +
                2
      in CompoundSym(size, xs) end

(* tokens *)

datatype tok =
           Bar
         | Comma
         | Dollar
         | Perc
         | Plus
         | Semicolon
         | Star
         | Tilde
         | OpenPar
         | ClosPar
         | SingArr
         | DoubArr
         | Sym of sym
         | Heading of string  (* string should have no whitespace *)
         | EOF

fun equalTok (Bar,       Bar)       = true
  | equalTok (Comma,     Comma)     = true
  | equalTok (Dollar,    Dollar)    = true
  | equalTok (Perc,      Perc)      = true
  | equalTok (Plus,      Plus)      = true
  | equalTok (Semicolon, Semicolon) = true
  | equalTok (Star,      Star)      = true
  | equalTok (Tilde,     Tilde)     = true
  | equalTok (OpenPar,   OpenPar)   = true
  | equalTok (ClosPar,   ClosPar)   = true
  | equalTok (SingArr,   SingArr)   = true
  | equalTok (DoubArr,   DoubArr)   = true
  | equalTok (Sym a,     Sym b)     = compareSym(a, b) = EQUAL
  | equalTok (Heading x, Heading y) = x = y
  | equalTok (EOF,       EOF)       = true
  | equalTok _                      = false

fun tokToMsg Bar         = PP.quote(PP.fromString "|")
  | tokToMsg Comma       = PP.quote(PP.fromString ",")
  | tokToMsg Dollar      = PP.quote(PP.fromString "$")
  | tokToMsg Perc        = PP.quote(PP.fromString "%")
  | tokToMsg Plus        = PP.quote(PP.fromString "+")
  | tokToMsg Semicolon   = PP.quote(PP.fromString ";")
  | tokToMsg Star        = PP.quote(PP.fromString "*")
  | tokToMsg Tilde       = PP.quote(PP.fromString "~")
  | tokToMsg OpenPar     = PP.quote(PP.fromString "(")
  | tokToMsg ClosPar     = PP.quote(PP.fromString ")")
  | tokToMsg SingArr     = PP.quote(PP.fromString "->")
  | tokToMsg DoubArr     = PP.quote(PP.fromString "=>")
  | tokToMsg (Sym a)     = PP.quote(symToPP a)
  | tokToMsg (Heading s) = PP.quote(PP.fromString s)
  | tokToMsg EOF         = PP.fromString "end-of-file"

fun errorNotEOFTerminated() =
      M.errorString
      (fn () => ["labeled", "token", "list", "isn't", "EOF-terminated"])

fun expectedTok(n, t) =
      M.errorPP
      (fn () =>
            [PP.fromString "line", PP.colon(PP.fromString(Int.toString n)),
             tokToMsg t, PP.fromString "expected"])

fun expectedDigit n = 
      M.errorPP
      (fn () =>
            [PP.fromString "line", PP.colon(PP.fromString(Int.toString n)),
             PP.fromString "digit", PP.fromString "expected"])

fun expectedLetter n =
      M.errorPP
      (fn () =>
            [PP.fromString "line", PP.colon(PP.fromString(Int.toString n)),
             PP.fromString "letter", PP.fromString "expected"])

fun expectedLetterOrDigit n =
      M.errorPP
      (fn () =>
            [PP.fromString "line", PP.colon(PP.fromString(Int.toString n)),
             PP.fromString "letter", PP.fromString "or",
             PP.fromString "digit", PP.fromString "expected"])

fun unexpectedTok(n, t) =
      M.errorPP
      (fn () =>
            [PP.fromString "line", PP.colon(PP.fromString(Int.toString n)),
             tokToMsg t, PP.fromString "unexpected"])

fun checkInLabToks(_, nil)             = errorNotEOFTerminated()
  | checkInLabToks(t, ((n, u) :: lts)) =
      if equalTok(u, t) then lts else expectedTok(n, t)

fun error(begLin, endLin, pps) =
      if begLin = endLin
      then M.errorPP
           (fn () =>
                 [PP.fromString "line",
                  PP.colon(PP.fromString(Int.toString begLin))] @
                 pps)
      else M.errorPP
           (fn () =>
                 [PP.fromString "lines",
                  PP.fromString(Int.toString begLin),
                  PP.fromString "-",
                  PP.colon(PP.fromString(Int.toString endLin))] @
                 pps)

fun initState(lin, nil,     lts) = rev((lin, EOF) :: lts)
  | initState(lin, c :: cs, lts) =
      case c of
           #"|"  => initState(lin, cs, (lin, Bar) :: lts)
         | #","  => initState(lin, cs, (lin, Comma) :: lts)
         | #"$"  => initState(lin, cs, (lin, Dollar) :: lts)
         | #"%"  => initState(lin, cs, (lin, Perc) :: lts)
         | #"+"  => initState(lin, cs, (lin, Plus) :: lts)
         | #";"  => initState(lin, cs, (lin, Semicolon) :: lts)
         | #"*"  => initState(lin, cs, (lin, Star) :: lts)
         | #"~"  => initState(lin, cs, (lin, Tilde) :: lts)
         | #"("  => initState(lin, cs, (lin, OpenPar) :: lts)
         | #")"  => initState(lin, cs, (lin, ClosPar) :: lts)
         | #"-"  => singArrState(lin, cs, lts, lin)
         | #"="  => doubArrState(lin, cs, lts, lin)
         | #"<"  => symState(lin, cs, lts, lin, "<", nil, nil)
         | #"{"  => headingState(lin, cs, lts, lin, "{")
         | #"#"  => commentState(lin, cs, lts)
         | #"\n" => initState(lin + 1, cs, lts)
         | _     =>
             if Char.isAlphaNum c
               then initState(lin, cs, (lin, Sym(BasicSym c)) :: lts)
             else if Char.isSpace c
               then initState(lin, cs, lts)
             else error(lin, lin,
                        [PP.fromString "unexpected",
                         PP.fromString "character:",
                         PP.quote(PP.fromString(Char.toString c))])

and singArrState(lin, nil,     _,   beg) =
      error(beg, lin,
            [PP.quote(PP.fromString "->"), PP.fromString "unfinished"])
  | singArrState(lin, c :: cs, lts, beg) =
      if c = #">"
        then initState(lin, cs, (beg, SingArr) :: lts)
      else if c = #"\n"
        then singArrState(lin + 1, cs, lts, beg)
      else if Char.isSpace c
        then singArrState(lin, cs, lts, beg)
      else error(beg, lin,
                 [PP.quote(PP.fromString "->"), PP.fromString "unfinished"])

and doubArrState(lin, nil,     _,   beg) =
      error(beg, lin,
            [PP.quote(PP.fromString "=>"), PP.fromString "unfinished"])
  | doubArrState(lin, c :: cs, lts, beg) =
      if c = #">"
        then initState(lin, cs, (beg, DoubArr) :: lts)
      else if c = #"\n"
        then doubArrState(lin + 1, cs, lts, beg)
      else if Char.isSpace c
        then doubArrState(lin, cs, lts, beg)
      else error(beg, lin,
                 [PP.quote(PP.fromString "=>"),
                  PP.fromString "unfinished"])

and symState(lin, nil,     _,   beg, s, _,  _)   =
      error(beg, lin,
            [PP.fromString "bad", PP.fromString "symbol:",
             PP.quote(PP.fromStringSplitEscape s)])
  | symState(lin, c :: cs, lts, beg, s, xs, yss) =
      if c = #"<"
        then symState(lin, cs, lts, beg, s ^ "<", nil, xs :: yss)
      else if c = #">"
        then if List.null yss
             then initState(lin, cs,
                            (beg,
                             Sym(symTopToSym(CompoundSymTop(List.rev xs)))) ::
                            lts)
             else symState(lin, cs, lts, beg, s ^ ">",
                           SOME(symTopToSym(CompoundSymTop(List.rev xs))) ::
                           hd yss,
                           tl yss)
      else if c = #"\n"
        then symState(lin + 1, cs, lts, beg, s, xs, yss)
      else if Char.isSpace c
        then symState(lin, cs, lts, beg, s, xs, yss)
      else if Char.isAlphaNum c
        then symState(lin, cs, lts, beg, s ^ str c,
                      SOME(BasicSym(charToBasic c)) :: xs, yss)
      else if c = #","
        then symState(lin, cs, lts, beg, s ^ ",", NONE :: xs, yss)
      else error(beg, lin,
                 [PP.fromString "bad", PP.fromString "symbol",
                  PP.fromString "prefix:",
                  PP.quote(PP.fromStringSplitEscape(s ^ str c))])

and headingState(lin, nil,     _,   beg, s) =
      error(beg, lin,
            [PP.fromString "bad", PP.fromString "heading:",
             PP.quote(PP.fromStringSplitEscape s)])
  | headingState(lin, c :: cs, lts, beg, s) =
      if c = #"}"
        then initState(lin, cs, (beg, Heading(s ^ "}")) :: lts)
      else if c = #"\n"
        then headingState(lin + 1, cs, lts, beg, s)
      else if Char.isSpace c
        then headingState(lin, cs, lts, beg, s)
      else if Char.isAlpha c
        then headingState(lin, cs, lts, beg, s ^ str(Char.toLower c))
      else error(beg, lin,
                 [PP.fromString "bad", PP.fromString "heading",
                  PP.fromString "prefix:",
                  PP.quote(PP.fromStringSplitEscape(s ^ str c))])

and commentState(lin, nil,     lts) = rev((lin, EOF) :: lts)
  | commentState(lin, c :: cs, lts) =
      if c = #"\n"
      then initState(lin + 1, cs, lts)
      else commentState(lin, cs, lts)

fun lexString s = initState(1, explode s, nil)

fun lexFile fil =
      let fun findErr fil =
                        M.errorPP
                        (fn () =>
                              [PP.fromString "file",
                               PP.quote(PP.fromStringSplitEscape fil),
                               PP.fromString "not", PP.fromString "found"])

          fun openErr fil =
                M.errorPP
                (fn () =>
                      [PP.fromString "unable", PP.fromString "to",
                       PP.fromString "open", PP.fromString "file:",
                       PP.quote(PP.fromStringSplitEscape fil)])

          fun existsFile fil = OS.FileSys.access(fil, nil)

          fun findFile fil =
                let val path = Params.getSearchPath()
          
                    fun fnd nil           = findErr fil
                      | fnd (dir :: dirs) =
                          let val dirFil = OS.Path.concat(dir, fil)
                          in if existsFile dirFil then dirFil else fnd dirs end
                in if existsFile fil
                     then fil
                   else if OS.Path.isAbsolute fil
                     then findErr fil
                   else fnd path
                end

          fun inputFromStdIn() =
                (print "@ ";
                 case TextIO.inputLine TextIO.stdIn of
                      NONE       =>
                        M.errorString
                        (fn () => ["input", "incorrectly", "terminated"])
                    | SOME ".\n" => ""
                    | SOME s     => s ^ inputFromStdIn())
      in if fil = ""
         then initState(1, explode(inputFromStdIn()), nil)
         else let val fil = findFile fil
                  val stm = TextIO.openIn fil handle _ => openErr fil
                  val lts = initState(1, explode(TextIO.inputAll stm), nil)
              in TextIO.closeIn stm; lts end
      end

end;
