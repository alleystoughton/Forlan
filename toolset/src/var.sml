(********************************** var.sml **********************************)

(* Copyright (C) 2001-2012 Alley Stoughton

   The file is part of the Forlan toolset for experimenting with
   formal languages.  See the file COPYING.txt for copying and
   usage restrictions. *)

structure Var :> VAR =
struct

structure M  = Messages
structure L  = Lex

(*********************************** Type ************************************)

(* nonempty sequence of letters and digits, beginning with letter *)

type var = string

(*********************************** Input ***********************************)

fun beginsWithVar ((_, L.Sym a) :: _) =
      (case Sym.toTop a of
            Sym.Basic b    => Char.isAlpha(Sym.basicToChar b)
          | Sym.Compound _ => false)
  | beginsWithVar _                   = false

fun inpLabVar'(_, _, nil)                         = L.errorNotEOFTerminated()
  | inpLabVar'(n, s, lts' as (_, L.Sym a) :: lts) =
      (case Sym.toTop a of
            Sym.Basic b    =>
              let val c = Sym.basicToChar b
              in if Char.isAlphaNum c
                 then inpLabVar'(n, s ^ String.str c, lts)
                 else (n, s, lts')
              end
          | Sym.Compound _ => (n, s, lts'))
  | inpLabVar'(n, s, lts)                         = (n, s, lts)

fun inpLabVar nil                    = L.errorNotEOFTerminated()
  | inpLabVar ((n, L.Sym a) :: lts)  =
      (case Sym.toTop a of
            Sym.Basic b    =>
              let val c = Sym.basicToChar b
              in if Char.isAlpha c
                 then inpLabVar'(n, String.str c, lts)
                 else L.expectedLetter n
              end
          | Sym.Compound _ => L.expectedLetter n)
  | inpLabVar ((n, _) :: _)          = L.expectedLetter n

val inputLabFromLabToks = inpLabVar

fun fromString s =
      case inpLabVar(L.lexString s) of
           (_, x, [(_, L.EOF)]) => x
         | (_, _, nil)          => M.cannotHappen() 
         | (_, _, lt :: _)      => L.unexpectedTok lt

fun input fil =
      case inpLabVar(L.lexFile fil) of
           (_, x, [(_, L.EOF)]) => x
         | (_, _, nil)          => M.cannotHappen() 
         | (_, _, lt :: _)      => L.unexpectedTok lt

(********************************** Output ***********************************)

val toPP = PP.fromString

fun toString v = PP.toString(toPP v)

fun output("",  v) = (print(toString v); print PP.newline)
  | output(fil, v) =
      case SOME(TextIO.openOut fil) handle _ => NONE of
           NONE     =>
             M.errorPP
             (fn () =>
                   [PP.fromString "unable", PP.fromString "to",
                    PP.fromString "open", PP.fromString "file",
                    PP.quote(PP.fromStringSplitEscape fil)])
         | SOME stm =>
             (TextIO.output(stm, toString v);
              TextIO.output(stm, PP.newline);
              TextIO.closeOut stm)

(****************************** Other Functions ******************************)

fun compare(v1, v2) =
      case Int.compare(size v1, size v2) of
           LESS    => LESS
         | EQUAL   => String.compare(v1, v2)
         | GREATER => GREATER

fun equal(v1 : var, v2) = v1 = v2

end;
