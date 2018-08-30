(********************************** sym.sml **********************************)

(* Copyright (C) 2012 Alley Stoughton

   The file is part of the Forlan toolset for experimenting with
   formal languages.  See the file COPYING.txt for copying and
   usage restrictions. *)

structure Sym :> SYM =
struct

structure L  = Lex
structure M  = Messages

(*********************************** Types ***********************************)

type basic = L.basic

val charToBasic = L.charToBasic

val basicToChar = L.basicToChar

type sym = L.sym

datatype top = Basic of basic
             | Compound of sym option list

fun fromTop(Basic c)     = L.symTopToSym(L.BasicSymTop c)
  | fromTop(Compound xs) = L.symTopToSym(L.CompoundSymTop xs)

fun toTop a =
      case L.symToSymTop a of
           L.BasicSymTop c     => Basic c
         | L.CompoundSymTop xs => Compound xs

(*********************************** Input ***********************************)

fun inputFromLabToks nil                   =
      M.errorString
      (fn () => ["labeled", "token", "list", "isn't", "EOF-terminated"])
  | inputFromLabToks ((_, L.Sym a) :: lts) = (a, lts)
  | inputFromLabToks (lt :: _)             = L.unexpectedTok lt

fun fromString s =
      case inputFromLabToks(L.lexString s) of
           (a, [(_, EOF)]) => a
         | (_, nil)        => M.cannotHappen() 
         | (_, lt :: _)    => L.unexpectedTok lt

fun input fil =
      case inputFromLabToks(L.lexFile fil) of
           (a, [(_, EOF)]) => a
         | (_, nil)        => M.cannotHappen() 
         | (_, lt :: _)    => L.unexpectedTok lt

(********************************** Output ***********************************)

val toPP = L.symToPP

val toString = PP.toString o toPP

fun output("",  a) = (print(toString a); print PP.newline)
  | output(fil, a) =
      case SOME(TextIO.openOut fil) handle _ => NONE of
           NONE     =>
             M.errorPP
             (fn () =>
                   [PP.fromString "unable", PP.fromString "to",
                    PP.fromString "open", PP.fromString "file:",
                    PP.quote(PP.fromStringSplitEscape fil)])
         | SOME stm =>
             (TextIO.output(stm, toString a);
              TextIO.output(stm, PP.newline);
              TextIO.closeOut stm)

(****************************** Other Functions ******************************)

val compare = L.compareSym

fun equal(a1, a2) = compare(a1, a2) = EQUAL

fun size a = L.sizeSym a

(*************************** Interface with JForlan ***************************)

(* if called with a string that isn't legal Forlan syntax for a
   symbol, prints "error" on a line, followed by one or more lines of
   parsing errors

   if called with a string that is legal Forlan syntax for a symbol,
   prints "valid" on a line, followed by a single line consisting of
   the symbol, with no whitespace *)

fun jforlanValidate s =
      let val a = Messages.quiet(fn () => fromString s)
      in print "valid"; print PP.newline;
         print(StringAux.removeWhiteSpace(toString a));
         print PP.newline
      end
        handle _ =>
                 (print "error"; print PP.newline;
                  (fromString s; ())
                    handle _ => ())

(* when called with a symbol in Forlan syntax, pretty prints on the
   standard output that symbol *)

fun jforlanPretty s = (print(toString(fromString s)); print PP.newline)

end;
