(********************************** str.sml **********************************)

(* Copyright (C) 2001-2012 Alley Stoughton

   The file is part of the Forlan toolset for experimenting with
   formal languages.  See the file COPYING.txt for copying and
   usage restrictions. *)

structure Str :> STR =
struct

structure M  = Messages
structure L  = Lex

(*********************************** Type ************************************)

type str = Sym.sym list

(*********************************** Input ***********************************)

fun possBeginsWithStr ((_, L.Perc) :: _)  = true
  | possBeginsWithStr ((_, L.Sym _) :: _) = true
  | possBeginsWithStr _                   = false

fun inpStr'(_, nil)                  = Lex.errorNotEOFTerminated()
  | inpStr'(xs, (_, L.Sym a) :: lts) = inpStr'(a :: xs, lts)
  | inpStr'(xs, lts)                 = (rev xs, lts)

fun inpStr nil                   = Lex.errorNotEOFTerminated()
  | inpStr ((_, L.Perc) :: lts)  = (nil, lts)
  | inpStr ((_, L.Sym a) :: lts) = inpStr'([a], lts)
  | inpStr (lt :: _)             = L.unexpectedTok lt

val inputFromLabToks = inpStr

fun fromString s =
      case inpStr(L.lexString s) of
           (x, [(_, L.EOF)]) => x
         | (_, nil)          => M.cannotHappen() 
         | (_, lt :: _)      => L.unexpectedTok lt

fun input fil =
      case inpStr(L.lexFile fil) of
           (x, [(_, L.EOF)]) => x
         | (_, nil)          => M.cannotHappen() 
         | (_, lt :: _)      => L.unexpectedTok lt

(********************************** Output ***********************************)

fun toPP nil = PP.fromString "%"
  | toPP x   = PP.block(false, map Sym.toPP x)

fun toString x = PP.toString(toPP x)

fun output("",  x) = (print(toString x); print PP.newline)
  | output(fil, x) =
      case SOME(TextIO.openOut fil) handle _ => NONE of
           NONE     =>
             M.errorPP
             (fn () =>
                   [PP.fromString "unable", PP.fromString "to",
                    PP.fromString "open", PP.fromString "file:",
                    PP.quote(PP.fromStringSplitEscape fil)])
         | SOME stm =>
             (TextIO.output(stm, toString x);
              TextIO.output(stm, PP.newline);
              TextIO.closeOut stm)

(****************************** Other Functions ******************************)

val last = List.last

val allButLast = ListAux.allButLast

val compare = Set.compareList Sym.compare

fun equal(bs, cs) = compare(bs, cs) = EQUAL

val alphabet = SymSet.fromList

fun renameAlphabet(xs, rel) =
      if SymRel.bijectionFromSupersetAvoiding(rel, alphabet xs, Set.empty)
      then let val apply = SymRel.applyFunction rel
           in map apply xs end
      else M.errorString
           (fn () => ["invalid", "alphabet", "renaming", "for", "string"])

fun prefix(nil,     _)       = true
  | prefix(_,       nil)     = false
  | prefix(b :: bs, c :: cs) = Sym.equal(b, c) andalso prefix(bs, cs)

fun suffix(bs, cs) =
      equal(bs, List.drop(cs, length cs - length bs))
        handle _ => false

fun substr(nil,             _)       = true
  | substr(_,               nil)     = false
  | substr(b_bs as b :: bs, c :: cs) =
      if Sym.equal(b, c)
      then prefix(bs, cs) orelse substr(b_bs, cs)
      else substr(b_bs, cs)

fun power(bs, n) =
      if n < 0
      then M.errorString(fn () => ["negative", "argument"])
      else let fun pow 0 = nil
                 | pow n = bs @ pow(n - 1)
           in pow n end

fun removePrefix(bs, cs) =
      if prefix(bs, cs)
      then SOME(List.drop(cs, length bs))
      else NONE

fun removeSuffix(bs, cs) =
      if suffix(bs, cs)
      then SOME(List.take(cs, length cs - length bs))
      else NONE

(*************************** Interface with JForlan ***************************)

(* if called with a string that isn't legal Forlan syntax for a
   string, prints "error" on a line, followed by one or more lines of
   parsing errors

   if called with a string that is legal Forlan syntax for a string,
   prints "valid" on a line, followed by a single line consisting of
   the string, with no whitespace *)

fun jforlanValidate s =
      let val x = Messages.quiet(fn () => fromString s)
      in print "valid"; print PP.newline;
         print(StringAux.removeWhiteSpace(toString x));
         print PP.newline
      end
        handle _ =>
                 (print "error"; print PP.newline;
                  (fromString s; ())
                    handle _ => ())

(* when called with a string in Forlan syntax, pretty prints on the
   standard output that Forlan string *)

fun jforlanPretty s = (print(toString(fromString s)); print PP.newline)

end;
