(******************************** str-set.sml ********************************)

(* Copyright (C) 2001-2018 Alley Stoughton

   The file is part of the Forlan toolset for experimenting with
   formal languages.  See the file COPYING.txt for copying and
   usage restrictions. *)

structure StrSet :> STR_SET =
struct

structure M  = Messages
structure L  = Lex

(******************* Specializations of Functions from Set *******************)

val memb = Set.memb Str.compare

val fromList = Set.fromList Str.compare

val compare = Set.compare Str.compare

val subset = Set.subset Str.compare

val equal = Set.equal Str.compare

val map = fn f => Set.map Str.compare f

val mapFromList = fn f => Set.mapFromList Str.compare f

val union = Set.union Str.compare

val genUnion = Set.genUnion Str.compare

val inter = Set.inter Str.compare

val genInter = Set.genInter Str.compare

val minus = Set.minus Str.compare

(*********************************** Input ***********************************)

fun inpNEStrSet lts =
      let val (x, lts) = Str.inputFromLabToks lts
      in case lts of
              (_, L.Comma) :: lts =>
                let val (ys, lts) = inpNEStrSet lts
                in (union(Set.sing x, ys), lts) end
            | _                   => (Set.sing x, lts)
      end

fun inpStrSet lts =
      if Str.possBeginsWithStr lts then inpNEStrSet lts else (Set.empty, lts)

val inputFromLabToks = inpStrSet

fun fromString s =
      case inpStrSet(L.lexString s) of
           (_,  nil)          => M.cannotHappen()
         | (xs, [(_, L.EOF)]) => xs
         | (_,  lt :: _)      => L.unexpectedTok lt

fun input fil =
      case inpStrSet(L.lexFile fil) of
           (_,  nil)          => M.cannotHappen()
         | (xs, [(_, L.EOF)]) => xs
         | (_,  lt :: _)      => L.unexpectedTok lt

(********************************** Output ***********************************)

fun strListToPPList nil       = nil
  | strListToPPList [x]       = [Str.toPP x]
  | strListToPPList (x :: xs) = PP.comma(Str.toPP x) :: strListToPPList xs

fun toPP xs = PP.block(true, strListToPPList(Set.toList xs))

fun toString xs = PP.toString(toPP xs)

fun output("",  xs) = (print(toString xs); print PP.newline)
  | output(fil, xs) =
      case SOME(TextIO.openOut fil) handle _ => NONE of
           NONE     =>
             M.errorPP
             (fn () =>
                 [PP.fromString "unable", PP.fromString "to",
                  PP.fromString "open", PP.fromString "file:",
                  PP.quote(PP.fromStringSplitEscape fil)])
         | SOME stm =>
             (TextIO.output(stm, toString xs);
              TextIO.output(stm, PP.newline);
              TextIO.closeOut stm)

(****************************** Other Functions ******************************)

fun concat(xs, ys) = map (op @) (Set.times(xs, ys))

fun power(xs, n) =
      if n < 0
      then M.errorString(fn () => ["negative", "argument"])
      else let fun pow 0 = Set.sing nil
                 | pow n = concat(xs, pow(n - 1))
           in pow n end

val rev = map List.rev 

fun prefixes nil       = Set.sing []
  | prefixes (y :: ys) =
      union(Set.sing [], map (fn u => (y :: u)) (prefixes ys))

fun prefix xs = genUnion(Set.mapToList prefixes xs)

fun prefixes x = prefix(Set.sing x)

fun suffix xs = rev(prefix(rev xs))

fun substring xs = suffix(prefix xs)

fun alphabet xs = SymSet.genUnion(Set.mapToList Str.alphabet xs)

fun renameAlphabet(xs, rel) =
      if SymRel.bijectionFromSupersetAvoiding(rel, alphabet xs, Set.empty)
      then let fun renam x = Str.renameAlphabet(x, rel)
           in map renam xs end
      else M.errorString
           (fn () =>
                 ["invalid", "alphabet", "renaming", "for", "string", "set"])

end;
