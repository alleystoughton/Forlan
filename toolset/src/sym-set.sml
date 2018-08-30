(******************************** sym-set.sml ********************************)

(* Copyright (C) 2012 Alley Stoughton

   The file is part of the Forlan toolset for experimenting with
   formal languages.  See the file COPYING.txt for copying and
   usage restrictions. *)

structure SymSet :> SYM_SET =
struct

structure M  = Messages
structure L  = Lex

(******************* Specializations of Functions from Set *******************)

val memb = Set.memb Sym.compare

val fromList = Set.fromList Sym.compare

val compare = Set.compare Sym.compare

val subset = Set.subset Sym.compare

val equal = Set.equal Sym.compare

val map = fn f => Set.map Sym.compare f

val mapFromList = fn f => Set.mapFromList Sym.compare f

val union = Set.union Sym.compare

val genUnion = Set.genUnion Sym.compare

val inter = Set.inter Sym.compare

val genInter = Set.genInter Sym.compare

val minus = Set.minus Sym.compare

(*********************************** Input ***********************************)

fun inpNESymSet lts =
      let val (a, lts) = Sym.inputFromLabToks lts
      in case lts of
              (_, L.Comma) :: lts =>
                let val (bs, lts) = inpNESymSet lts
                in (union(Set.sing a, bs), lts) end
            | _                   => (Set.sing a, lts)
      end

fun inpSymSet (lts as (_, L.Sym _) :: _) = inpNESymSet lts
  | inpSymSet lts                        = (Set.empty, lts)

val inputFromLabToks = inpSymSet

fun fromString s =
      case inpSymSet(L.lexString s) of
           (_,  nil)          => M.cannotHappen()
         | (bs, [(_, L.EOF)]) => bs
         | (_,  lt :: _)      => L.unexpectedTok lt

fun input fil =
      case inpSymSet(L.lexFile fil) of
           (_,  nil)          => M.cannotHappen()
         | (bs, [(_, L.EOF)]) => bs
         | (_,  lt :: _)      => L.unexpectedTok lt

(********************************** Output ***********************************)

fun symListToPPList nil       = nil
  | symListToPPList [b]       = [Sym.toPP b]
  | symListToPPList (b :: bs) = PP.comma(Sym.toPP b) :: symListToPPList bs

fun toPP bs = PP.block(true, symListToPPList(Set.toList bs))

fun toString bs = PP.toString(toPP bs)

fun output("",  bs) = (print(toString bs); print PP.newline)
  | output(fil, bs) =
      case SOME(TextIO.openOut fil) handle _ => NONE of
           NONE     =>
             M.errorPP
             (fn () =>
                   [PP.fromString "unable", PP.fromString "to",
                    PP.fromString "open", PP.fromString "file:",
                    PP.quote(PP.fromStringSplitEscape fil)])
         | SOME stm =>
             (TextIO.output(stm, toString bs);
              TextIO.output(stm, PP.newline);
              TextIO.closeOut stm)

end;
