(******************************** str-set.sml ********************************)

(* Copyright (C) 2012 Alley Stoughton

   The file is part of the Forlan toolset for experimenting with
   formal languages.  See the file COPYING.txt for copying and
   usage restrictions. *)

structure VarSet :> VAR_SET =
struct

structure M  = Messages
structure L  = Lex

(******************* Specializations of Functions from Set *******************)

val memb = Set.memb Var.compare

val fromList = Set.fromList Var.compare

val compare = Set.compare Var.compare

val subset = Set.subset Var.compare

val equal = Set.equal Var.compare

val map = fn f => Set.map Var.compare f

val mapFromList = fn f => Set.mapFromList Var.compare f

val union = Set.union Var.compare

val genUnion = Set.genUnion Var.compare

val inter = Set.inter Var.compare

val genInter = Set.genInter Var.compare

val minus = Set.minus Var.compare

(*********************************** Input ***********************************)

fun inpNEVarSet lts =
      let val (_, v, lts) = Var.inputLabFromLabToks lts
      in case lts of
              (_, L.Comma) :: lts =>
                let val (vs, lts) = inpNEVarSet lts
                in (union(Set.sing v, vs), lts) end
            | _                   => (Set.sing v, lts)
      end

fun inpVarSet lts =
      if Var.beginsWithVar lts then inpNEVarSet lts else (Set.empty, lts)

val inputFromLabToks = inpVarSet

fun fromString s =
      case inpVarSet(L.lexString s) of
           (_,  nil)          => M.cannotHappen()
         | (vs, [(_, L.EOF)]) => vs
         | (_,  lt :: _)      => L.unexpectedTok lt

fun input fil =
      case inpVarSet(L.lexFile fil) of
           (_,  nil)          => M.cannotHappen()
         | (vs, [(_, L.EOF)]) => vs
         | (_,  lt :: _)      => L.unexpectedTok lt

(********************************** Output ***********************************)

fun varListToPPList nil       = nil
  | varListToPPList [v]       = [Var.toPP v]
  | varListToPPList (v :: vs) = PP.comma(Var.toPP v) :: varListToPPList vs

fun toPP vs = PP.block(true, varListToPPList(Set.toList vs))

fun toString vs = PP.toString(toPP vs)

fun output("",  vs) = (print(toString vs); print PP.newline)
  | output(fil, vs) =
      case SOME(TextIO.openOut fil) handle _ => NONE of
           NONE     =>
             M.errorPP
             (fn () =>
                 [PP.fromString "unable", PP.fromString "to",
                  PP.fromString "open", PP.fromString "file:",
                  PP.quote(PP.fromStringSplitEscape fil)])
         | SOME stm =>
             (TextIO.output(stm, toString vs);
              TextIO.output(stm, PP.newline);
              TextIO.closeOut stm)

end;
