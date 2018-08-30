(******************************** prod-set.sml ********************************)

(* Copyright (C) 2012 Alley Stoughton

   The file is part of the Forlan toolset for experimenting with
   formal languages.  See the file COPYING.txt for copying and
   usage restrictions. *)

structure ProdSet :> PROD_SET =
struct

structure L  = Lex
structure M  = Messages

(******************* Specializations of Functions from Set *******************)

val memb = Set.memb Prod.compare

val fromList = Set.fromList Prod.compare

val compare = Set.compare Prod.compare

val subset = Set.subset Prod.compare

val equal = Set.equal Prod.compare

val map = fn f => Set.map Prod.compare f

val mapFromList = fn f => Set.mapFromList Prod.compare f

val union = Set.union Prod.compare

val genUnion = Set.genUnion Prod.compare

val inter = Set.inter Prod.compare

val genInter = Set.genInter Prod.compare

val minus = Set.minus Prod.compare

(*********************************** Input ***********************************)

fun inpBarNEStrSet lts =
      let val (x, lts) = Str.inputFromLabToks lts
      in case lts of
              (_, L.Bar) :: lts =>
                let val (ys, lts) = inpBarNEStrSet lts
                in (StrSet.union(Set.sing x, ys), lts) end
            | _                 => (Set.sing x, lts)
      end

fun inpProdFam lts =
      let val (q, lts)  = Sym.inputFromLabToks lts
          val lts       = L.checkInLabToks(L.SingArr, lts)
          val (xs, lts) = inpBarNEStrSet lts
      in (map (fn x => (q, x)) xs, lts) end

fun inpNEProdFamSet lts =
      let val (prods, lts) = inpProdFam lts
      in case lts of
              (_, L.Semicolon) :: lts =>
                let val (prod's, lts) = inpNEProdFamSet lts
                in (union(prods, prod's), lts) end
            | _                       => (prods, lts)
      end

fun inpProdFamSet (lts as (_, L.Sym _) :: _) = inpNEProdFamSet lts
  | inpProdFamSet lts                        = (Set.empty, lts)

val inputFromLabToks = inpProdFamSet

fun fromString s =
      case inpProdFamSet(L.lexString s) of
           (_,  nil)          => M.cannotHappen()
         | (bs, [(_, L.EOF)]) => bs
         | (_,  lt :: _)      => L.unexpectedTok lt

fun input fil =
      case inpProdFamSet(L.lexFile fil) of
           (_,  nil)          => M.cannotHappen()
         | (bs, [(_, L.EOF)]) => bs
         | (_,  lt :: _)      => L.unexpectedTok lt

(********************************** Output ***********************************)

fun strListToBarPPList nil       = nil
  | strListToBarPPList [x]       = [Str.toPP x]
  | strListToBarPPList (x :: xs) =
      PP.decorate("", Str.toPP x, " |") :: strListToBarPPList xs

fun strSetToBarPP xs = PP.block(true, strListToBarPPList(Set.toList xs))

fun prodFamToPP(q, xs) =
      PP.block(true,
               [PP.decorate("", Sym.toPP q, " ->"),
                strSetToBarPP xs])

local
  fun toPPList ((q, x) :: (prods as (q', x') :: _), ys) =
        let val ys = StrSet.union(Set.sing x, ys)
        in if Sym.equal(q', q)
           then toPPList(prods, ys)
           else PP.semicolon(prodFamToPP(q, ys)) :: toPPList(prods, Set.empty)
        end
    | toPPList ([(q, x)], ys)                           =
        [prodFamToPP(q, StrSet.union(Set.sing x, ys))]
    | toPPList _                                        =
        M.cannotHappen()
in
  fun prodListToPP nil   = PP.empty
    | prodListToPP prods = PP.block(true, toPPList(prods, Set.empty))
end

fun toPP prods = prodListToPP(Set.toList prods)

fun toString prods = PP.toString(toPP prods)

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
