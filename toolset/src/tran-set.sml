(******************************** tran-set.sml ********************************)

(* Copyright (C) 2012 Alley Stoughton

   The file is part of the Forlan toolset for experimenting with
   formal languages.  See the file COPYING.txt for copying and
   usage restrictions. *)

structure TranSet :> TRAN_SET =
struct

structure L  = Lex
structure M  = Messages

(******************* Specializations of Functions from Set *******************)

val memb = Set.memb Tran.compare

val fromList = Set.fromList Tran.compare

val compare = Set.compare Tran.compare

val subset = Set.subset Tran.compare

val equal = Set.equal Tran.compare

val map = fn f => Set.map Tran.compare f

val mapFromList = fn f => Set.mapFromList Tran.compare f

val union = Set.union Tran.compare

val genUnion = Set.genUnion Tran.compare

val inter = Set.inter Tran.compare

val genInter = Set.genInter Tran.compare

val minus = Set.minus Tran.compare

(*********************************** Input ***********************************)

fun inpBarNESymSet lts =
      let val (a, lts) = Sym.inputFromLabToks lts
      in case lts of
              (_, L.Bar) :: lts =>
                let val (bs, lts) = inpBarNESymSet lts
                in (SymSet.union(Set.sing a, bs), lts) end
            | _                 => (Set.sing a, lts)
      end

fun inpTranFam lts =
      let val (q, lts)  = Sym.inputFromLabToks lts
          val lts       = L.checkInLabToks(L.Comma, lts)
          val (x, lts)  = Str.inputFromLabToks lts
          val lts       = L.checkInLabToks(L.SingArr, lts)
          val (rs, lts) = inpBarNESymSet lts
      in (map (fn r => (q, x, r)) rs, lts) end

fun inpNETranFamSet lts =
      let val (trans, lts) = inpTranFam lts
      in case lts of
              (_, L.Semicolon) :: lts =>
                let val (tran's, lts) = inpNETranFamSet lts
                in (union(trans, tran's), lts) end
            | _                       => (trans, lts)
      end

fun inpTranFamSet (lts as (_, L.Sym _) :: _) = inpNETranFamSet lts
  | inpTranFamSet lts                        = (Set.empty, lts)

val inputFromLabToks = inpTranFamSet

fun fromString s =
      case inpTranFamSet(L.lexString s) of
           (_,  nil)          => M.cannotHappen()
         | (bs, [(_, L.EOF)]) => bs
         | (_,  lt :: _)      => L.unexpectedTok lt

fun input fil =
      case inpTranFamSet(L.lexFile fil) of
           (_,  nil)          => M.cannotHappen()
         | (bs, [(_, L.EOF)]) => bs
         | (_,  lt :: _)      => L.unexpectedTok lt

(********************************** Output ***********************************)

fun symListToBarPPList nil       = nil
  | symListToBarPPList [b]       = [Sym.toPP b]
  | symListToBarPPList (b :: bs) =
      PP.decorate("", Sym.toPP b, " |") :: symListToBarPPList bs

fun symSetToBarPP bs = PP.block(true, symListToBarPPList(Set.toList bs))

fun tranFamToPP(q, x, rs) =
      PP.block(true,
               [PP.decorate("",
                            PP.block(true,
                                     [PP.comma(Sym.toPP q), Str.toPP x]),
                            " ->"),
                symSetToBarPP rs])

local
  fun toPPList ((q, x, r) :: (trans as (q', x', r') :: _), ps) =
        let val ps = SymSet.union(Set.sing r, ps)
        in if Sym.equal(q', q) andalso Str.equal(x', x)
           then toPPList(trans, ps)
           else PP.semicolon(tranFamToPP(q, x, ps)) ::
                toPPList(trans, Set.empty)
        end
    | toPPList ([(q, x, r)], ps)                               =
        [tranFamToPP(q, x, SymSet.union(Set.sing r, ps))]
    | toPPList _                                               =
        M.cannotHappen()
in
  fun tranListToPP nil   = PP.empty
    | tranListToPP trans = PP.block(true, toPPList(trans, Set.empty))
end

fun toPP trans = tranListToPP(Set.toList trans)

fun toString trans = PP.toString(toPP trans)

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
