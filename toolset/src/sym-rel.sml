(******************************** sym-rel.sml ********************************)

(* Copyright (C) 2001-2012 Alley Stoughton

   The file is part of the Forlan toolset for experimenting with
   formal languages.  See the file COPYING.txt for copying and
   usage restrictions. *)

structure SymRel :> SYM_REL =
struct

structure M  = Messages
structure L  = Lex

(*********************************** Type ************************************)

type sym_rel = (Sym.sym, Sym.sym)Rel.rel

(******************* Specializations of Functions from Set *******************)

val comparePair = Set.comparePair(Sym.compare, Sym.compare)

val memb = Set.memb comparePair

val fromList = Set.fromList comparePair

val compare = Set.compare comparePair

val subset = Set.subset comparePair

val equal = Set.equal comparePair

val map = fn f => Set.map comparePair f

val mapFromList = fn f => Set.mapFromList comparePair f

val union = Set.union comparePair

val genUnion = Set.genUnion comparePair

val inter = Set.inter comparePair

val genInter = Set.genInter comparePair

val minus = Set.minus comparePair

(******************* Specializations of Functions from Rel *******************)

val domain : sym_rel -> Sym.sym Set.set = Rel.domain Sym.compare

val range : sym_rel -> Sym.sym Set.set = Rel.range Sym.compare

val relationFromTo : sym_rel * Sym.sym Set.set * Sym.sym Set.set -> bool =
      Rel.relationFromTo(Sym.compare, Sym.compare)

val relationOn : sym_rel * Sym.sym Set.set -> bool =
      Rel.relationOn Sym.compare

val apply = Rel.apply(Sym.compare, Sym.compare)

val reflexive = Rel.reflexive Sym.compare

val symmetric = Rel.symmetric Sym.compare

val antisymmetric = Rel.antisymmetric Sym.compare

val transitive = Rel.transitive Sym.compare

val total = Rel.total Sym.compare

val inverse = Rel.inverse(Sym.compare, Sym.compare)

val reflexiveClosure = Rel.reflexiveClosure Sym.compare

val transitiveClosure = Rel.transitiveClosure Sym.compare

val reflexiveTransitiveClosure = Rel.reflexiveTransitiveClosure Sym.compare

val symmetricClosure = Rel.symmetricClosure Sym.compare

val transitiveSymmetricClosure = Rel.symmetricClosure Sym.compare

val reflexiveTransitiveSymmetricClosure =
      Rel.reflexiveTransitiveSymmetricClosure Sym.compare

val compose = Rel.compose(Sym.compare, Sym.compare, Sym.compare)

val function : sym_rel -> bool = Rel.function Sym.compare

val functionFromTo :
      sym_rel * Sym.sym Set.set * Sym.sym Set.set -> bool =
      Rel.functionFromTo(Sym.compare, Sym.compare)

val injection = Rel.injection(Sym.compare, Sym.compare)

val bijectionFromTo :
      sym_rel * Sym.sym Set.set * Sym.sym Set.set -> bool =
      Rel.bijectionFromTo(Sym.compare, Sym.compare)

val bijectionFromAvoiding :
      sym_rel * Sym.sym Set.set * Sym.sym Set.set -> bool =
      Rel.bijectionFromAvoiding(Sym.compare, Sym.compare)

val bijectionFromSupersetAvoiding :
      sym_rel * Sym.sym Set.set * Sym.sym Set.set -> bool =
      Rel.bijectionFromSupersetAvoiding(Sym.compare, Sym.compare)

val applyFunction : sym_rel -> Sym.sym -> Sym.sym =
      Rel.applyFunction Sym.compare

val restrictFunction : sym_rel * Sym.sym Set.set -> sym_rel =
      Rel.restrictFunction(Sym.compare, Sym.compare)

val updateFunction : sym_rel * Sym.sym * Sym.sym -> sym_rel =
      Rel.updateFunction(Sym.compare, Sym.compare)

val mlFunctionToFunction :
      (Sym.sym -> Sym.sym) * Sym.sym Set.set -> sym_rel =
        Rel.mlFunctionToFunction(Sym.compare, Sym.compare)

(*********************************** Input ***********************************)

fun inpSymPair lts =
      let val lts = L.checkInLabToks(L.OpenPar,lts)
          val (a, lts) = Sym.inputFromLabToks lts
          val lts = L.checkInLabToks(L.Comma, lts)
          val (b, lts) = Sym.inputFromLabToks lts
          val lts = L.checkInLabToks(L.ClosPar, lts)
      in ((a, b), lts) end

fun inpNESymRel lts =
      let val (pair, lts) = inpSymPair lts
      in case lts of
              (_, L.Comma) :: lts =>
                let val (pairs, lts) = inpNESymRel lts
                in (union(Set.sing pair, pairs), lts) end
            | _                   => (Set.sing pair, lts)
      end

fun inpSymRel (lts as (_, L.OpenPar) :: _) = inpNESymRel lts
  | inpSymRel lts                          = (Set.empty, lts)

val inputFromLabToks = inpSymRel

fun fromString s =
      case inpSymRel(L.lexString s) of
           (_,   nil)          => M.cannotHappen()
         | (rel, [(_, L.EOF)]) => rel
         | (_,   lt :: _)      => L.unexpectedTok lt

fun input fil =
      case inpSymRel(L.lexFile fil) of
           (_,   nil)          => M.cannotHappen()
         | (rel, [(_, L.EOF)]) => rel
         | (_,   lt :: _)      => L.unexpectedTok lt

(********************************** Output ***********************************)

fun pairToPP(a, b) =
      PP.decorate("(", 
                  PP.block(true,
                           [PP.comma(Sym.toPP a), Sym.toPP b]),
                  ")")

fun pairListToPPList nil             = nil
  | pairListToPPList [pair]          = [pairToPP pair]
  | pairListToPPList (pair :: pairs) =
      PP.comma(pairToPP pair) :: pairListToPPList pairs

fun toPP rel = PP.block(true, pairListToPPList(Set.toList rel))

fun toString rel = PP.toString(toPP rel)

fun output("",  rel) = (print(toString rel); print PP.newline)
  | output(fil, rel) =
      case SOME(TextIO.openOut fil) handle _ => NONE of
           NONE     =>
             M.errorPP
             (fn () =>
                 [PP.fromString "unable", PP.fromString "to",
                  PP.fromString "open", PP.fromString "file:",
                  PP.quote(PP.fromStringSplitEscape fil)])
         | SOME stm =>
             (TextIO.output(stm, toString rel);
              TextIO.output(stm, PP.newline);
              TextIO.closeOut stm)

(****************************** Other Functions ******************************)

fun makeBijectionFromAvoiding(dom, avoid) =
      let fun bracket 0 a = a
            | bracket n a = Sym.fromTop(Sym.Compound[SOME(bracket (n - 1) a)])

          fun findLevel n =
                if Set.isEmpty(SymSet.inter(SymSet.map (bracket n) dom, avoid))
                then n
                else findLevel(n + 1)

          val f = bracket(findLevel 0)
      in mlFunctionToFunction(f, dom) end

end;
