(* sym-rel.sig
 *
 * COPYRIGHT (c) 2012 Alley Stoughton.
 *
 * extracted from sym-rel.mldoc
 *)

signature SYM_REL =
  sig
    type sym_rel = (Sym.sym, Sym.sym) Rel.rel
    val comparePair : (Sym.sym * Sym.sym) Sort.total_ordering
    val memb : (Sym.sym * Sym.sym) * sym_rel -> bool
    val fromList : (Sym.sym * Sym.sym) list -> sym_rel
    val compare : sym_rel Sort.total_ordering
    val subset : sym_rel * sym_rel -> bool
    val equal : sym_rel * sym_rel -> bool
    val map : ('a -> Sym.sym * Sym.sym) -> 'a Set.set -> sym_rel
    val mapFromList : ('a -> Sym.sym * Sym.sym) -> 'a list -> sym_rel
    val union : sym_rel * sym_rel -> sym_rel
    val genUnion : sym_rel list -> sym_rel
    val inter : sym_rel * sym_rel -> sym_rel
    val genInter : sym_rel list -> sym_rel
    val minus : sym_rel * sym_rel -> sym_rel
    val domain : sym_rel -> Sym.sym Set.set
    val range : sym_rel -> Sym.sym Set.set
    val relationFromTo : sym_rel * Sym.sym Set.set * Sym.sym Set.set -> bool
    val relationOn : sym_rel * Sym.sym Set.set -> bool
    val apply : sym_rel * Sym.sym Set.set -> Sym.sym Set.set
    val reflexive : sym_rel * Sym.sym Set.set -> bool
    val symmetric : sym_rel -> bool
    val antisymmetric : sym_rel -> bool
    val transitive : sym_rel -> bool
    val total : sym_rel * Sym.sym Set.set -> bool
    val inverse : sym_rel -> sym_rel
    val reflexiveClosure : sym_rel * Sym.sym Set.set -> sym_rel
    val transitiveClosure : sym_rel -> sym_rel
    val reflexiveTransitiveClosure : sym_rel * Sym.sym Set.set -> sym_rel
    val symmetricClosure : sym_rel -> sym_rel
    val transitiveSymmetricClosure : sym_rel -> sym_rel
    val reflexiveTransitiveSymmetricClosure : sym_rel * Sym.sym Set.set
                                                -> sym_rel
    val compose : sym_rel * sym_rel -> sym_rel
    val function : sym_rel -> bool
    val functionFromTo : sym_rel * Sym.sym Set.set * Sym.sym Set.set -> bool
    val injection : sym_rel -> bool
    val bijectionFromTo : sym_rel * Sym.sym Set.set * Sym.sym Set.set -> bool
    val bijectionFromAvoiding : sym_rel * Sym.sym Set.set * Sym.sym Set.set
                                  -> bool
    val bijectionFromSupersetAvoiding : sym_rel
                                          * Sym.sym Set.set
                                          * Sym.sym Set.set -> bool
    val applyFunction : sym_rel -> Sym.sym -> Sym.sym
    val restrictFunction : sym_rel * Sym.sym Set.set -> sym_rel
    val updateFunction : sym_rel * Sym.sym * Sym.sym -> sym_rel
    val mlFunctionToFunction : (Sym.sym -> Sym.sym) * Sym.sym Set.set
                                 -> sym_rel
    val inputFromLabToks : (int * Lex.tok) list
                             -> sym_rel * (int * Lex.tok) list
    val fromString : string -> sym_rel
    val input : string -> sym_rel
    val toPP : sym_rel -> PP.pp
    val toString : sym_rel -> string
    val output : string * sym_rel -> unit
    val makeBijectionFromAvoiding : Sym.sym Set.set * Sym.sym Set.set
                                      -> sym_rel
  end
