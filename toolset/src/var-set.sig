(* var-set.sig
 *
 * COPYRIGHT (c) 2012 Alley Stoughton.
 *
 * extracted from var-set.mldoc
 *)

signature VAR_SET =
  sig
    val memb : Var.var * Var.var Set.set -> bool
    val fromList : Var.var list -> Var.var Set.set
    val compare : Var.var Set.set Sort.total_ordering
    val subset : Var.var Set.set * Var.var Set.set -> bool
    val equal : Var.var Set.set * Var.var Set.set -> bool
    val map : ('a -> Var.var) -> 'a Set.set -> Var.var Set.set
    val mapFromList : ('a -> Var.var) -> 'a list -> Var.var Set.set
    val union : Var.var Set.set * Var.var Set.set -> Var.var Set.set
    val genUnion : Var.var Set.set list -> Var.var Set.set
    val inter : Var.var Set.set * Var.var Set.set -> Var.var Set.set
    val genInter : Var.var Set.set list -> Var.var Set.set
    val minus : Var.var Set.set * Var.var Set.set -> Var.var Set.set
    val inputFromLabToks : (int * Lex.tok) list
                             -> Var.var Set.set * (int * Lex.tok) list
    val fromString : string -> Var.var Set.set
    val input : string -> Var.var Set.set
    val toPP : Var.var Set.set -> PP.pp
    val toString : Var.var Set.set -> string
    val output : string * Var.var Set.set -> unit
  end
