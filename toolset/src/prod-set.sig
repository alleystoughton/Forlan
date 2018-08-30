(* prod-set.sig
 *
 * COPYRIGHT (c) 2012 Alley Stoughton.
 *
 * extracted from prod-set.mldoc
 *)

signature PROD_SET =
  sig
    val memb : Prod.prod * Prod.prod Set.set -> bool
    val fromList : Prod.prod list -> Prod.prod Set.set
    val compare : Prod.prod Set.set Sort.total_ordering
    val subset : Prod.prod Set.set * Prod.prod Set.set -> bool
    val equal : Prod.prod Set.set * Prod.prod Set.set -> bool
    val map : ('a -> Prod.prod) -> 'a Set.set -> Prod.prod Set.set
    val mapFromList : ('a -> Prod.prod) -> 'a list -> Prod.prod Set.set
    val union : Prod.prod Set.set * Prod.prod Set.set -> Prod.prod Set.set
    val genUnion : Prod.prod Set.set list -> Prod.prod Set.set
    val inter : Prod.prod Set.set * Prod.prod Set.set -> Prod.prod Set.set
    val genInter : Prod.prod Set.set list -> Prod.prod Set.set
    val minus : Prod.prod Set.set * Prod.prod Set.set -> Prod.prod Set.set
    val inputFromLabToks : (int * Lex.tok) list
                             -> Prod.prod Set.set * (int * Lex.tok) list
    val fromString : string -> Prod.prod Set.set
    val input : string -> Prod.prod Set.set
    val toPP : Prod.prod Set.set -> PP.pp
    val toString : Prod.prod Set.set -> string
    val output : string * Prod.prod Set.set -> unit
  end
