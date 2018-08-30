(* tran-set.sig
 *
 * COPYRIGHT (c) 2012 Alley Stoughton.
 *
 * extracted from tran-set.mldoc
 *)

signature TRAN_SET =
  sig
    val memb : Tran.tran * Tran.tran Set.set -> bool
    val fromList : Tran.tran list -> Tran.tran Set.set
    val compare : Tran.tran Set.set Sort.total_ordering
    val subset : Tran.tran Set.set * Tran.tran Set.set -> bool
    val equal : Tran.tran Set.set * Tran.tran Set.set -> bool
    val map : ('a -> Tran.tran) -> 'a Set.set -> Tran.tran Set.set
    val mapFromList : ('a -> Tran.tran) -> 'a list -> Tran.tran Set.set
    val union : Tran.tran Set.set * Tran.tran Set.set -> Tran.tran Set.set
    val genUnion : Tran.tran Set.set list -> Tran.tran Set.set
    val inter : Tran.tran Set.set * Tran.tran Set.set -> Tran.tran Set.set
    val genInter : Tran.tran Set.set list -> Tran.tran Set.set
    val minus : Tran.tran Set.set * Tran.tran Set.set -> Tran.tran Set.set
    val inputFromLabToks : (int * Lex.tok) list
                             -> Tran.tran Set.set * (int * Lex.tok) list
    val fromString : string -> Tran.tran Set.set
    val input : string -> Tran.tran Set.set
    val toPP : Tran.tran Set.set -> PP.pp
    val toString : Tran.tran Set.set -> string
    val output : string * Tran.tran Set.set -> unit
  end
