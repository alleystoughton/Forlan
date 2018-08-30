(* list-aux.sig
 *
 * COPYRIGHT (c) 2012 Alley Stoughton.
 *
 * extracted from list-aux.mldoc
 *)

signature LIST_AUX =
  sig
    val sum : int list -> int
    val prod : int list -> int
    val max : int list -> int
    val min : int list -> int
    val sub : 'a list * int -> 'a
    val update : 'a list * int * 'a -> 'a list
    val position : ('a -> bool) -> 'a list -> int option
    val repeat : 'a * int -> 'a list
    val allButLast : 'a list -> 'a list
    val splitAt : 'a list * int -> 'a list * 'a list
    val allSplittings : 'a list -> ('a list * 'a list) list
    val adjacentElts : 'a list -> ('a * 'a) list
    val fromTo : int * int -> int list
  end
