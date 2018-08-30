(* set.sig
 *
 * COPYRIGHT (c) 2012 Alley Stoughton.
 *
 * extracted from set.mldoc
 *)

signature SET =
  sig
    type 'a set
    val memb : 'a Sort.total_ordering -> 'a * 'a set -> bool
    val fromList : 'a Sort.total_ordering -> 'a list -> 'a set
    val toList : 'a set -> 'a list
    val isEmpty : 'a set -> bool
    val isNonEmpty : 'a set -> bool
    val size : 'a set -> int
    val hd : 'a set -> 'a
    val tl : 'a set -> 'a set
    val compare : 'a Sort.total_ordering -> 'a set * 'a set -> order
    val subset : 'a Sort.total_ordering -> 'a set * 'a set -> bool
    val equal : 'a Sort.total_ordering -> 'a set * 'a set -> bool
    val all : ('a -> bool) -> 'a set -> bool
    val exists : ('a -> bool) -> 'a set -> bool
    val empty : 'a set
    val sing : 'a -> 'a set
    val filter : ('a -> bool) -> 'a set -> 'a set
    val partition : ('a -> bool) -> 'a set -> 'a set * 'a set
    val position : ('a -> bool) -> 'a set -> int option
    val map : 'b Sort.total_ordering -> ('a -> 'b) -> 'a set -> 'b set
    val mapFromList : 'b Sort.total_ordering -> ('a -> 'b) -> 'a list -> 'b set
    val mapToList : ('a -> 'b) -> 'a set -> 'b list
    val union : 'a Sort.total_ordering -> 'a set * 'a set -> 'a set
    val genUnion : 'a Sort.total_ordering -> 'a set list -> 'a set
    val inter : 'a Sort.total_ordering -> 'a set * 'a set -> 'a set
    val genInter : 'a Sort.total_ordering -> 'a set list -> 'a set
    val minus : 'a Sort.total_ordering -> 'a set * 'a set -> 'a set
    val comparePair : 'a Sort.total_ordering * 'b Sort.total_ordering
                        -> ('a * 'b) Sort.total_ordering
    val times : 'a set * 'b set -> ('a * 'b) set
    val compareTriple : 'a Sort.total_ordering
                          * 'b Sort.total_ordering
                          * 'c Sort.total_ordering
                          -> ('a * 'b * 'c) Sort.total_ordering
    val times3 : 'a set * 'b set * 'c set -> ('a * 'b * 'c) set
    val compareList : 'a Sort.total_ordering -> 'a list Sort.total_ordering
    val genTimes : 'a set list -> 'a list set
  end
