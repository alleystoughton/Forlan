(* sort.sig
 *
 * COPYRIGHT (c) 2012 Alley Stoughton.
 *
 * extracted from sort.mldoc
 *)

signature SORT =
  sig
    type 'a total_ordering = 'a * 'a -> order
    val equal : 'a total_ordering -> 'a * 'a -> bool
    val less : 'a total_ordering -> 'a * 'a -> bool
    val lessEqual : 'a total_ordering -> 'a * 'a -> bool
    val sorted : bool * 'a total_ordering -> 'a list -> bool
    val insert : bool * 'a total_ordering -> 'a * 'a list -> 'a list
    val merge : bool * 'a total_ordering -> 'a list * 'a list -> 'a list
    val sort : bool * 'a total_ordering -> 'a list -> 'a list
  end
