(* tab.sig
 *
 * COPYRIGHT (c) 2012 Alley Stoughton.
 *
 * extracted from tab.mldoc
 *)

signature TAB =
  sig
    type ('a,'b) tab
    val lookup : 'a Sort.total_ordering -> ('a, 'b) tab * 'a -> 'b option
    val isEmpty : ('a, 'b) tab -> bool
    val empty : ('a, 'b) tab
    val update : 'a Sort.total_ordering
                   -> ('a, 'b) tab * ('a * 'b) list -> ('a, 'b) tab
    val domain : 'a Sort.total_ordering -> ('a, 'b) tab -> 'a Set.set
    val toList : ('a, 'b) tab -> ('a * 'b) list
    val fromList : 'a Sort.total_ordering -> ('a * 'b) list -> ('a, 'b) tab
  end
