(* prod.sig
 *
 * COPYRIGHT (c) 2012 Alley Stoughton.
 *
 * extracted from prod.mldoc
 *)

signature PROD =
  sig
    type prod = Sym.sym * Str.str
    val compare : prod Sort.total_ordering
    val equal : prod * prod -> bool
  end
