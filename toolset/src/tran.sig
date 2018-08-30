(* tran.sig
 *
 * COPYRIGHT (c) 2012 Alley Stoughton.
 *
 * extracted from tran.mldoc
 *)

signature TRAN =
  sig
    type tran = Sym.sym * Str.str * Sym.sym
    val compare : tran Sort.total_ordering
    val equal : tran * tran -> bool
  end
