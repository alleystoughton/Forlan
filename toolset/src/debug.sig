(* debug.sig
 *
 * COPYRIGHT (c) 2012 Alley Stoughton.
 *
 * extracted from debug.mldoc
 *)

signature DEBUG =
  sig
    val debug : ('a -> PP.pp) -> PP.pp * 'a -> 'a
  end
