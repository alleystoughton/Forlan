(* pp.sig
 *
 * COPYRIGHT (c) 2012 Alley Stoughton.
 *
 * extracted from pp.mldoc
 *)

signature PP =
  sig
    val newline : string
    type pp
    val toString : pp -> string
    val empty : pp
    val block : bool * pp list -> pp
    val decorate : string * pp * string -> pp
    val quote : pp -> pp
    val comma : pp -> pp
    val colon : pp -> pp
    val semicolon : pp -> pp
    val fromString : string -> pp
    val fromStringSplitEscape : string -> pp
  end
