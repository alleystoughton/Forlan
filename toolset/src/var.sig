(* var.sig
 *
 * COPYRIGHT (c) 2012 Alley Stoughton.
 *
 * extracted from var.mldoc
 *)

signature VAR =
  sig
    type var
    val beginsWithVar : (int * Lex.tok) list -> bool
    val inputLabFromLabToks : (int * Lex.tok) list
                                -> int * var * (int * Lex.tok) list
    val fromString : string -> var
    val input : string -> var
    val toPP : var -> PP.pp
    val toString : var -> string
    val output : string * var -> unit
    val compare : var Sort.total_ordering
    val equal : var * var -> bool
  end
