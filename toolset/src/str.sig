(* str.sig
 *
 * COPYRIGHT (c) 2012 Alley Stoughton.
 *
 * extracted from str.mldoc
 *)

signature STR =
  sig
    type str = Sym.sym list
    val possBeginsWithStr : (int * Lex.tok) list -> bool
    val inputFromLabToks : (int * Lex.tok) list -> str * (int * Lex.tok) list
    val fromString : string -> str
    val input : string -> str
    val toPP : str -> PP.pp
    val toString : str -> string
    val output : string * str -> unit
    val last : str -> Sym.sym
    val allButLast : str -> str
    val compare : str Sort.total_ordering
    val equal : str * str -> bool
    val alphabet : str -> Sym.sym Set.set
    val renameAlphabet : str * SymRel.sym_rel -> str
    val prefix : str * str -> bool
    val suffix : str * str -> bool
    val substr : str * str -> bool
    val power : str * int -> str
    val removePrefix : str * str -> str option
    val removeSuffix : str * str -> str option
    val jforlanValidate : string -> unit
    val jforlanPretty : string -> unit
  end
