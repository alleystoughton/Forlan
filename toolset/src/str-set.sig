(* str-set.sig
 *
 * COPYRIGHT (c) 2018 Alley Stoughton.
 *
 * extracted from str-set.mldoc
 *)

signature STR_SET =
  sig
    val memb : Str.str * Str.str Set.set -> bool
    val fromList : Str.str list -> Str.str Set.set
    val compare : Str.str Set.set Sort.total_ordering
    val subset : Str.str Set.set * Str.str Set.set -> bool
    val equal : Str.str Set.set * Str.str Set.set -> bool
    val map : ('a -> Str.str) -> 'a Set.set -> Str.str Set.set
    val mapFromList : ('a -> Str.str) -> 'a list -> Str.str Set.set
    val union : Str.str Set.set * Str.str Set.set -> Str.str Set.set
    val genUnion : Str.str Set.set list -> Str.str Set.set
    val inter : Str.str Set.set * Str.str Set.set -> Str.str Set.set
    val genInter : Str.str Set.set list -> Str.str Set.set
    val minus : Str.str Set.set * Str.str Set.set -> Str.str Set.set
    val inputFromLabToks : (int * Lex.tok) list
                             -> Str.str Set.set * (int * Lex.tok) list
    val fromString : string -> Str.str Set.set
    val input : string -> Str.str Set.set
    val toPP : Str.str Set.set -> PP.pp
    val toString : Str.str Set.set -> string
    val output : string * Str.str Set.set -> unit
    val concat : Str.str Set.set * Str.str Set.set -> Str.str Set.set
    val power : Str.str Set.set * int -> Str.str Set.set
    val rev : Str.str Set.set -> Str.str Set.set
    val prefixes : Str.str -> Str.str Set.set
    val suffixes : Str.str -> Str.str Set.set
    val substrings : Str.str -> Str.str Set.set
    val prefix : Str.str Set.set -> Str.str Set.set
    val suffix : Str.str Set.set -> Str.str Set.set
    val substring : Str.str Set.set -> Str.str Set.set
    val alphabet : Str.str Set.set -> Sym.sym Set.set
    val renameAlphabet : Str.str Set.set * SymRel.sym_rel -> Str.str Set.set
  end
