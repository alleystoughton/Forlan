(* sym.sig
 *
 * COPYRIGHT (c) 2012 Alley Stoughton.
 *
 * extracted from sym.mldoc
 *)

signature SYM =
  sig
    type basic = Lex.basic
    val charToBasic : char -> basic
    val basicToChar : basic -> char
    type sym = Lex.sym
    datatype top
      = Basic of basic
      | Compound of sym option list
    val fromTop : top -> sym
    val toTop : sym -> top
    val inputFromLabToks : (int * Lex.tok) list -> sym * (int * Lex.tok) list
    val fromString : string -> sym
    val input : string -> sym
    val toPP : sym -> PP.pp
    val toString : sym -> string
    val output : string * sym -> unit
    val compare : sym Sort.total_ordering
    val equal : sym * sym -> bool
    val size : sym -> int
    val jforlanValidate : string -> unit
    val jforlanPretty : string -> unit
  end
