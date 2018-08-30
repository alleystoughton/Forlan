(* lex.sig
 *
 * COPYRIGHT (c) 2012 Alley Stoughton.
 *
 * extracted from lex.mldoc
 *)

signature LEX =
  sig
    type basic
    val charToBasic : char -> basic
    val basicToChar : basic -> char
    type sym
    val symToString : sym -> string
    val sizeSym : sym -> int
    val compareSym : sym Sort.total_ordering
    val symToPP : sym -> PP.pp
    datatype sym_top
      = BasicSymTop of basic
      | CompoundSymTop of sym option list
    val symTopToSym : sym_top -> sym
    val symToSymTop : sym -> sym_top
    datatype tok
      = Bar
      | Comma
      | Dollar
      | Perc
      | Plus
      | Semicolon
      | Star
      | Tilde
      | OpenPar
      | ClosPar
      | SingArr
      | DoubArr
      | Sym of sym
      | Heading of string
      | EOF
    val equalTok : tok * tok -> bool
    val errorNotEOFTerminated : unit -> 'a
    val expectedTok : int * tok -> 'a
    val expectedDigit : int -> 'a
    val expectedLetter : int -> 'a
    val expectedLetterOrDigit : int -> 'a
    val unexpectedTok : int * tok -> 'a
    val checkInLabToks : tok * (int * tok) list -> (int * tok) list
    val error : int * int * PP.pp list -> 'a
    val lexString : string -> (int * tok) list
    val lexFile : string -> (int * tok) list
  end
