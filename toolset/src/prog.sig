(* prog.sig
 *
 * COPYRIGHT (c) 2012 Alley Stoughton.
 *
 * extracted from prog.mldoc
 *)

signature PROG =
  sig
    datatype const
      = True
      | False
      | Nil
    datatype oper
      = IsNil
      | IsInt
      | IsNeg
      | IsZero
      | IsPos
      | IsSym
      | IsStr
      | IsPair
      | IsLam
      | Plus
      | Minus
      | Compare
      | Fst
      | Snd
      | ConsSym
      | DeconsSym
      | SymListToStr
      | StrToSymList
    datatype concr
      = Var of Var.var
      | Const of const
      | Int of IntInf.int
      | Sym of Sym.sym
      | Str of Str.str
      | Pair of concr * concr
      | Calc of oper * concr
      | Cond of concr * concr * concr
      | App of concr * concr
      | Lam of Var.var * concr
      | LetSimp of Var.var * concr * concr
      | LetRec of Var.var * Var.var * concr * concr
    type prog
    val fromConcr : concr -> prog
    val toConcr : prog -> concr
    val var : Var.var -> prog
    val const : const -> prog
    val int : IntInf.int -> prog
    val sym : Sym.sym -> prog
    val str : Str.str -> prog
    val pair : prog * prog -> prog
    val calc : oper * prog -> prog
    val cond : prog * prog * prog -> prog
    val app : prog * prog -> prog
    val lam : Var.var * prog -> prog
    val letSimp : Var.var * prog * prog -> prog
    val letRec : Var.var * Var.var * prog * prog -> prog
    val fromString : string -> prog
    val input : string -> prog
    val toPP : prog -> PP.pp
    val toString : prog -> string
    val output : string * prog -> unit
    val compare : prog Sort.total_ordering
    val equal : prog * prog -> bool
    val toStr : prog -> Str.str
    val fromStr : Str.str -> prog
    val validPath : prog * int list -> bool
    val height : prog -> int
    val size : prog -> int
    val numLeaves : prog -> int
    val select : prog * int list -> prog
    val update : prog * int list * prog -> prog
    val maximumLengthPath : prog -> int list
    val validLeafPath : prog * int list -> bool
    val free : prog -> Var.var Set.set
    type cp
    val toClosed : prog -> cp
    val fromClosed : cp -> prog
    val subst : cp * Var.var * prog -> prog
    val isValue : cp -> bool
    datatype step
      = Value
      | Error
      | Next of cp
    val step : cp -> step
    datatype run
      = Ans of cp
      | Fail of cp
      | Intermed of cp
    val run : cp * int -> run
    val evaluate : prog * int -> unit
    datatype accept
      = Accept
      | RejectWithFalse
      | RejectOtherwise
      | Unknown of cp
    val accept : cp -> Str.str * int -> accept
    val accepted : prog -> Str.str * int -> unit
    val toRep : prog -> prog
    val fromRep : prog -> prog
    val isRep : prog -> bool
    val jforlanNew : unit -> prog
    val jforlanEdit : prog -> prog
    val jforlanValidate : string -> unit
    val jforlanPretty : string -> unit
  end
