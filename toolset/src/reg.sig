(* reg.sig
 *
 * COPYRIGHT (c) 2012 Alley Stoughton.
 *
 * extracted from reg.mldoc
 *)

signature REG =
  sig
    datatype concr
      = EmptyStr
      | EmptySet
      | Sym of Sym.sym
      | Closure of concr
      | Concat of concr * concr
      | Union of concr * concr
    type reg
    val fromConcr : concr -> reg
    val toConcr : reg -> concr
    val inputFromLabToks : (int * Lex.tok) list -> reg * (int * Lex.tok) list
    val fromString : string -> reg
    val input : string -> reg
    val toPP : reg -> PP.pp
    val toString : reg -> string
    val output : string * reg -> unit
    val validPath : reg * int list -> bool
    val height : reg -> int
    val size : reg -> int
    val numLeaves : reg -> int
    val select : reg * int list -> reg
    val update : reg * int list * reg -> reg
    val maximumLengthPath : reg -> int list
    val validLeafPath : reg * int list -> bool
    val emptyStr : reg
    val emptySet : reg
    val fromSym : Sym.sym -> reg
    val closure : reg -> reg
    val concat : reg * reg -> reg
    val union : reg * reg -> reg
    val isEmptyStr : reg -> bool
    val isEmptySet : reg -> bool
    val isSym : reg -> bool
    val isClosure : reg -> bool
    val isConcat : reg -> bool
    val isUnion : reg -> bool
    val compare : reg Sort.total_ordering
    val equal : reg * reg -> bool
    val fromStr : Str.str -> reg
    val power : reg * int -> reg
    val alphabet : reg -> Sym.sym Set.set
    val split : reg -> Str.str * reg option
    val genConcat : reg list -> reg
    val genUnion : reg list -> reg
    val rightConcat : reg * reg -> reg
    val rightUnion : reg * reg -> reg
    val concatsToList : reg -> reg list
    val unionsToList : reg -> reg list
    val sortUnions : reg -> reg
    val allSym : Sym.sym Set.set -> reg
    val allStr : Sym.sym Set.set -> reg
    val fromStrSet : Str.str Set.set -> reg
    type cc
    val ccToList : cc -> int list
    val singCC : int -> cc
    val unionCC : cc * cc -> cc
    val succCC : cc -> cc
    val cc : reg -> cc
    val compareCC : cc Sort.total_ordering
    val numConcats : reg -> int
    val numSyms : reg -> int
    val standardized : reg -> bool
    val compareComplexity : reg * reg -> order
    val compareComplexityTotal : reg Sort.total_ordering
    val weaklySimplified : reg -> bool
    val weaklySimplify : reg -> reg
    val toStrSetOpt : reg -> Str.str Set.set option
    val toStrSet : reg -> Str.str Set.set
    val hasEmp : reg -> bool
    val hasSym : Sym.sym * reg -> bool
    val obviousSubset : reg * reg -> bool
    val localSimplificationRelations : reg * reg -> order
    val locallySimplified : (reg * reg -> bool) -> reg -> bool
    val locallySimplifyTrace : int option * (reg * reg -> bool)
                                 -> reg -> bool * reg
    val locallySimplify : int option * (reg * reg -> bool) -> reg -> bool * reg
    val globallySimplified : bool * (reg * reg -> bool) -> reg -> bool
    val globallySimplifyTrace : int option * bool * (reg * reg -> bool)
                                  -> reg -> bool * reg
    val globallySimplify : int option * bool * (reg * reg -> bool)
                             -> reg -> bool * reg
    val renameAlphabet : reg * SymRel.sym_rel -> reg
    val rev : reg -> reg
    val prefix : reg -> reg
    val jforlanNew : unit -> reg
    val jforlanEdit : reg -> reg
    val jforlanValidate : string -> unit
    val jforlanPretty : string -> unit
  end
