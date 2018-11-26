(* gram.sig
 *
 * COPYRIGHT (c) 2012 Alley Stoughton.
 *
 * extracted from gram.mldoc
 *)

signature GRAM =
  sig
    type concr = {
                   vars : Sym.sym Set.set,
                   start : Sym.sym,
                   prods : Prod.prod Set.set
                 }
    type gram
    val valid : concr -> bool
    val fromConcr : concr -> gram
    val toConcr : gram -> concr
    val fromString : string -> gram
    val input : string -> gram
    val toPP : gram -> PP.pp
    val toString : gram -> string
    val output : string * gram -> unit
    val variables : gram -> Sym.sym Set.set
    val startVariable : gram -> Sym.sym
    val productions : gram -> Prod.prod Set.set
    val compare : gram Sort.total_ordering
    val equal : gram * gram -> bool
    val numVariables : gram -> int
    val numProductions : gram -> int
    val alphabet : gram -> Sym.sym Set.set
    val sub : gram * gram -> bool
    val checkPT : gram -> PT.pt -> unit
    val validPT : gram -> PT.pt -> bool
    val renameVariables : gram * SymRel.sym_rel -> gram
    val renameVariablesCanonically : gram -> gram
    val isomorphism : gram * gram * SymRel.sym_rel -> bool
    val findIsomorphismOpt : gram * gram -> SymRel.sym_rel option
    val findIsomorphism : gram * gram -> SymRel.sym_rel
    val isomorphic : gram * gram -> bool
    val renameAlphabet : gram * SymRel.sym_rel -> gram
    val parsable : gram -> Sym.sym * Str.str -> bool
    val generatedFromVariable : gram -> Sym.sym * Str.str -> bool
    val generated : gram -> Str.str -> bool
    val parseOpt : gram -> Sym.sym * Str.str -> PT.pt option
    val parse : gram -> Sym.sym * Str.str -> PT.pt
    val parseAlphabetFromVariableOpt : gram
                                         -> Sym.sym * Str.str -> PT.pt option
    val parseAlphabetFromVariable : gram -> Sym.sym * Str.str -> PT.pt
    val parseAlphabetOpt : gram -> Str.str -> PT.pt option
    val parseAlphabet : gram -> Str.str -> PT.pt
    val reachableFrom : gram -> Sym.sym Set.set -> Sym.sym Set.set
    val reachableFromBackwards : gram -> Sym.sym Set.set -> Sym.sym Set.set
    val reachify : gram -> gram
    val reachified : gram -> bool
    val simplify : gram -> gram
    val simplified : gram -> bool
    val eliminateVariable : gram * Sym.sym -> gram
    val eliminateVariableOpt : gram * Sym.sym -> gram option
    val restart : gram -> gram
    val restartOpt : gram -> gram option
    val nullableVariables : gram -> Sym.sym Set.set
    val hasNoEmptyProductions : gram -> bool
    val eliminateEmptyProductions : gram -> gram
    val hasNoEmptyOrUnitProductions : gram -> bool
    val eliminateEmptyAndUnitProductions : gram -> gram
    val inChomskyNormalForm : gram -> bool
    val chomskyNormalForm : gram -> gram
    val toStrSetOpt : gram -> Str.str Set.set option
    val toStrSet : gram -> Str.str Set.set
    val emptyStr : gram
    val emptySet : gram
    val fromStr : Str.str -> gram
    val fromSym : Sym.sym -> gram
    val fromStrSet : Str.str Set.set -> gram
    val union : gram * gram -> gram
    val concat : gram * gram -> gram
    val closure : gram -> gram
    val genUnion : gram list -> gram
    val genConcat : gram list -> gram
    val fromFA : FA.fa -> gram
    val fromReg : Reg.reg -> gram
    val rev : gram -> gram
    val prefix : gram -> gram
    val inter : gram * EFA.efa -> gram
    val minus : gram * DFA.dfa -> gram
  end
