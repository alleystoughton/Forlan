(* sym-set.sig
 *
 * COPYRIGHT (c) 2012 Alley Stoughton.
 *
 * extracted from sym-set.mldoc
 *)

signature SYM_SET =
  sig
    val memb : Sym.sym * Sym.sym Set.set -> bool
    val fromList : Sym.sym list -> Sym.sym Set.set
    val compare : Sym.sym Set.set Sort.total_ordering
    val subset : Sym.sym Set.set * Sym.sym Set.set -> bool
    val equal : Sym.sym Set.set * Sym.sym Set.set -> bool
    val map : ('a -> Sym.sym) -> 'a Set.set -> Sym.sym Set.set
    val mapFromList : ('a -> Sym.sym) -> 'a list -> Sym.sym Set.set
    val union : Sym.sym Set.set * Sym.sym Set.set -> Sym.sym Set.set
    val genUnion : Sym.sym Set.set list -> Sym.sym Set.set
    val inter : Sym.sym Set.set * Sym.sym Set.set -> Sym.sym Set.set
    val genInter : Sym.sym Set.set list -> Sym.sym Set.set
    val minus : Sym.sym Set.set * Sym.sym Set.set -> Sym.sym Set.set
    val inputFromLabToks : (int * Lex.tok) list
                             -> Sym.sym Set.set * (int * Lex.tok) list
    val fromString : string -> Sym.sym Set.set
    val input : string -> Sym.sym Set.set
    val toPP : Sym.sym Set.set -> PP.pp
    val toString : Sym.sym Set.set -> string
    val output : string * Sym.sym Set.set -> unit
  end
