(* tran-reg-set.sig
 *
 * COPYRIGHT (c) 2012 Alley Stoughton.
 *
 * extracted from tran-reg-set.mldoc
 *)

signature TRAN_REG_SET =
  sig
    val memb : TranReg.tran_reg * TranReg.tran_reg Set.set -> bool
    val fromList : TranReg.tran_reg list -> TranReg.tran_reg Set.set
    val compare : TranReg.tran_reg Set.set Sort.total_ordering
    val subset : TranReg.tran_reg Set.set * TranReg.tran_reg Set.set -> bool
    val equal : TranReg.tran_reg Set.set * TranReg.tran_reg Set.set -> bool
    val map : ('a -> TranReg.tran_reg)
                -> 'a Set.set -> TranReg.tran_reg Set.set
    val mapFromList : ('a -> TranReg.tran_reg)
                        -> 'a list -> TranReg.tran_reg Set.set
    val union : TranReg.tran_reg Set.set * TranReg.tran_reg Set.set
                  -> TranReg.tran_reg Set.set
    val genUnion : TranReg.tran_reg Set.set list -> TranReg.tran_reg Set.set
    val inter : TranReg.tran_reg Set.set * TranReg.tran_reg Set.set
                  -> TranReg.tran_reg Set.set
    val genInter : TranReg.tran_reg Set.set list -> TranReg.tran_reg Set.set
    val minus : TranReg.tran_reg Set.set * TranReg.tran_reg Set.set
                  -> TranReg.tran_reg Set.set
    val inputFromLabToks : (int * Lex.tok) list
                             -> TranReg.tran_reg Set.set * (int * Lex.tok) list
    val fromString : string -> TranReg.tran_reg Set.set
    val input : string -> TranReg.tran_reg Set.set
    val toPP : TranReg.tran_reg Set.set -> PP.pp
    val toString : TranReg.tran_reg Set.set -> string
    val output : string * TranReg.tran_reg Set.set -> unit
  end
