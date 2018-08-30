(* tran-reg.sig
 *
 * COPYRIGHT (c) 2012 Alley Stoughton.
 *
 * extracted from tran-reg.mldoc
 *)

signature TRAN_REG =
  sig
    type tran_reg = Sym.sym * Reg.reg * Sym.sym
    val compare : tran_reg Sort.total_ordering
    val equal : tran_reg * tran_reg -> bool
  end
