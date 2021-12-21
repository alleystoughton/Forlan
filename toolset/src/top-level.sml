(******************************* top-level.sml *******************************)

(* Copyright (C) 2001-2021 Alley Stoughton

   The file is part of the Forlan toolset for experimenting with
   formal languages.  See the file COPYING.txt for copying and
   usage restrictions. *)

(* this structure should be opened at the top-level *)

structure TopLevel =
struct

(********************************** Version **********************************)

val getVersion : unit -> string = Version.getVersion
val getSMLNJVersion : unit -> string = Version.getSMLNJVersion

(******************************** Parameters *********************************)

val getSearchPath : unit -> string list = Params.getSearchPath

val setSearchPath : string list -> unit = Params.setSearchPath

val getWorkingDirectory : unit -> string = Params.getWorkingDirectory

val setWorkingDirectory : string -> unit = Params.setWorkingDirectory

val getPrintingListLength : unit -> int = Params.getPrintingListLength

val setPrintingListLength : int -> unit = Params.setPrintingListLength

val getPrintingStringSize : unit -> int = Params.getPrintingStringSize

val setPrintingStringSize : int -> unit = Params.setPrintingStringSize

val getPrintingDataStructureDepth : unit -> int =
      Params.getPrintingDataStructureDepth

val setPrintingDataStructureDepth : int -> unit =
      Params.setPrintingDataStructureDepth

val getPrintingLineLength : unit -> int = Params.getPrintingLineLength

val setPrintingLineLength : int -> unit = Params.setPrintingLineLength

val setPrintingOfGarbageCollectionMessages : bool -> unit =
      Params.setPrintingOfGarbageCollectionMessages

val getTrackExceptions : unit -> bool = Params.getTrackExceptions

val setTrackExceptions : bool -> unit = Params.setTrackExceptions

val getCompilationManagerVerbosity : unit -> bool =
      Params.getCompilationManagerVerbosity

val setCompilationManagerVerbosity : bool -> unit =
      Params.setCompilationManagerVerbosity

(********************************* Debugging *********************************)

val debug : ('a -> PP.pp) -> PP.pp * 'a -> 'a = Debug.debug

(******************************** Using Files ********************************)

val use : string -> unit = Use.use

(******************************** Basic Types ********************************)

type 'a set = 'a Set.set

type sym = Sym.sym

type str = Str.str

type ('a, 'b)rel = ('a, 'b)Rel.rel

type sym_rel = SymRel.sym_rel

(**************** Conversions Between Basic Types and Strings ****************)

val stringToSym : string -> sym = Sym.fromString

val symToString : sym -> string = Sym.toString

val stringToStr : string -> str = Str.fromString

val strToString : str -> string = Str.toString

val stringToSymSet : string -> sym set = SymSet.fromString

val symSetToString : sym set -> string = SymSet.toString

val stringToStrSet : string -> str set = StrSet.fromString

val strSetToString : str set -> string = StrSet.toString

val stringToSymRel : string -> sym_rel = SymRel.fromString

val symRelToString : sym_rel -> string = SymRel.toString

(**************************** Regular Expressions ****************************)

type reg = Reg.reg

val stringToReg : string -> reg = Reg.fromString

val regToString : reg -> string = Reg.toString

val symToReg : sym -> reg = Reg.fromSym

val strToReg : str -> reg = Reg.fromStr

(******************************* Labeled Paths *******************************)

type lp = LP.lp

val stringToLP : string -> lp = LP.fromString

val lpToString : lp -> string = LP.toString

(****************************** Finite Automata ******************************)

type tran = Tran.tran

type fa = FA.fa

type efa = EFA.efa

type nfa = NFA.nfa

type dfa = DFA.dfa

val symToNFA : sym -> nfa = NFA.fromSym

val symToEFA : sym -> efa = EFA.fromSym

val symToFA : sym -> fa = FA.fromSym

val strToFA : str -> fa = FA.fromStr

(************ Injections and Projections Between Finite Automata ************)

val injDFAToNFA : dfa -> nfa = DFA.injToNFA

val injDFAToEFA : dfa -> efa = DFA.injToEFA

val injDFAToFA : dfa -> fa = DFA.injToFA

val injNFAToEFA : nfa -> efa = NFA.injToEFA

val injNFAToFA : nfa -> fa = NFA.injToFA

val injEFAToFA : efa -> fa = EFA.injToFA

val projNFAToDFA : nfa -> dfa = DFA.projFromNFA

val projEFAToDFA : efa -> dfa = DFA.projFromEFA

val projFAToDFA : fa -> dfa = DFA.projFromFA

val projEFAToNFA : efa -> nfa = NFA.projFromEFA

val projFAToNFA : fa -> nfa = NFA.projFromFA

val projFAToEFA : fa -> efa = EFA.projFromFA

(******************** Conversions Between Finite Automata ********************)

val nfaToDFA : nfa -> dfa = DFA.fromNFA

val efaToNFA : efa -> nfa = NFA.fromEFA

val faToEFA : fa -> efa = EFA.fromFA

(******** Conversions Between Finite Automata and Regular Expressions ********)

val regToFA : reg -> fa = FA.fromReg

val faToReg : (reg -> reg) -> fa -> reg = RFA.faToReg

val faToRegPerms :
      int option * (reg -> reg) -> fa -> reg = RFA.faToRegPerms

val faToRegPermsTrace :
      int option * (reg -> reg) -> fa -> reg = RFA.faToRegPermsTrace

(******************** Regular Expression Finite Automata ********************)

type tran_reg = TranReg.tran_reg

type rfa = RFA.rfa

(******************************** Parse Trees ********************************)

type pt = PT.pt

val stringToPT : string -> pt = PT.fromString

val ptToString : pt -> string = PT.toString

(********************************* Grammars *********************************)

type prod = Prod.prod

type gram = Gram.gram

val symToGram : sym -> gram = Gram.fromSym

val strToGram : str -> gram = Gram.fromStr

val faToGram : fa -> gram = Gram.fromFA

val regToGram : reg -> gram = Gram.fromReg

(********************************** Programs **********************************)

type var = Var.var

type prog = Prog.prog

type cp = Prog.cp

val progToClosedProg = Prog.toClosed

val closedProgToProg = Prog.fromClosed

end;
