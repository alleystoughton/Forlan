val regToDFA  = nfaToDFA o efaToNFA o faToEFA o regToFA;
val minAndRen = DFA.renameStatesCanonically o DFA.minimize;
val syms0123 = SymSet.fromString "0, 1, 2, 3";
val allStrReg = Reg.closure(Reg.allSym syms0123);
val allStrDFA = minAndRen(regToDFA allStrReg);

(* symbolic relation swapping 2/3 for 0/1 *)

val swap23for01 = SymRel.fromString "(0, 2), (1, 3), (2, 0), (3, 1)";

(* DFA accepting all elements of {0, 1, 2, 3}* in which the sum of
   the numbers of 2s and 3s is odd *)

val odd2plus3Alp01DFA =
      minAndRen
      (DFA.minus
       (allStrDFA,
        DFA.renameAlphabet(even0plus1Alp23DFA, swap23for01)));

(* DFA accepting all elements of {0, 1, 2, 3}* in which sum of the
   numbers of 0s and 1s is even, and sum of the numbers of 2s and 3s
   is odd *)

val even0plus1AndOdd2plus3DFA =
      minAndRen(DFA.inter(even0plus1Alp23DFA, odd2plus3Alp01DFA));

(* grammar generating X *)

val gram0 =
      Gram.renameVariablesCanonically
      (Gram.inter
       (seq0123Where0lt3And1gt2Gram,
        injDFAToEFA even0plus1AndOdd2plus3DFA));
