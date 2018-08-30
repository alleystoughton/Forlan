val regToEFA  = faToEFA o regToFA;
val efaToDFA  = nfaToDFA o efaToNFA;
val regToDFA  = efaToDFA o regToEFA;
val minAndRen = DFA.renameStatesCanonically o DFA.minimize;

val allStrReg = Reg.fromString "(0 + 1)*";
val allStrDFA = minAndRen(regToDFA allStrReg);
val allStrEFA = injDFAToEFA allStrDFA;

val longReg =
      Reg.concat
      (Reg.power(Reg.fromString "0 + 1", 5),
       Reg.fromString "(0 + 1)*");
val longDFA = minAndRen(regToDFA longReg);

val stutterReg = Reg.fromString "(0 + 1)*(00 + 11)(0 + 1)*";
val stutterDFA = minAndRen(regToDFA stutterReg);

val notStutterDFA =
      minAndRen(DFA.minus(allStrDFA, stutterDFA));

val longAndNotStutterDFA =
      minAndRen(DFA.inter(longDFA, notStutterDFA));
val longAndNotStutterEFA =
      injDFAToEFA longAndNotStutterDFA;

val someLongNotStutterEFA' =
      EFA.concat
      (allStrEFA,
       EFA.concat
       (longAndNotStutterEFA,
        allStrEFA));
val someLongNotStutterEFA  =
      EFA.renameStatesCanonically someLongNotStutterEFA';

val someLongNotStutterDFA =
      minAndRen(efaToDFA someLongNotStutterEFA);
val allLongStutterDFA     =
      minAndRen(DFA.minus(allStrDFA, someLongNotStutterDFA));
