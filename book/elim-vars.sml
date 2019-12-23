fun elimVars(gram, nil)     = gram
  | elimVars(gram, q :: qs) =
      elimVars(Gram.eliminateVariable(gram, Sym.fromString q), qs);

val gram1 =
      Gram.renameVariablesCanonically
      (elimVars(Gram.restart gram0, ["E", "G", "H", "J", "M", "C"]));
