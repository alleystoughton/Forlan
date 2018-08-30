(TeX-add-style-hook "chap-3.6"
 (lambda ()
    (LaTeX-add-index-entries
     "finite automaton!checking for string acceptance"
     "finite automaton!searching for labeled paths"
     "Delta@$\\Delta_\\cdot$"
     "finite automaton!Delta@$\\Delta_\\cdot$"
     "finite automaton!calculating Delta@calculating $\\Delta_\\cdot(\\cdot,\\cdot)$"
     "L(.)@$L(\\cdot)$"
     "finite automaton!L(.)@$L(\\cdot)$"
     "finite automaton!characterizing L(.)@characterizing $L(\\cdot)$"
     "FA@\\texttt{FA}"
     "FA@\\texttt{FA}!processStr@\\texttt{processStr}"
     "FA@\\texttt{FA}!accepted@\\texttt{accepted}"
     "FA@\\texttt{FA}!findLP@\\texttt{findLP}"
     "FA@\\texttt{FA}!findAcceptingLP@\\texttt{findAcceptingLP}")
    (LaTeX-add-labels
     "CheckingAcceptanceAndFindingAcceptingPaths"
     "AcceptLem1"
     "AcceptLem2"
     "AcceptLem3")
    (TeX-run-style-hooks
     "chap-3")))

