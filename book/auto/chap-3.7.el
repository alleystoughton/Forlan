(TeX-add-style-hook "chap-3.7"
 (lambda ()
    (LaTeX-add-index-entries
     "simplification!finite automaton|("
     "finite automaton!simplification|("
     "reachable state"
     "finite automaton!reachable state"
     "live state"
     "finite automaton!live state"
     "dead state"
     "finite automaton!dead state"
     "useful state"
     "finite automaton!useful state"
     "finite automaton!simplified"
     "simplified!finite automaton"
     "simplification!finite automaton!simplified"
     "simplification!finite automaton!algorithm"
     "finite automaton!simplification algorithm"
     "simplify@$\\simplify$"
     "finite automaton!simplify@$\\simplify$"
     "simplification!finite automaton!simplify@$\\simplify$"
     "FA@\\texttt{FA}"
     "FA@\\texttt{FA}!simplified@\\texttt{simplified}"
     "FA@\\texttt{FA}!simplify@\\texttt{simplify}"
     "simplification!finite automaton|)"
     "finite automaton!simplification|)")
    (LaTeX-add-labels
     "SimplificationOfFiniteAutomata"
     "AlphabetSimplifiedFA"
     "RemRedunFA")
    (TeX-run-style-hooks
     "chap-3")))

