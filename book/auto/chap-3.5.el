(TeX-add-style-hook "chap-3.5"
 (lambda ()
    (LaTeX-add-index-entries
     "isomorphism!finite automaton|("
     "finite automaton!isomorphism|("
     "finite automaton!isomorphism from FA to FA"
     "isomorphism!finite automaton!isomorphism from FA to FA"
     "iso@$\\iso$"
     "finite automaton!iso@$\\iso$"
     "isomorphism!finite automaton!iso@$\\iso$"
     "isomorphic!finite automaton"
     "finite automaton!isomorphic"
     "isomorphism!finite automaton!isomorphic"
     "reflexive on set!iso@$\\iso$"
     "symmetry!iso@$\\iso$"
     "transitive!iso@$\\iso$"
     "iso@$\\iso$!reflexive"
     "iso@$\\iso$!symmetric"
     "iso@$\\iso$!transitive"
     "finite automaton!renaming states"
     "renameStates@$\\renameStates$"
     "finite automaton!renameStates@$\\renameStates$"
     "renameStatesCanonically@$\\renameStatesCanonically$"
     "finite automaton!renameStatesCanonically@$\\renameStatesCanonically$"
     "finite automaton!isomorphism!checking whether FAs are isomorphic"
     "isomorphism!finite automaton!checking whether FAs are isomorphic"
     "FA@\\texttt{FA}"
     "FA@\\texttt{FA}!isomorphism@\\texttt{isomorphism}"
     "FA@\\texttt{FA}!findIsomorphism@\\texttt{findIsomorphism}"
     "FA@\\texttt{FA}!isomorphic@\\texttt{isomorphic}"
     "FA@\\texttt{FA}!renameStates@\\texttt{renameStates}"
     "FA@\\texttt{FA}!renameStatesCanonically@\\texttt{renameStatesCanonically}"
     "isomorphism!finite automaton|)"
     "finite automaton!isomorphism|)")
    (LaTeX-add-labels
     "IsomorphismOfFiniteAutomata"
     "IsoEquivProp"
     "IsoSubProp"
     "IsoLem"
     "FindIsoLem")
    (TeX-run-style-hooks
     "chap-3")))

