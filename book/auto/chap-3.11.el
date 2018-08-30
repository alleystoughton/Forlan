(TeX-add-style-hook "chap-3.11"
 (lambda ()
    (LaTeX-add-index-entries
     "deterministic finite automaton!exponential blowup")
    (LaTeX-add-labels
     "DeterministicFiniteAutomata"
     "DetermProp1"
     "DetermProp2"
     "DetermProp3"
     "NFADeltaProp"
     "NFAToDFAConvLemma"
     "NFAToDFATheorem")
    (TeX-run-style-hooks
     "chap-3")))

