(TeX-add-style-hook "chap-1.3"
 (lambda ()
    (LaTeX-add-index-entries
     "tree|("
     "tree|)"
     "recursion|("
     "recursion|)")
    (LaTeX-add-labels
     "TreesAndInductiveDefinitions"
     "ClosureLem"
     "ClosureIntersect"
     "TreeDestruct"
     "WellFoundedRecursion")))

