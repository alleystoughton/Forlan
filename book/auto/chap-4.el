(TeX-add-style-hook "chap-4"
 (lambda ()
    (LaTeX-add-index-entries
     "programming language"
     "programming language!parser"
     "programming language!parsing"
     "context-free language"
     "regular language")
    (LaTeX-add-labels
     "ContextFreeLanguages")
    (TeX-run-style-hooks)))

