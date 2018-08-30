(TeX-add-style-hook "chap-5.1"
 (lambda ()
    (LaTeX-add-index-entries
     "VarSet@\\texttt{VarSet}"
     "set@\\texttt{\\primesym a~set}"
     "VarSet@\\texttt{VarSet}!input@\\texttt{input}"
     "VarSet@\\texttt{VarSet}!output@\\texttt{output}"
     "VarSet@\\texttt{VarSet}!fromList@\\texttt{fromList}"
     "VarSet@\\texttt{VarSet}!memb@\\texttt{memb}"
     "VarSet@\\texttt{VarSet}!subset@\\texttt{subset}"
     "VarSet@\\texttt{VarSet}!equal@\\texttt{equal}"
     "VarSet@\\texttt{VarSet}!union@\\texttt{union}"
     "VarSet@\\texttt{VarSet}!inter@\\texttt{inter}"
     "VarSet@\\texttt{VarSet}!minus@\\texttt{minus}"
     "VarSet@\\texttt{VarSet}!genUnion@\\texttt{genUnion}"
     "VarSet@\\texttt{VarSet}!genInter@\\texttt{genInter}")
    (LaTeX-add-labels
     "ProgramsAndRecursiveAndRELanguages"
     "RecProp"
     "REProp")
    (TeX-run-style-hooks
     "chap-5")))

