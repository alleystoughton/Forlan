(TeX-add-style-hook "chap-3.3"
 (lambda ()
    (LaTeX-add-index-entries
     "regular expression!simplification|("
     "simplification!regular expression|("
     "numConcats@$\\numConcats$"
     "regular expression!numConcats@$\\numConcats$"
     "regular expression!number of concatenations"
     "numSyms@$\\numSyms$"
     "regular expression!numSyms@$\\numSyms$"
     "regular expression!number of symbols"
     "standardized"
     "regular expression!standardized"
     "weakly simplified"
     "regular expression!weakly simplified"
     "simplification!regular expression!weakly simplified"
     "hasEmp@$\\hasEmp$"
     "regular expression!hasEmp@$\\hasEmp$"
     "regular expression!testing for membership of empty string"
     "hasSym@$\\hasSym$"
     "regular expression!hasSym@$\\hasSym$"
     "regular expression!testing for membership of symbol"
     "obviousSubset@$\\obviousSubset$"
     "regular expression!obviousSubset@$\\obviousSubset$"
     "regular expression!conservative subset test"
     "regular expression!conservative approximation to subset testing"
     "conservative approximation to subset testing"
     "structural rule"
     "regular expression!structural rule"
     "simplification!regular expression!structural rule"
     "regular expression!simplification|)"
     "simplification!regular expression|)"
     "regular expression|)")
    (LaTeX-add-labels
     "SimplificationOfRegularExpressions"
     "CCEquivContext"
     "CCLTContext"
     "WeakSimpProp3"
     "WeakSimpProp2"
     "WeakSimpProp4"
     "ShiftClosuresRightLem"
     "DeepClosureLem"
     "DeepConcatLem"
     "DeepUnionLem"
     "WeakSimpProp1"
     "WeakSimpExercise"
     "HasEmpProp"
     "HasSymProp"
     "WeakSubProp"
     "LTCCWellFounded")
    (TeX-run-style-hooks
     "chap-3")))

