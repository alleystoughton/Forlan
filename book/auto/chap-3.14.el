(TeX-add-style-hook "chap-3.14"
 (lambda ()
    (LaTeX-add-index-entries
     "pumping lemma!regular languages|("
     "regular language!pumping lemma|("
     "regular language!showing that languages are non-regular|("
     "pumping lemma!regular languages|)"
     "regular language!pumping lemma|)"
     "regular language!showing that languages are non-regular|)")
    (LaTeX-add-labels
     "ThePumpingLemmaForRegularLanguages"
     "NonRegularProp1")
    (TeX-run-style-hooks
     "chap-3")))

