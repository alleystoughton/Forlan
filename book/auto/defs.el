(TeX-add-style-hook "defs"
 (lambda ()
    (LaTeX-add-environments
     '("proofof" 1)
     "proof"
     "mtabbing"
     "ctabbing"
     "myalltt"
     "theorem"
     "lemma"
     "corollary"
     "conjecture"
     "notation"
     "proposition"
     "openproblem"
     "example"
     "counterexample"
     "exercise"
     "definition")
    (TeX-add-symbols
     '("casesdef" 4)
     '("eqtxtn" 1)
     '("eqtxtr" 1)
     '("eqtxtl" 1)
     '("eqtxt" 1)
     '("powset" 1)
     '("singset" 1)
     '("setof" 2)
     '("byproposition" 1)
     '("bycorollary" 1)
     '("bytheorem" 1)
     '("bylemma" 1)
     '("by" 1)
     '("boper" 1)
     '("moper" 1)
     '("oper" 1)
     '("theoremnumber" 1)
     '("mtab" 1)
     "TS"
     "NL"
     "TSN"
     "NT"
     "clearemptydoublepage"
     "abr"
     "gbr"
     "hquad"
     "fun"
     "restr"
     "sub"
     "tildesym"
     "underscoresym"
     "primesym"
     "quotesym"
     "uparrowsym")))

