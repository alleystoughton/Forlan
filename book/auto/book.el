(TeX-add-style-hook "book"
 (lambda ()
    (LaTeX-add-bibliographies)
    (TeX-add-symbols
     "item")
    (TeX-run-style-hooks
     "breakurl"
     "hyperref"
     "pdfpagelabels"
     "fancyheadings"
     "alltt"
     "index"
     "calc"
     "theorem"
     "url"
     "enumerate"
     "moreverb"
     "verbatim"
     "amsmath"
     "amssymb"
     "latexsym"
     "eepic"
     "epic"
     "latex2e"
     "bk11"
     "11pt"
     "twoside"
     "defs"
     "commands"
     "preface"
     "chap-1"
     "chap-2"
     "chap-3"
     "chap-4"
     "chap-5")))

