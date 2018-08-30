(TeX-add-style-hook "chap-2.3"
 (lambda ()
    (LaTeX-add-index-entries
     "Forlan|("
     "Forlan!installing"
     "Forlan!running"
     "unit@\\texttt{unit}"
     " empty tuple@\\texttt{()}"
     "Forlan!primary prompt"
     "prompt!primary"
     "Forlan!exiting"
     "Forlan!interrupting"
     "Standard ML!value"
     "Standard ML!type"
     "int@\\texttt{int}"
     "Standard ML!int@\\texttt{int}"
     "Standard ML!semicolon@\\texttt{;}"
     "Standard ML!product type"
     "string@\\texttt{string}"
     "bool@\\texttt{bool}"
     "Standard ML!string@\\texttt{string}"
     "Standard ML!bool@\\texttt{bool}"
     "Standard ML!option type"
     "Standard ML!NONE@\\texttt{NONE}"
     "Standard ML!SOME@\\texttt{NONE}"
     "Standard ML!declaration"
     "val@\\texttt{val}"
     "Forlan!secondary prompt"
     "prompt!secondary"
     "Standard ML!function"
     "function"
     "fn@\\texttt{fn}"
     "Standard ML!function type"
     "Standard ML!precedence"
     "Standard ML!composition"
     "Standard ML! composition@\\texttt{o}"
     "Standard ML!associativity"
     "Standard ML!function!recursive"
     "use@\\texttt{use}"
     "Standard ML|)"
     "Sym@\\texttt{Sym}"
     "sym@\\texttt{sym}"
     "Sym@\\texttt{Sym}!sym@\\texttt{sym}"
     "symbol"
     "Sym@\\texttt{Sym}!input@\\texttt{input}"
     "Sym@\\texttt{Sym}!output@\\texttt{output}"
     "Sym@\\texttt{Sym}!compare@\\texttt{compare}"
     "interactive input"
     "Forlan!input prompt"
     "prompt!input"
     "Sym@\\texttt{Sym}!fromString@\\texttt{fromString}"
     "Sym@\\texttt{Sym}!toString@\\texttt{toString}"
     "Set@\\texttt{Set}"
     "set@\\texttt{\\primesym a~set}"
     "Set@\\texttt{Set}!set@\\texttt{\\primesym a~set}"
     "set!finite"
     "Set@\\texttt{Set}!toList@\\texttt{toList}"
     "Set@\\texttt{Set}!size@\\texttt{size}"
     "Set@\\texttt{Set}!empty@\\texttt{empty}"
     "Set@\\texttt{Set}!sing@\\texttt{sing}"
     "Set@\\texttt{Set}!filter@\\texttt{filter}"
     "SymSet@\\texttt{SymSet}"
     "SymSet@\\texttt{SymSet}!input@\\texttt{input}"
     "SymSet@\\texttt{SymSet}!output@\\texttt{output}"
     "SymSet@\\texttt{SymSet}!fromList@\\texttt{fromList}"
     "SymSet@\\texttt{SymSet}!memb@\\texttt{memb}"
     "SymSet@\\texttt{SymSet}!subset@\\texttt{subset}"
     "SymSet@\\texttt{SymSet}!equal@\\texttt{equal}"
     "SymSet@\\texttt{SymSet}!union@\\texttt{union}"
     "SymSet@\\texttt{SymSet}!inter@\\texttt{inter}"
     "SymSet@\\texttt{SymSet}!minus@\\texttt{minus}"
     "SymSet@\\texttt{SymSet}!genUnion@\\texttt{genUnion}"
     "SymSet@\\texttt{SymSet}!genInter@\\texttt{genInter}"
     "Str@\\texttt{Str}"
     "Str@\\texttt{Str}!str@\\texttt{str}"
     "str@\\texttt{str}"
     "string"
     "string!Forlan syntax"
     "Forlan!string syntax"
     "Str@\\texttt{Str}!input@\\texttt{input}"
     "Str@\\texttt{Str}!output@\\texttt{output}"
     "Str@\\texttt{Str}!alphabet@\\texttt{alphabet}"
     "Str@\\texttt{Str}!compare@\\texttt{compare}"
     "Str@\\texttt{Str}!prefix@\\texttt{prefix}"
     "Str@\\texttt{Str}!suffix@\\texttt{suffix}"
     "Str@\\texttt{Str}!substr@\\texttt{substr}"
     "Str@\\texttt{Str}!power@\\texttt{power}"
     "Str@\\texttt{Str}!last@\\texttt{last}"
     "Str@\\texttt{Str}!allButLast@\\texttt{allButLast}"
     "StrSet@\\texttt{StrSet}"
     "language"
     "StrSet@\\texttt{StrSet}!input@\\texttt{input}"
     "StrSet@\\texttt{StrSet}!output@\\texttt{output}"
     "StrSet@\\texttt{StrSet}!fromList@\\texttt{fromList}"
     "StrSet@\\texttt{StrSet}!memb@\\texttt{memb}"
     "StrSet@\\texttt{StrSet}!subset@\\texttt{subset}"
     "StrSet@\\texttt{StrSet}!equal@\\texttt{equal}"
     "StrSet@\\texttt{StrSet}!union@\\texttt{union}"
     "StrSet@\\texttt{StrSet}!inter@\\texttt{inter}"
     "StrSet@\\texttt{StrSet}!minus@\\texttt{minus}"
     "StrSet@\\texttt{StrSet}!genUnion@\\texttt{genUnion}"
     "StrSet@\\texttt{StrSet}!genInter@\\texttt{getInter}"
     "StrSet@\\texttt{StrSet}!alphabet@\\texttt{alphabet}"
     "SymRel@\\texttt{SymRel}"
     "sym_rel@\\texttt{sym\\underscoresym rel}"
     "SymRel@\\texttt{SymRel}!sym_rel@\\texttt{sym\\underscoresym rel}"
     "relation"
     "ordered pair"
     " ordered pair@$(\\cdot,\\cdot)$"
     "SymRel@\\texttt{SymRel}!input@\\texttt{input}"
     "SymRel@\\texttt{SymRel}!output@\\texttt{output}"
     "SymRel@\\texttt{SymRel}!fromList@\\texttt{fromList}"
     "SymRel@\\texttt{SymRel}!memb@\\texttt{memb}"
     "SymRel@\\texttt{SymRel}!subset@\\texttt{subset}"
     "SymRel@\\texttt{SymRel}!equal@\\texttt{equal}"
     "SymRel@\\texttt{SymRel}!union@\\texttt{union}"
     "SymRel@\\texttt{SymRel}!inter@\\texttt{inter}"
     "SymRel@\\texttt{SymRel}!minus@\\texttt{minus}"
     "SymRel@\\texttt{SymRel}!genUnion@\\texttt{genUnion}"
     "SymRel@\\texttt{SymRel}!genInter@\\texttt{genInter}"
     "SymRel@\\texttt{SymRel}!domain@\\texttt{domain}"
     "SymRel@\\texttt{SymRel}!range@\\texttt{range}"
     "SymRel@\\texttt{SymRel}!relationFromTo@\\texttt{relationFromTo}"
     "SymRel@\\texttt{SymRel}!reflexive@\\texttt{reflexive}"
     "SymRel@\\texttt{SymRel}!symmetric@\\texttt{symmetric}"
     "SymRel@\\texttt{SymRel}!antisymmetric@\\texttt{antisymmetric}"
     "SymRel@\\texttt{SymRel}!transitive@\\texttt{transitive}"
     "SymRel@\\texttt{SymRel}!total@\\texttt{total}"
     "SymRel@\\texttt{SymRel}!inverse@\\texttt{inverse}"
     "SymRel@\\texttt{SymRel}!compose@\\texttt{compose}"
     "SymRel@\\texttt{SymRel}!function@\\texttt{function}"
     "SymRel@\\texttt{SymRel}!functionFromTo@\\texttt{functionFromTo}"
     "SymRel@\\texttt{SymRel}!injection@\\texttt{injection}"
     "SymRel@\\texttt{SymRel}!bijectionFromTo@\\texttt{bijectionFromTo}"
     "SymRel@\\texttt{SymRel}!applyFunction@\\texttt{applyFunction}"
     "curried function"
     "Standard ML!curried function"
     "Standard ML!function!curried"
     "Forlan|)")
    (LaTeX-add-labels
     "IntroductionToForlan")
    (TeX-run-style-hooks
     "chap-2")))

