(TeX-add-style-hook "chap-1.1"
 (lambda ()
    (LaTeX-add-index-entries
     "set|("
     "empty set"
     "set!empty"
     " empty@$\\emptyset$"
     "set! empty@$\\emptyset$"
     " set@$\\{\\ldots\\}$"
     "set! set@$\\{\\ldots\\}$"
     "singleton set"
     "set!singleton"
     "n@$\\nats$"
     "natural number"
     "z@$\\ints$"
     "integer"
     "r@$\\reals$"
     "real number"
     "element of"
     "set!element of"
     " element of@$\\in$"
     "set! element of@$\\in$"
     "equal!set"
     "set!equal"
     " equal@$=$"
     "set! equal@$=$"
     "iff"
     "subset"
     "set!subset"
     " subset@$\\sub$"
     "set! subset@$\\sub$"
     "proper!subset"
     "subset!proper"
     "set!subset!proper"
     " proper subset@$\\subsetneq$"
     "set! proper subset@$\\subsetneq$"
     "superset"
     "set!superset"
     " superset@$\\supseteq$"
     "set! superset@$\\supseteq$"
     "proper!superset"
     "superset!proper"
     "set!superset!proper"
     " proper superset@$\\supsetneq$"
     "set! proper superset@$\\supsetneq$"
     "forming sets|("
     "set!formation|("
     " set of all@$\\setof{\\cdots}{\\cdots}$"
     "set! set of all@$\\setof{\\cdots}{\\cdots}$"
     "bound variable"
     "universally quantified"
     "quantification!universal"
     "existentially quantified"
     "quantification!existential"
     "integers!interval"
     "integers! interval@$[\\cdot:\\cdot]$"
     "natural numbers!interval"
     "natural numbers! interval@$[\\cdot:\\cdot]$"
     " interval@$[\\cdot:\\cdot]$"
     "forming sets|)"
     "set!formation|)"
     " union@$\\cup$"
     "set! union@$\\cup$"
     "union!set"
     "set!union|see{union, set}"
     " intersection@$\\cap$"
     "set! intersection@$\\cap$"
     "intersection!set"
     "set!intersection|see{intersection, set}"
     " difference@$-$"
     "set! difference@$-$"
     "difference!set"
     "set!difference"
     " product@$\\times$"
     "set! product@$\\times$"
     "product"
     "set!product"
     "powerset@$\\powset$"
     "set!powerset@$\\powset$"
     "powerset"
     "set!powerset"
     "commutative!union"
     "commutative!intersection"
     "union!set!commutative"
     "intersection!set!commutative"
     "associative!union"
     "associative!intersection"
     "union!set!associative"
     "intersection!set!associative"
     "idempotent!union"
     "union!set!idempotent"
     "identity!union"
     "union!set!identity"
     "idempotent!intersection"
     "intersection!set!idempotent"
     "zero!intersection"
     "intersection!set!zero"
     "ordered pair"
     " ordered pair@$(\\cdot,\\cdot)$"
     "ordered tuple@ordered $n$-tuple"
     "ordered triple"
     " ordered triple@$(\\cdot,\\cdot,\\cdot)$"
     "distributivity"
     "generalized union"
     "union!set!generalized"
     " generalized union@$\\bigcup$"
     "set! generalized union@$\\bigcup$"
     "generalized intersection"
     "intersection!set!generalized"
     " generalized intersection@$\\bigcap$"
     "set! generalized intersection@$\\bigcap$"
     "relation"
     "domain"
     "relation!domain"
     "domain@$\\domain$"
     "relation!domain@$\\domain$"
     "range"
     "relation!range"
     "range@$\\range$"
     "relation!range@$\\range$"
     "relation from set to set"
     "forming sets"
     "set!formation"
     "identity relation"
     "relation!identity"
     "id@$\\id$"
     "relation!id@$\\id$"
     "relation!composition"
     "composition!relation|see{relation, composition}"
     "associative!relation composition"
     " composition@$\\circ$"
     "relation! composition@$\\circ$"
     "relation!composition!associative"
     "identity!relation composition"
     "relation!composition!identity"
     "relation!inverse"
     "reflexive on set"
     "relation!reflexive on set"
     "transitive"
     "relation!transitive"
     "symmetric"
     "relation!symmetric"
     "antisymmetric"
     "relation!antisymmetric"
     "total"
     "relation!total"
     "function"
     "relation!function|see{function}"
     "total ordering"
     "application|see{function, application}"
     "function!application"
     " function application@$\\cdot\\,\\cdot$"
     "function! application@$\\cdot\\,\\cdot$"
     "function!from set to set"
     " set of all functions@$\\fun$"
     "set! set of all functions@$\\fun$"
     "function!equality"
     "identity function"
     "function!identity"
     "function!id@$\\id$"
     "function!composition"
     "composition!function|see{function, composition}"
     "function! composition@$\\circ$"
     "associative!function composition"
     "function!composition!associative"
     "identity!function composition"
     "function!composition!identity"
     "image under!function|see{function, image under}"
     "function!image under"
     "function! image under@$\\cdot(\\cdot)$"
     " image under@$\\cdot(\\cdot)$"
     "inverse image under!function|see{function, inverse image under}"
     "function!inverse image under"
     "function! inverse image under@$\\cdot^{-1}(\\cdot)$"
     " inverse image under@$\\cdot^{-1}(\\cdot)$"
     "restriction!function|see{function, restriction}"
     "function!restriction"
     "function! restriction@$\\cdot\\restr\\cdot$"
     " restriction@$\\cdot\\restr \\cdot$"
     "updating!function|see{function, updating}"
     "function!updating"
     "function! updating@$\\cdot[\\cdot\\mapsto\\cdot]$"
     " updating@$\\cdot[\\cdot\\mapsto\\cdot]$"
     " hash@$\\hash{\\cdot}$"
     "projection"
     "tuple!projection"
     "set!cardinality|("
     "set!size|("
     "size!set|("
     "cardinality|("
     "bijection from set to set"
     "function!bijection from set to set"
     "one-to-one correspondence"
     "injection"
     "injective"
     "function!injection"
     "function!injective"
     "same size"
     "set!same size"
     " same size@$\\cong$"
     "set! same size@$\\cong$"
     "finite"
     "set!finite"
     "infinite"
     "set!infinite"
     "countably infinite"
     "infinite!countably|see{countably infinite}"
     "set!infinite!countably|see{countably infinite}"
     "countable"
     "set!countable"
     "uncountable"
     "set!uncountable|see{uncountable}"
     " size of@$\\sizedot$"
     "set! size of@$\\sizedot$"
     "diagonalization!cardinality"
     "proof by contradiction"
     "contradiction!proof by"
     "no bigger"
     "set!no bigger"
     " no bigger than@$\\preceq$"
     "set! no bigger than@$\\preceq$"
     "strictly smaller"
     "set!strictly smaller"
     "Schr\\\"oder-Bernstein Theorem"
     "Axiom of Choice"
     "set!cardinality|)"
     "set!size|)"
     "size!set|)"
     "cardinality|)"
     "data structure|("
     "boolean|("
     "true@$\\true$"
     "false@$\\false$"
     "Bool@$\\Bool$"
     "boolean!true@$\\true$"
     "boolean!false@$\\false$"
     "boolean!Bool@$\\Bool$"
     "not@$\\mynot$"
     "boolean!not@$\\mynot$"
     "boolean!negation"
     "and@$\\myand$"
     "boolean!and@$\\myand$"
     "boolean!conjunction"
     "or@$\\myor$"
     "boolean!or@$\\myor$"
     "boolean!disjunction"
     "boolean|)"
     "option"
     "none@$\\none$"
     "some@$\\some\\,\\cdot$"
     "Option@$\\Option$"
     "option!none@$\\none$"
     "option!some@$\\some\\,\\cdot$"
     "option!Option@$\\Option$"
     "list|("
     " list@$[\\cdot,\\cdots,\\cdot]$"
     "lists! list@$[\\cdot,\\cdots,\\cdot]$"
     "list!concatenation"
     "concatenation!list"
     " at@$\\myconcat$"
     "list! at@$\\myconcat$"
     "identity!list concatenation"
     "concatenation!list!identity"
     "list!concatenation!identity"
     "associative!list concatenation"
     "concatenation!list!associative"
     "list!concatenation!associative"
     "List@$\\List\\,\\cdot$"
     "list!List@$\\List\\,\\cdot$"
     "list!list@$\\cdot$-list"
     "list@$\\cdot$-list"
     "list|)"
     "data structure|)"
     "set|)")
    (LaTeX-add-labels
     "BasisSetTheory"
     "InterOverUnionProp"
     "BijectionEx1"
     "BijectionEx2"
     "DiagCard"
     "PowsetNatsUncountableProp")))

