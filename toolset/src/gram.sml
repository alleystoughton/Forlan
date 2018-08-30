(********************************** gram.sml *********************************)

(* Copyright (C) 2012 Alley Stoughton

   The file is part of the Forlan toolset for experimenting with
   formal languages.  See the file COPYING.txt for copying and
   usage restrictions. *)

structure Gram :> GRAM =
struct

structure M  = Messages
structure L  = Lex

(*********************************** Types ***********************************)

type concr =
       {vars  : Sym.sym Set.set,
        start : Sym.sym,
        prods : Prod.prod Set.set}
               
type gram = concr

fun checkStart(start, vars) =
      if not(SymSet.memb(start, vars))
      then M.errorPP
           (fn () =>
                 [PP.fromString "invalid", PP.fromString "start",
                  PP.fromString "variable:", PP.quote(Sym.toPP start)])
      else ()

fun checkProductions(nil,             _)    = ()
  | checkProductions((q, _) :: prods, vars) =
      if not(SymSet.memb(q, vars))
      then M.errorPP
           (fn () =>
                 [PP.fromString "invalid", PP.fromString "left-hand-side",
                  PP.fromString "of", PP.fromString "production:",
                  PP.quote(Sym.toPP q)])
      else checkProductions(prods, vars)
      
fun check{vars, start, prods} =
      (checkStart(start, vars);
       checkProductions(Set.toList prods, vars))

fun valid concr =
      (M.quiet(fn () => check concr); true)
        handle _ => false

fun fromConcr(concr : concr) : gram  = (check concr; concr)

fun toConcr(gram : gram) : concr = gram

(*********************************** Input ***********************************)

fun inpGram lts =
      let val lts          =
                L.checkInLabToks(L.Heading "{variables}", lts)
          val (vars, lts)  = SymSet.inputFromLabToks lts
          val lts          =
                L.checkInLabToks(L.Heading "{startvariable}", lts)
          val (start, lts) = Sym.inputFromLabToks lts
          val lts          =
                L.checkInLabToks(L.Heading "{productions}", lts)
          val (prods, lts) = ProdSet.inputFromLabToks lts
          val concr        =
                {vars  = vars,
                 start = start,
                 prods = prods}
      in (fromConcr concr, lts) end

fun fromString s =
      case inpGram(L.lexString s) of
           (gram, [(_, L.EOF)]) => gram
         | (_,    nil)          => M.cannotHappen() 
         | (_,    lt :: _)      => L.unexpectedTok lt

fun input fil =
      case inpGram(L.lexFile fil) of
           (gram, [(_, L.EOF)]) => gram
         | (_,    nil)          => M.cannotHappen() 
         | (_,    lt :: _)      => L.unexpectedTok lt

(********************************** Output **********************************)

fun toPP{vars, start, prods} =
      PP.block(true,
               [PP.block(true,
                         [PP.fromString "{variables}",
                          SymSet.toPP vars]),
                PP.block(true,
                         [PP.fromString "{start variable}",
                          Sym.toPP start]),
                PP.block(true,
                         [PP.fromString "{productions}",
                          ProdSet.toPP prods])])

val toString = PP.toString o toPP

fun output("",  gram) = (print(toString gram); print PP.newline)
  | output(fil, gram) =
      case SOME(TextIO.openOut fil) handle _ => NONE of
           NONE     =>
             M.errorPP
             (fn () =>
                   [PP.fromString "unable", PP.fromString "to",
                    PP.fromString "open", PP.fromString "file:",
                    PP.quote(PP.fromStringSplitEscape fil)])
         | SOME stm =>
             (TextIO.output(stm, toString gram);
              TextIO.output(stm, PP.newline);
              TextIO.closeOut stm)

(****************************** More Functions ******************************)

fun variables (gram : gram) = #vars gram

fun startVariable (gram : gram) = #start gram

fun productions (gram : gram) = #prods gram

fun compare(gram1, gram2) =
      case SymSet.compare(variables gram1, variables gram2) of
           LESS    => LESS
         | EQUAL   =>
             (case Sym.compare(startVariable gram1, startVariable gram2) of
                   LESS    => LESS
                 | EQUAL   =>
                     ProdSet.compare(productions gram1, productions gram2)
                 | GREATER => GREATER)
         | GREATER => GREATER

fun equal gramPair = compare gramPair = EQUAL

fun numVariables gram = Set.size(variables gram)

fun numProductions gram = Set.size(productions gram)

fun alphabet gram =
      let val rightSyms =
                SymSet.genUnion(Set.mapToList (fn (_, xs) =>
                                                    SymSet.fromList xs)
                                              (productions gram))
      in SymSet.minus(rightSyms, variables gram) end

fun sub(gram1, gram2) =
      SymSet.subset(variables gram1, variables gram2)     andalso
      Sym.equal(startVariable gram1, startVariable gram2) andalso
      ProdSet.subset(productions gram1, productions gram2)      

fun rightSides gram q =
      let val prods = productions gram

          fun rightSides nil               = Set.empty
            | rightSides ((r, x) :: prods) =
                (case Sym.compare(q, r) of
                      LESS    => Set.empty
                    | EQUAL   => StrSet.union(Set.sing x, rightSides prods)
                    | GREATER => rightSides prods)
      in rightSides(Set.toList prods) end

fun checkPT gram =
      let val prods = productions gram
          val vars  = variables gram
          val alpha = alphabet gram

          fun errSym a =
                M.errorPP
                (fn () =>
                      [PP.fromString "invalid",
                       PP.fromString "symbol:",
                       PP.quote(Sym.toPP a)])

          fun errProd prod =
                M.errorPP
                (fn () =>
                      [PP.fromString "invalid",
                       PP.fromString "production:",
                       PP.quote(ProdSet.toPP(Set.sing prod))])

          fun check pt =
                case PT.decons pt of
                     (q, NONE)     =>
                       if ProdSet.memb((q, nil), prods)
                       then ()
                       else errProd(q, nil)
                   | (a, SOME nil) =>
                       if SymSet.memb(a, alpha) orelse
                          SymSet.memb(a, vars)
                       then ()
                       else errSym a  (* can only happen at top-level *)
                   | (q, SOME pts) =>
                       let val bs = map PT.rootLabel pts
                       in if ProdSet.memb((q, bs), prods)
                          then app check pts
                          else errProd(q, bs)
                       end
      in check end

fun validPT gram pt =
      (M.quiet(fn () => checkPT gram pt); true)
        handle _ => false

fun validVariablesRenaming(gram, rel) =
      let val vars  = variables gram
          val alpha = alphabet gram
      in SymRel.bijectionFromAvoiding(rel, vars, alpha) end

fun renameVariables(gram as {vars, start, prods}, rel) =
      if validVariablesRenaming(gram, rel)
      then let val renam           = SymRel.applyFunction rel
               val renameVarsInStr =
                     map(fn a =>
                              if SymSet.memb(a, vars)
                              then renam a
                              else a)
               val vars'           = SymSet.map renam vars
               val start'          = renam start
               val prods'          =
                     ProdSet.map (fn (q, x) => (renam q, renameVarsInStr x))
                                 prods
           in {vars  = vars',
               start = start',
               prods = prods'}
           end
      else M.errorString
           (fn () => ["invalid", "variables", "renaming", "for", "grammar"])

fun renameVariablesCanonically gram =
      let val vars  = variables gram
          val alpha = alphabet gram

          fun position q =
                valOf(Set.position (fn p => Sym.equal(p, q)) vars)

          val renamPref =
                if Set.size vars <= 26
                then let fun renam q =
                               let val s = str(chr(ord #"A" + position q - 1))
                               in Sym.fromString s end
                     in renam end
                else let fun renam q =
                               let val ss =
                                         ["<", Int.toString(position q), ">"]
                               in Sym.fromString(String.concat ss) end
                     in renam end
          val relPref   = SymRel.mlFunctionToFunction(renamPref, vars)
          val vars'Pref = SymSet.map renamPref vars          
          val rel'      = SymRel.makeBijectionFromAvoiding(vars'Pref, alpha)
          val rel''     = SymRel.compose(rel', relPref)
      in renameVariables(gram, rel'') end

fun isomorphism(gram1, gram2, rel) =
      validVariablesRenaming(gram1, rel) andalso
      equal(gram2, renameVariables(gram1, rel))

fun findIsomorphismOpt(gram1, gram2) =
      let val vars1     = variables gram1
          val start1    = startVariable gram1
          val prods1    = productions gram1
          val nonStart1 = SymSet.minus(vars1, Set.sing start1)
          val alpha1    = alphabet gram1
          val rights1   = rightSides gram1

          val vars2     = variables gram2
          val start2    = startVariable gram2
          val prods2    = productions gram2
          val nonStart2 = SymSet.minus(vars2, Set.sing start2)
          val alpha2    = alphabet gram2
          val rights2   = rightSides gram2

          fun findIso(rel, _,   _,   nil)                   = SOME rel
            | findIso(rel, dom, ran, (xs, ys) :: constrnts) =
                case Set.size xs of
                     0 => findIso(rel, dom, ran, constrnts)
                   | 1 =>
                       let val x = hd(Set.toList xs)
                           val y = hd(Set.toList ys)
                       in if length x = 1
                          then simple(rel, dom, ran, (hd x, hd y), constrnts)
                          else moderate(rel, dom, ran, (x, y), constrnts)
                       end
                   | _ => complex(rel, dom, ran, (xs, ys), constrnts)

          and simple(rel, dom, ran, (b, c), constrnts) =
                if not(SymSet.memb(b, vars1))
                  then if SymSet.memb(c, vars2)
                         then NONE
                       else if Sym.equal(b, c)
                         then findIso(rel, dom, ran, constrnts)
                       else NONE
                else if not(SymSet.memb(c, vars2))
                  then NONE
                else if SymRel.memb((b, c), rel)
                  then findIso(rel, dom, ran, constrnts)
                else if SymSet.memb(b, dom) orelse SymSet.memb(c, ran)
                  then NONE
                else let val rel = SymRel.union(Set.sing(b, c), rel)
                         val dom = SymSet.union(Set.sing b, dom)
                         val ran = SymSet.union(Set.sing c, ran)
                         val xs  = rights1 b
                         val ys  = rights2 c

                         fun info zs =
                               (Set.size zs,
                                ListAux.sum(Set.mapToList length zs))
                     in if info xs <> info ys
                        then NONE
                        else findIso(rel, dom, ran, (xs, ys) :: constrnts)
                     end

          and moderate(rel, dom, ran, (bs, cs), constrnts) =
                let fun newConstraints (nil,     nil)     = SOME nil
                      | newConstraints (b :: bs, c :: cs) =
                          if not(SymSet.memb(b, vars1))
                            then if SymSet.memb(c, vars2)
                                   then NONE
                                 else if Sym.equal(b, c)
                                   then newConstraints(bs, cs)
                                 else NONE
                          else if not(SymSet.memb(c, vars2))
                            then NONE
                          else (case newConstraints(bs, cs) of
                                     NONE      => NONE
                                   | SOME news =>
                                       SOME((Set.sing[b], Set.sing[c]) ::
                                            news))
                      | newConstraints _                  = M.cannotHappen()
                in case newConstraints(bs, cs) of
                        NONE      => NONE
                      | SOME news => findIso(rel, dom, ran, news @ constrnts)
                end

          and complex(rel, dom, ran, (xs, ys), constrnts) =
                let val x  = Set.hd xs
                    val xs = Set.tl xs

                    fun tryPairings nil       = NONE
                      | tryPairings (z :: zs) =
                          if length z <> length x
                          then tryPairings zs
                          else let val ys   = StrSet.minus(ys, Set.sing z)
                                   val news =
                                         [(Set.sing x, Set.sing z), (xs, ys)]
                               in case findIso(rel, dom, ran,
                                               news @ constrnts) of
                                       NONE   => tryPairings zs
                                     | relOpt => relOpt
                               end
                in tryPairings(Set.toList ys) end
      in if Set.size vars1 <> Set.size vars2   orelse
            Set.size prods1 <> Set.size prods2 orelse
            not(SymSet.equal(alpha1, alpha2))
         then NONE
         else let val constrnts =
                        [(Set.sing[start1], Set.sing[start2]),
                         (StrSet.map (fn q => [q]) nonStart1,
                          StrSet.map (fn q => [q]) nonStart2)]
              in findIso(Set.empty, Set.empty, Set.empty, constrnts) end
      end

fun findIsomorphism(gram1, gram2) =
      case findIsomorphismOpt(gram1, gram2) of
           NONE     => M.errorString(fn () => ["not", "isomorphic"])
         | SOME rel => rel

fun isomorphic(gram1, gram2) =
      case findIsomorphismOpt(gram1, gram2) of
           NONE   => false
         | SOME _ => true

fun renameAlphabet(gram, rel) =
      if SymRel.bijectionFromSupersetAvoiding(rel, alphabet gram, Set.empty)
      then let val vars  = variables gram
               val start = startVariable gram
               val prods = productions gram
               val alpha = alphabet gram

               val rel      = SymRel.restrictFunction(rel, alpha)
               val alpha'   = SymRel.range rel
               val relVars  = SymRel.makeBijectionFromAvoiding(vars, alpha')
               val renamVar = SymRel.applyFunction relVars

               fun renam a =
                     if SymSet.memb(a, vars)
                     then renamVar a
                     else SymRel.applyFunction rel a

               val vars'  = SymSet.map renamVar vars
               val start' = renamVar start
               val prods' =
                     ProdSet.map
                     (fn (q, x) => (renamVar q, map renam x))
                     prods
           in {vars  = vars',
               start = start',
               prods = prods'}
           end
      else M.errorString
           (fn () => ["invalid", "alphabet", "renaming", "for", "grammar"])

(* in a call to parsableCore, vars is the variables of gram, prods is
   the productions of gram, alpha is the alphabet of gram, syms is the
   alphabet of w, a is in vars or syms, and each symbol of w is in
   vars or alpha *)

fun parsableCore(gram, a, w, vars, prods, alpha, syms) =
      let val prods =
                Set.filter
                (fn (_, y) =>
                      List.all
                      (fn b =>
                            SymSet.memb(b, vars) orelse
                            SymSet.memb(b, syms))
                      y)
                prods

          val emptyProds    = Set.filter (fn (b, y) => null y) prods
          val nonemptyProds = Set.filter (fn (b, y) => not(null y)) prods

          (* (b, x) : parsable has the property that b is in vars or is a
             symbol of w, x is a substring of w, and there is a parse tree
             valid for gram with b as its root label and yield x *)

          type parsable = Sym.sym * Str.str

          val compareParsable : parsable * parsable -> order =
                Set.comparePair(Sym.compare, Str.compare)
          val unionParsableSet                               =
                Set.union compareParsable
          val minusParsableSet                               =
                Set.minus compareParsable
          val membParsableSet                                =
                Set.memb compareParsable

          fun parsablesWithLab parbls c =
                Set.filter (fn (b, _) => Sym.equal(b, c)) parbls

          fun parsableListsOfNonemptyRHS(olds, news, y) =
                let val alls = unionParsableSet(olds, news)

                    fun parsableLists(_, nil)    = Set.empty
                      | parsableLists(u, b :: v) =
                          Set.union
                          (Set.compareList compareParsable)
                          (let val xs = parsablesWithLab news b
                           in if Set.isEmpty xs
                              then Set.empty
                              else Set.genTimes
                                   (map (parsablesWithLab alls) u @
                                    [xs] @
                                    map (parsablesWithLab alls) v)
                           end,
                           parsableLists(u @ [b], v))
                in parsableLists(nil, y) end

          fun parsablesOfNonemptyProd (olds : parsable Set.set,
                                       news : parsable Set.set)
                                      (r, y) =
                let val parbls =  (* won't always meet substring requirement *)
                          Set.map compareParsable
                                  (fn parsableList =>
                                        (r, List.concat(map #2 parsableList)))
                                  (parsableListsOfNonemptyRHS(olds, news, y))
                in Set.filter (fn parbl => Str.substr(#2 parbl, w))
                              parbls
                end

          fun parsablesOfNonemptyProds(olds, news, prods) =
                Set.genUnion compareParsable
                             (Set.mapToList (parsablesOfNonemptyProd
                                             (olds, news))
                                            prods)

          (* in a call iterate(olds, news), olds and news are disjoint *)

          fun iterate(olds, news) =
                if Set.isEmpty news
                then olds
                else let val alls = unionParsableSet(olds, news)
                     in iterate
                        (alls,
                         minusParsableSet
                         (parsablesOfNonemptyProds(olds, news, nonemptyProds),
                          alls))
                     end

          val inits  =
                unionParsableSet
                (Set.map compareParsable
                         (fn b => (b, [b]))
                         syms,
                 Set.map compareParsable
                         (fn (b, _) => (b, nil))
                         emptyProds)
          val finals = iterate(Set.empty, inits)
      in membParsableSet((a, w), finals) end

fun parsable gram (a, w) =
      let val vars  = variables gram
          val prods = productions gram
          val alpha = alphabet gram
          val syms  = Str.alphabet w
      in SymSet.subset(syms, SymSet.union(vars, alpha))     andalso
         (SymSet.memb(a, vars) orelse SymSet.memb(a, syms)) andalso
         parsableCore(gram, a, w, vars, prods, alpha, syms)
      end

fun generatedFromVariable gram (q, w) =
      if SymSet.memb(q, variables gram)
      then SymSet.subset(Str.alphabet w, alphabet gram) andalso
           parsable gram (q, w)
      else M.errorString(fn () => ["symbol", "isn't", "variable"])

fun generated gram w = generatedFromVariable gram (startVariable gram, w)

(* in a call to parseOptCore, vars is the variables of gram, prods is
   the productions of gram, alpha is the alphabet of gram, syms is the
   alphabet of w, a is in vars or syms, and each symbol of w is in
   vars or alpha *)

fun parseOptCore(gram, a, w, vars, prods, alpha, syms) =
      let val prods =
                Set.filter
                (fn (_, y) =>
                      List.all
                      (fn b =>
                            SymSet.memb(b, vars) orelse
                            SymSet.memb(b, syms))
                      y)
                prods

          val emptyProds    = Set.filter (fn (b, y) => null y) prods
          val nonemptyProds = Set.filter (fn (b, y) => not(null y)) prods

          (* (b, x) : parsable has the property that b is in vars or is
             a symbol of w, x is a substring of w, and there is a parse tree
             valid for gram with b at its root and yield x *)

          type parsable = Sym.sym * Str.str

          val compareParsable : parsable * parsable -> order =
                Set.comparePair(Sym.compare, Str.compare)

          fun equalParsable(prsbl, prsbl') =
                compareParsable(prsbl, prsbl') = EQUAL

          (* ((b, x), (pt, n)) : binding has the property that (b, x)
             : parsable, pt is a valid parse tree for gram with root
             label b and whose yield is x, and n is the size of pt *)

          type binding = parsable * (PT.pt * int)

          val compareBinding : binding * binding -> order =
                Set.comparePair(compareParsable,
                                Set.comparePair(PT.compare, Int.compare))

          fun consBindings(b : Sym.sym, NONE : (binding list)option) =
                ((b, nil), (PT.cons(b, NONE), 2))
            | consBindings(b, SOME nil)                              =
                ((b, [b]), (PT.cons(b, SOME nil), 1))
            | consBindings(b, SOME bndgs)                            =
                ((b,
                  List.concat(map (#2 o #1) bndgs)),
                 (PT.cons(b, SOME(map (#1 o #2) bndgs)),
                  1 + ListAux.sum(map (#2 o #2) bndgs)))
          
          fun bindingsWithLab (bndgs : binding Set.set) c =
                Set.filter (fn ((b, y), _) => Sym.equal(b, c))
                           bndgs

          fun bindingListsOfNonemptyRHS(olds : binding Set.set,
                                        news : binding Set.set,
                                        y) =
                let val alls = Set.union compareBinding (olds, news)

                    fun bindingLists(_, nil)    = Set.empty
                      | bindingLists(u, b :: v) =
                          Set.union
                          (Set.compareList compareBinding)
                          (let val xs = bindingsWithLab news b
                           in if Set.isEmpty xs
                              then Set.empty
                              else Set.genTimes
                                   (map (bindingsWithLab alls) u @
                                    [xs] @
                                    map (bindingsWithLab alls) v)
                           end,
                           bindingLists(u @ [b], v))
                in bindingLists(nil, y) end

          fun bindingsOfNonemptyProd (olds, news) (r, y) =
                let val bindings = (* won't always meet substring requirement *)
                          Set.map compareBinding
                                  (fn bindingList =>
                                        consBindings(r, SOME bindingList))
                                  (bindingListsOfNonemptyRHS(olds, news, y))
                in Set.filter (fn bndg => Str.substr(#2(#1 bndg), w))
                              bindings
                end

          fun bindingsOfNonemptyProds(olds, news, prods) =
                Set.genUnion compareBinding
                             (Set.mapToList (bindingsOfNonemptyProd
                                             (olds, news))
                                            prods)

          (* table associating a parsable (b, x) with a pair (pt, n), where
             pt is a parse tree valid for gram with root b and yield x,
             and n is the size of pt *)

          type tab = (parsable, (PT.pt * int))Tab.tab

          val lookupParsable : tab * parsable -> (PT.pt * int)option =
                Tab.lookup compareParsable

          fun tabToSet(tab : tab) : binding Set.set =
                Set.fromList compareBinding (Tab.toList tab)

          fun bestBindingsToTab (bndgs : binding Set.set) : tab =
                let fun altCompare((prsbl, (pt, n)), (prsbl', (pt', n'))) =
                          case compareParsable(prsbl, prsbl') of
                               LESS    => LESS
                             | EQUAL   =>
                                 (case Int.compare(n, n') of
                                       LESS    => GREATER
                                     | EQUAL   => PT.compare(pt, pt')
                                     | GREATER => LESS)
                             | GREATER => GREATER

                    fun best nil                           = nil
                      | best [bndg]                        = [bndg]
                      | best ((bndg as (prsbl, _)) ::
                              (bndgs as (prsbl', _) :: _)) =
                          if equalParsable(prsbl, prsbl')
                          then best bndgs
                          else bndg :: best bndgs

                    val bndgs = Sort.sort (false, altCompare) (Set.toList bndgs)
                in Tab.fromList compareParsable (best bndgs) end

          (* a pair (olds', news') returned by this function will have
             disjoint domains *)

          fun reconcileOldAndNewTabs(olds : tab, news : tab) : tab * tab =
                let fun recon(olds' : binding list, news' : binding list,
                              nil, news)               =
                          (Tab.fromList compareParsable (rev olds'),
                           Tab.fromList compareParsable
                                        (List.revAppend(news', news)))
                      | recon(olds', news', olds, nil) =
                          (Tab.fromList compareParsable
                                        (List.revAppend(olds', olds)),
                           Tab.fromList compareParsable (rev news'))
                      | recon(olds', news', old_olds as old :: olds,
                              new_news as new :: news) =
                          case compareParsable(#1 old, #1 new) of
                               LESS    =>
                                 recon(old :: olds', news', olds, new_news)
                             | EQUAL   =>
                                 if #2(#2 old) <= #2(#2 new)
                                 then recon(old :: olds', news', olds, news)
                                 else recon(olds', new :: news', olds, news)
                             | GREATER =>
                                 recon(olds', new :: news', old_olds, news)
                in recon(nil, nil, Tab.toList olds, Tab.toList news) end

          (* in a call iterate(olds, news), the domains of olds and
             news are disjoint *)

          fun iterate(olds : tab, news : tab) : tab =
                if Tab.isEmpty news
                then olds
                else let val olds  = tabToSet olds
                         val news  = tabToSet news
                         val bndgs =
                               bindingsOfNonemptyProds
                               (olds, news, nonemptyProds)
                         val alls  =
                               Tab.fromList
                               compareParsable
                               (Set.toList
                                (Set.union compareBinding (olds, news)))
                     in iterate
                        (reconcileOldAndNewTabs
                         (alls, bestBindingsToTab bndgs))
                     end

          val inits : tab =
                Tab.fromList
                compareParsable
                (Set.mapToList
                 (fn b => consBindings(b, SOME nil))
                 syms @
                 Set.mapToList
                 (fn (b, _) => consBindings(b, NONE))
                 emptyProds)
          val finals      = iterate(Tab.empty, inits)
      in case lookupParsable(finals, (a, w)) of
              NONE        => NONE
            | SOME(pt, _) => SOME pt
      end

fun parseOpt gram (a, w) =
      let val vars  = variables gram
          val prods = productions gram
          val alpha = alphabet gram
          val syms  = Str.alphabet w
      in if SymSet.subset(syms, SymSet.union(vars, alpha))
         then if SymSet.memb(a, vars) orelse SymSet.memb(a, syms)
              then parseOptCore(gram, a, w, vars, prods, alpha, syms)
              else M.errorString
                   (fn () =>
                         ["symbol", "is", "neither", "variable", "nor",
                          "in", "string"])
         else M.errorString
              (fn () =>
                    ["string", "has", "symbol", "neither", "variable",
                     "nor", "in", "grammar's", "alphabet"])
      end

fun parse gram (a, w) =
      case parseOpt gram (a, w) of
           NONE    =>
             M.errorString
             (fn () => ["no", "such", "parse", "exists"])
         | SOME pt => pt

fun parseAlphabetFromVariableOpt gram (a, w) =
      if SymSet.memb(a, variables gram)
      then if SymSet.subset(Str.alphabet w, alphabet gram)
           then parseOpt gram (a, w)
           else M.errorString
                (fn () =>
                      ["string", "has", "symbol", "not", "in", "grammar's",
                       "alphabet"])
      else M.errorString(fn () => ["symbol", "isn't", "variable"])

fun parseAlphabetFromVariable gram (a, w) =
      if SymSet.memb(a, variables gram)
      then if SymSet.subset(Str.alphabet w, alphabet gram)
           then parse gram (a, w)
           else M.errorString
                (fn () =>
                      ["string", "has", "symbol", "not", "in", "grammar's",
                       "alphabet"])
      else M.errorString(fn () => ["symbol", "isn't", "variable"])

fun parseAlphabetOpt gram w =
      parseAlphabetFromVariableOpt gram (startVariable gram, w)

fun parseAlphabet gram w =
      parseAlphabetFromVariable gram (startVariable gram, w)

fun reachableFrom gram =
      let val vars  = variables gram
          val prods = productions gram

          fun newVariables(nil,             _) = nil
            | newVariables((q, x) :: prods, p) =
                (case Sym.compare(p, q) of
                      LESS    => nil
                    | EQUAL   =>
                        List.filter (fn a => SymSet.memb(a, vars)) x @
                        newVariables(prods, p)
                    | GREATER => newVariables(prods, p))

          fun closure(nil,         olds) = olds
            | closure(new :: news, olds) =
                if SymSet.memb(new, olds)
                then closure(news, olds)
                else closure(newVariables(Set.toList prods, new) @ news,
                             SymSet.union(Set.sing new, olds))

          fun reachFrom qs =
                if Set.exists (fn q => not(SymSet.memb(q, vars))) qs
                then M.errorString
                     (fn () =>
                           ["set", "of", "symbols", "contains", "non",
                            "variable"])
                else closure(Set.toList qs, Set.empty)
      in reachFrom end

fun reachableFromBackwards gram =
      let val vars  = variables gram
          val prods = productions gram
          val alpha = alphabet gram

          fun newVariables(nil,             _)    = Set.empty
            | newVariables((q, x) :: prods, olds) =
                if List.all
                   (fn a =>
                         SymSet.memb(a, olds) orelse
                         SymSet.memb(a, alpha))
                   x
                then SymSet.union(Set.sing q, newVariables(prods, olds))
                else newVariables(prods, olds)

          fun closure olds =
                let val news = newVariables(Set.toList prods, olds)
                in if SymSet.subset(news, olds)
                   then olds
                   else closure(SymSet.union(news, olds))
                end

          fun reachFromBackwards qs =
                if Set.exists (fn q => not(SymSet.memb(q, vars))) qs
                then M.errorString
                     (fn () =>
                           ["set", "of", "symbols", "contains", "non",
                            "variable"])
                else closure qs
      in reachFromBackwards end

fun reachify gram =
      let val start = startVariable gram
          val prods = productions gram

          val vars'  = reachableFrom gram (Set.sing start)
          val start' = start
          val prods' =
                Set.filter (fn (q, _) => SymSet.memb(q, vars')) prods
      in {vars  = vars',
          start = start',
          prods = prods'}
      end

fun reachified gram = equal(reachify gram, gram)

fun simplify gram =
      let val start = startVariable gram
          val alpha = alphabet gram
          val prods = productions gram

          val vars'    = reachableFromBackwards gram (Set.empty)
          val symbols' = SymSet.union(alpha, vars')
          val prods'   =
                Set.filter (fn (_, x) =>
                                 List.all (fn a => SymSet.memb(a, symbols'))
                                          x)
                           prods
      in if SymSet.memb(start, vars')
         then let val gram''  =
                        reachify
                        {vars  = vars',
                         start = start,
                         prods = prods'}
                  val vars''  = variables gram''
                  val start'' = start
                  val prods'' = productions gram''

                  fun implicit((p, x), gram) = parsable gram (p, x)

                  (* in a call remRedun(prods1, prods2), the grammar
                     built from the union of prods1 and prods2 has the
                     same implicit productions as that built from
                     prods''; but none of the elements of prods1 are
                     redundant in the grammar built from prods1 *)

                  fun remRedun(prods, nil)            = prods
                    | remRedun(prods, prod :: prods') =
                        if implicit
                           (prod,
                            {vars  = vars'',
                             start = start'',
                             prods =
                               ProdSet.union(prods, ProdSet.fromList prods')})
                        then remRedun(prods, prods')
                        else remRedun
                             (ProdSet.union(prods, Set.sing prod), prods')

                  fun altCompare(prod as (_, x), prod' as (_, y)) =
                        case Int.compare(length x, length y) of
                             LESS    => LESS
                           | EQUAL   => Prod.compare(prod, prod')
                           | GREATER => GREATER

                  (* favor keeping productions with shorter right-hand
                     sides; next, favor earlier productions *)

                  fun removeRedundant prods =
                        remRedun
                        (Set.empty,
                         rev(Sort.sort (false, altCompare) (Set.toList prods)))
              in {vars  = vars'',
                  start = start'',
                  prods = removeRedundant prods''}
              end
         else {vars  = Set.sing start,
               start = start,
               prods = Set.empty}
      end

fun simplified gram = equal(simplify gram, gram)

fun nullableVariables gram =
      let val prods = productions gram

          fun newVariables(nil,             _)    = Set.empty
            | newVariables((q, x) :: prods, olds) =
                if not(SymSet.memb(q, olds)) andalso
                   List.all (fn a => SymSet.memb(a, olds)) x
                then SymSet.union(Set.sing q, newVariables(prods, olds))
                else newVariables(prods, olds)

          fun closure olds =
                let val news = newVariables(Set.toList prods, olds)
                in if Set.isEmpty news
                   then olds
                   else closure(SymSet.union(news, olds))
                end
      in closure Set.empty end

fun hasNoEmptyProductions(gram : gram) =
      not(Set.exists
          (fn (_, nil) => true
            | (_, _)   => false)
          (#prods gram))

fun eliminateEmptyProductions gram =
      let val vars     = variables gram
          val start    = startVariable gram
          val prods    = productions gram
          val nullable = nullableVariables gram

          fun newRightSides nil       = Set.sing nil
            | newRightSides (b :: bs) =
                let val xs = newRightSides bs
                    val ys = StrSet.map (fn cs => b :: cs) xs
                in if SymSet.memb(b, nullable)
                   then StrSet.union(xs, ys)
                   else ys
                end

          fun newProductions(q, x) =
                ProdSet.map (fn y => (q, y)) (newRightSides x)

          val prods' =
                Set.filter (fn (_, x) => not(List.null x))
                           (ProdSet.genUnion(Set.mapToList newProductions
                                                           prods))
          val gram' =
                {vars  = vars,
                 start = start,
                 prods = prods'}
      in simplify gram' end

fun hasNoEmptyOrUnitProductions(gram as {vars, prods, ...} : gram) =
      hasNoEmptyProductions gram andalso
      not(Set.exists
          (fn (_, [q]) => SymSet.memb(q, vars)
            | (_, _)   => false)
          (#prods gram))

fun eliminateEmptyAndUnitProductions gram =
      let val gram  = eliminateEmptyProductions gram
          val vars  = variables gram
          val start = startVariable gram
          val prods = productions gram

          fun unitProd (_, [a]) = SymSet.memb(a, vars)
            | unitProd _        = false

          val unitProds    = Set.filter unitProd prods
          val nonUnitProds = Set.filter (not o unitProd) prods

          fun newProductions (nil,               _)      = nil
            | newProductions ((q, [r]) :: units, (p, x)) =
                if Sym.equal(r, p)
                then (q, x) :: newProductions(units, (p, x))
                else newProductions(units, (p, x))
            | newProductions _                           = M.cannotHappen()

          fun closure(nil,         olds) = olds
            | closure(new :: news, olds) =
                if ProdSet.memb(new, olds)
                then closure(news, olds)
                else closure(newProductions(Set.toList unitProds, new) @ news,
                             ProdSet.union(Set.sing new, olds))

          val prods' = closure(Set.toList nonUnitProds, Set.empty)
          val gram'  =
                {vars  = vars,
                 start = start,
                 prods = prods'}
      in simplify gram' end

fun inChomskyNormalForm(gram as {vars, prods, ...} : gram) =
      Set.all
      (fn (_, [a])    => not(SymSet.memb(a, vars))
        | (_, [a, b]) => SymSet.memb(a, vars) andalso SymSet.memb(b, vars)
        | _           => false)
      (#prods gram)

fun chomskyNormalForm gram =
      let val gram  = eliminateEmptyAndUnitProductions gram
          val vars  = variables gram
          val start = startVariable gram
          val prods = productions gram        
          val alpha = alphabet gram

          fun newVarFromOldVar q =
                Sym.fromTop
                (Sym.Compound[SOME(Sym.fromString "1"), NONE, SOME q])

          fun newVarFromAlphabetSym a =
                Sym.fromTop
                (Sym.Compound[SOME(Sym.fromString "2"), NONE, SOME a])

          fun newVarFromOldRightSideSuff x =
                Sym.fromTop
                (Sym.Compound([SOME(Sym.fromString "3"), NONE] @ map SOME x))

          fun newVarsFromNewVarPlusOldRightSideSuff(q, [_])     =
                Set.sing q
            | newVarsFromNewVarPlusOldRightSideSuff(q, [a, b])  =
                SymSet.fromList[q,
                                if SymSet.memb(a, vars)
                                then newVarFromOldVar a
                                else newVarFromAlphabetSym a,
                                if SymSet.memb(b, vars)
                                then newVarFromOldVar b
                                else newVarFromAlphabetSym b]
            | newVarsFromNewVarPlusOldRightSideSuff(q, b :: bs) =
                SymSet.union
                (SymSet.fromList[q,
                                 if SymSet.memb(b, vars)
                                 then newVarFromOldVar b
                                 else newVarFromAlphabetSym b],
                 newVarsFromNewVarPlusOldRightSideSuff
                 (newVarFromOldRightSideSuff bs, bs))
            |  newVarsFromNewVarPlusOldRightSideSuff _          =
                 M.cannotHappen()

          fun newVarsFromOldProd(q, x) =
                newVarsFromNewVarPlusOldRightSideSuff(newVarFromOldVar q, x)

          val vars' =
                SymSet.union(Set.sing(newVarFromOldVar start),
                             SymSet.genUnion(Set.mapToList newVarsFromOldProd
                                                           prods))

          val rel   = SymRel.makeBijectionFromAvoiding(vars', alpha)
          val renam = SymRel.applyFunction rel

          val newVarFromOldVar      = renam o newVarFromOldVar
          val newVarFromAlphabetSym = renam o newVarFromAlphabetSym
          val newVarFromRightSide   = renam o newVarFromOldRightSideSuff

          val vars''  = SymSet.map renam vars'
          val start'' = newVarFromOldVar start

          fun newProdsFromNewVarPlusOldRightSideSuff (q, [a])     =
                Set.sing(q, [a])
            | newProdsFromNewVarPlusOldRightSideSuff (q, [a, b])  =
                if SymSet.memb(a, vars)
                then if SymSet.memb(b, vars)
                     then let val p = newVarFromOldVar a
                              val r = newVarFromOldVar b
                          in Set.sing(q, [p, r]) end
                     else let val p     = newVarFromOldVar a
                              val r     = newVarFromAlphabetSym b
                              val prods = [(q, [p, r]), (r, [b])]
                          in ProdSet.fromList prods end
                else if SymSet.memb(b, vars)
                     then let val p     = newVarFromAlphabetSym a
                              val r     = newVarFromOldVar b
                              val prods = [(q, [p, r]), (p, [a])]
                          in ProdSet.fromList prods end
                     else let val p     = newVarFromAlphabetSym a
                              val r     = newVarFromAlphabetSym b
                              val prods = [(q, [p, r]), (p, [a]), (r, [b])]
                          in ProdSet.fromList prods end
            | newProdsFromNewVarPlusOldRightSideSuff (q, b :: bs) =
                if SymSet.memb(b, vars)
                then let val p = newVarFromOldVar b
                         val r = newVarFromOldRightSideSuff bs
                     in ProdSet.union
                        (Set.sing(q, [p, r]),
                         newProdsFromNewVarPlusOldRightSideSuff(r, bs))
                     end
                else let val p     = newVarFromAlphabetSym b
                         val r     = newVarFromOldRightSideSuff bs
                         val prods = [(q, [p, r]), (p, [b])]
                     in ProdSet.union
                        (ProdSet.fromList prods,
                         newProdsFromNewVarPlusOldRightSideSuff(r, bs))
                     end
            |  newProdsFromNewVarPlusOldRightSideSuff _           =
                 M.cannotHappen()

          fun newProdsFromOldProd(q, x) =
                newProdsFromNewVarPlusOldRightSideSuff(newVarFromOldVar q, x)

          val prods'' =
                ProdSet.genUnion(Set.mapToList newProdsFromOldProd prods)
      in {vars  = vars'',
          start = start'',
          prods = prods''}
      end

local
  fun prodsToFams prods =
        let fun group(p, xs, nil)             = [(p, rev xs)]
              | group(p, xs, (q, y) :: prods) =
                  if Sym.equal(q, p)
                  then group(p, y :: xs, prods)
                  else (p, rev xs) :: group(q, [y], prods)
        in case Set.toList prods of
                nil             => nil
              | (q, x) :: prods => group(q, [x], prods)
        end

  fun defined(_, nil)            = false
    | defined(a, (q, _) :: fams) = Sym.equal(a, q) orelse defined(a, fams)

  fun stratify(alpha, prods) =
        let fun strat(fams', nil)  = SOME fams'
              | strat(fams', fams) =
                  case List.partition
                       (fn (_, xs) =>
                             List.all
                             (fn x =>
                                   List.all
                                   (fn a =>
                                         SymSet.memb(a, alpha) orelse
                                         defined(a, fams'))
                                   x)
                             xs)
                       fams of
                       (nil,   _)     => NONE
                     | (fams1, fams2) => strat(fams1 @ fams', fams2)
        in strat(nil, prodsToFams prods) end
in
  fun toStrSetOpt gram =
        let val includesEmpty = generated gram nil
            val gram          = eliminateEmptyAndUnitProductions gram
            val alpha         = alphabet gram
            val start         = startVariable gram
            val prods         = productions gram

            fun rhsToSS(_,   nil)     = Set.sing[]
              | rhsToSS(tab, b :: bs) =
                  StrSet.concat
                  (if SymSet.memb(b, alpha)
                   then Set.sing[b]
                   else valOf(Tab.lookup Sym.compare (tab, b)),
                   rhsToSS(tab, bs))

            fun rhssToSS(_,   nil)         = Set.empty
              | rhssToSS(tab, rhs :: rhss) =
                  StrSet.union(rhsToSS(tab, rhs), rhssToSS(tab, rhss))

            fun stratToSS nil                 = Tab.empty
              | stratToSS ((q, rhss) :: fams) =
                  let val tab = stratToSS fams
                  in Tab.update
                     Sym.compare
                     (tab, [(q, rhssToSS(tab, rhss))])
                  end

        in if Set.size prods = 0
           then SOME(Set.empty)
           else case stratify(alpha, prods) of
                     NONE      => NONE
                   | SOME fams =>
                       SOME
                       (StrSet.union
                        (if includesEmpty then Set.sing nil else Set.empty,
                         valOf(Tab.lookup Sym.compare (stratToSS fams, start))))
        end
end

fun toStrSet gram =
      case toStrSetOpt gram of
           NONE    => M.errorString(fn () => ["language", "is", "infinite"])
         | SOME xs => xs

val emptyStr : gram =
      {vars  = Set.sing(Sym.fromString "A"),
       start = Sym.fromString "A",
       prods = Set.sing(Sym.fromString "A", nil)}

val emptySet : gram =
      {vars  = Set.sing(Sym.fromString "A"),
       start = Sym.fromString "A",
       prods = Set.empty}

fun fromStr x =
      let val alpha     = SymSet.fromList x
          val startPref = Sym.fromString "A"
          val rel       =
                SymRel.makeBijectionFromAvoiding(Set.sing startPref, alpha)
          val start = SymRel.applyFunction rel startPref
      in {vars  = Set.sing start,
          start = start,
          prods = Set.sing(start, x)}
      end

fun fromSym a = fromStr[a]

fun fromStrSet xs =
      let val alpha         = StrSet.alphabet xs
          val startPref     = Sym.fromString "A"
          val otherVarsPref =
                SymSet.mapFromList
                (fn n => Sym.fromString("<B" ^ Int.toString n ^ ">"))
                (ListAux.fromTo(1, Set.size xs))
          val varsPref      = SymSet.union(Set.sing startPref, otherVarsPref)
          val rel           = SymRel.makeBijectionFromAvoiding(varsPref, alpha)
          val renam         = SymRel.applyFunction rel
          val vars          = SymSet.map renam varsPref
          val start         = renam startPref
          val startProds    = ProdSet.map (fn a => (start, [a])) otherVarsPref

          fun otherProductions (nil,     nil)     = Set.empty
            | otherProductions (b :: bs, x :: xs) =
                ProdSet.union(Set.sing(b, x), otherProductions(bs, xs))
            | otherProductions _                  = M.cannotHappen()

          val otherProds =
                otherProductions(Set.toList otherVarsPref, Set.toList xs)
          val prods      = ProdSet.union(startProds, otherProds)
      in {vars  = vars,
          start = start,
          prods = prods}
      end

fun union(gram1, gram2) =
      let val vars1     = variables gram1
          val vars2     = variables gram2
          val alpha     = SymSet.union(alphabet gram1, alphabet gram2)
          val startPref = Sym.fromString "A"

          fun renam1Pref a =
                Sym.fromTop
                (Sym.Compound[SOME(Sym.fromString "1"), NONE, SOME a])

          fun renam2Pref a =
                Sym.fromTop
                (Sym.Compound[SOME(Sym.fromString "2"), NONE, SOME a])

          val rel1Pref = SymRel.mlFunctionToFunction(renam1Pref, vars1)
          val rel2Pref = SymRel.mlFunctionToFunction(renam2Pref, vars2)

          val vars1Pref = SymSet.map renam1Pref vars1
          val vars2Pref = SymSet.map renam2Pref vars2
          val varsPref  =
                SymSet.genUnion[Set.sing startPref, vars1Pref, vars2Pref]

          val rel  = SymRel.makeBijectionFromAvoiding(varsPref, alpha)
          val rel1 = SymRel.compose(rel, rel1Pref)
          val rel2 = SymRel.compose(rel, rel2Pref)

          val gram1  = renameVariables(gram1, rel1)
          val vars1  = variables gram1
          val start1 = startVariable gram1
          val prods1 = productions gram1

          val gram2  = renameVariables(gram2, rel2)
          val vars2  = variables gram2
          val start2 = startVariable gram2
          val prods2 = productions gram2

          val start = SymRel.applyFunction rel startPref
          val vars  =
                SymSet.union(Set.sing start, SymSet.union(vars1, vars2))
          val prods =
                ProdSet.union(ProdSet.fromList
                              [(start, [start1]),
                               (start, [start2])],
                              ProdSet.union(prods1, prods2))
      in {vars  = vars,
          start = start,
          prods = prods}
      end

fun concat(gram1, gram2) =
      let val vars1     = variables gram1
          val vars2     = variables gram2
          val alpha     = SymSet.union(alphabet gram1, alphabet gram2)
          val startPref = Sym.fromString "A"

          fun renam1Pref a =
                Sym.fromTop
                (Sym.Compound[SOME(Sym.fromString "1"), NONE, SOME a])

          fun renam2Pref a =
                Sym.fromTop
                (Sym.Compound[SOME(Sym.fromString "2"), NONE, SOME a])

          val rel1Pref = SymRel.mlFunctionToFunction(renam1Pref, vars1)
          val rel2Pref = SymRel.mlFunctionToFunction(renam2Pref, vars2)

          val vars1Pref = SymSet.map renam1Pref vars1
          val vars2Pref = SymSet.map renam2Pref vars2
          val varsPref  =
                SymSet.genUnion[Set.sing startPref, vars1Pref, vars2Pref]

          val rel  = SymRel.makeBijectionFromAvoiding(varsPref, alpha)
          val rel1 = SymRel.compose(rel, rel1Pref)
          val rel2 = SymRel.compose(rel, rel2Pref)

          val gram1  = renameVariables(gram1, rel1)
          val vars1  = variables gram1
          val start1 = startVariable gram1
          val prods1 = productions gram1

          val gram2  = renameVariables(gram2, rel2)
          val vars2  = variables gram2
          val start2 = startVariable gram2
          val prods2 = productions gram2

          val start = SymRel.applyFunction rel startPref
          val vars  =
                SymSet.union(Set.sing start, SymSet.union(vars1, vars2))
          val prods =
                ProdSet.union(ProdSet.fromList[(start, [start1, start2])],
                              ProdSet.union(prods1, prods2))
      in {vars  = vars,
          start = start,
          prods = prods}
      end

fun closure gram =
      let val vars       = variables gram
          val alpha      = alphabet gram
          val start'Pref = Sym.fromString "A"

          fun renamPref a = Sym.fromTop(Sym.Compound[SOME a])

          val relPref = SymRel.mlFunctionToFunction(renamPref, vars)

          val varsPref  = SymSet.map renamPref vars
          val vars'Pref = SymSet.union(Set.sing start'Pref, varsPref)

          val rel' = SymRel.makeBijectionFromAvoiding(vars'Pref, alpha)
          val rel  = SymRel.compose(rel', relPref)

          val gram  = renameVariables(gram, rel)
          val vars  = variables gram
          val start = startVariable gram
          val prods = productions gram

          val start' = SymRel.applyFunction rel' start'Pref
          val vars'  = SymSet.union(Set.sing start', vars)
          val prods' =
                ProdSet.union(ProdSet.fromList
                              [(start', nil),
                               (start', [start, start'])],
                              prods)
      in {vars  = vars',
          start = start',
          prods = prods'}
      end

fun genUnion nil             = emptySet
  | genUnion [gram]          = gram
  | genUnion (gram :: grams) = union(gram, genUnion grams)

fun genConcat nil             = emptyStr
  | genConcat [gram]          = gram
  | genConcat (gram :: grams) = concat(gram, genConcat grams)

fun fromFA fa =
      let val states = FA.states fa
          val alpha  = FA.alphabet fa

          val rel = SymRel.makeBijectionFromAvoiding(states, alpha)
          val fa  = FA.renameStates(fa, rel)

          val vars  = FA.states fa
          val start = FA.startState fa
          val prods =
                ProdSet.union(ProdSet.map (fn (q, x, r) => (q, x @ [r]))
                                          (FA.transitions fa),
                              ProdSet.map (fn q => (q, nil))
                                          (FA.acceptingStates fa))
      in {vars  = vars,
          start = start,
          prods = prods}
      end

fun fromConcrReg Reg.EmptyStr                    = emptyStr
  | fromConcrReg Reg.EmptySet                    = emptySet
  | fromConcrReg (Reg.Sym a)                     = fromStr[a]
  | fromConcrReg (Reg.Closure reg)               = closure(fromConcrReg reg)
  | fromConcrReg (reg as Reg.Concat(reg1, reg2)) =
      (case Reg.split(Reg.fromConcr reg) of
            (bs,  NONE)      => fromStr bs
          | (nil, SOME _)    => concat(fromConcrReg reg1, fromConcrReg reg2)
          | (bs,  SOME reg') =>
              concat(fromStr bs, fromConcrReg(Reg.toConcr reg')))
  | fromConcrReg (Reg.Union(reg1, reg2))         =
      union(fromConcrReg reg1, fromConcrReg reg2)

fun fromReg reg = fromConcrReg(Reg.toConcr reg)

fun rev gram =
      let val vars   = variables gram
          val start  = startVariable gram
          val prods  = productions gram
          val prods' = ProdSet.map (fn (a, x) => (a, List.rev x)) prods
      in {vars  = vars,
          start = start,
          prods = prods'}
      end

fun prefix gram =
      let val gram  = simplify gram
          val vars  = variables gram
          val start = startVariable gram
          val prods = productions gram        
          val alpha = alphabet gram
          val syms  = SymSet.union(vars, alpha)

          fun fullVarFromOldVarPref a =
                Sym.fromTop
                (Sym.Compound[SOME(Sym.fromString "1"), NONE, SOME a])

          fun prefixVarFromOldVarOrAlpSymPref a =
                Sym.fromTop
                (Sym.Compound[SOME(Sym.fromString "2"), NONE, SOME a])

          val vars'Pref =
                SymSet.union
                (SymSet.map fullVarFromOldVarPref vars,
                 SymSet.map prefixVarFromOldVarOrAlpSymPref syms)

          val relRenam = SymRel.makeBijectionFromAvoiding(vars'Pref, alpha)
          val renam    = SymRel.applyFunction relRenam

          val fullVarFromOldVar           = renam o fullVarFromOldVarPref
          val prefixVarFromOldVarOrAlpSym =
                renam o prefixVarFromOldVarOrAlpSymPref

          val start' = prefixVarFromOldVarOrAlpSym start
          val vars'  =
                SymSet.union(SymSet.map fullVarFromOldVar vars,
                             SymSet.map prefixVarFromOldVarOrAlpSym syms)

          val alphaProds =
                ProdSet.genUnion
                (map
                 (fn a =>
                       (ProdSet.fromList
                        [(prefixVarFromOldVarOrAlpSym a, nil),
                         (prefixVarFromOldVarOrAlpSym a, [a])]))
                 (Set.toList alpha))

          fun convertOldVarOrAlpSym a =
                if SymSet.memb(a, alpha)
                then a
                else fullVarFromOldVar a

          fun convertOldProd(a, x) =
                (fullVarFromOldVar a, map convertOldVarOrAlpSym x)

          fun newProdFromPrefix(a, xs, b) =
                (prefixVarFromOldVarOrAlpSym a,
                 map convertOldVarOrAlpSym xs @ [prefixVarFromOldVarOrAlpSym b])

          fun oldProdToNewProds(a, nil) =
                (ProdSet.fromList
                 [(fullVarFromOldVar a, nil),
                  (prefixVarFromOldVarOrAlpSym a, nil)])
            | oldProdToNewProds(a, x)   =
                let val nonEmpPrefixes = tl(map #1 (ListAux.allSplittings x))
                in ProdSet.union
                   (Set.sing(convertOldProd(a, x)),
                    ProdSet.mapFromList newProdFromPrefix
                                        (map (fn x =>
                                                   (a,
                                                    ListAux.allButLast x,
                                                    List.last x))
                                             nonEmpPrefixes))
                end

          val prods' =
                ProdSet.union(alphaProds,
                              ProdSet.genUnion(Set.mapToList oldProdToNewProds
                                                             prods))
      in {vars  = vars',   (* will be simplified unless gram generated *)
          start = start',  (* nothing *)
          prods = prods'}
      end

fun inter(gram, efa) =
      let val vars     = variables gram
          val startVar = startVariable gram
          val prods    = productions gram
          val alpha    = alphabet gram

          val stats      = EFA.states efa
          val startStat  = EFA.startState efa
          val accepting  = EFA.acceptingStates efa
          val procStr    = EFA.processStr efa
          val statePairs = Set.times(stats, stats)

          fun tripleToVarPref(p, q, r) =          
                Sym.fromTop(Sym.Compound[SOME p, NONE, SOME q, NONE, SOME r])

          val triples     = Set.times3(vars, stats, stats)
          val vars'Pref   =
                SymSet.union
                (Set.sing(Sym.fromString "A"),
                 SymSet.map tripleToVarPref triples)
          val renamRel    = SymRel.makeBijectionFromAvoiding(vars'Pref, alpha)
          val renam       = SymRel.applyFunction renamRel
          val vars'       = SymSet.map renam vars'Pref
          val startVar'   = renam(Sym.fromString "A")
          val tripleToVar = renam o tripleToVarPref

          fun startProds nil       = Set.empty
            | startProds (q :: qs) =
                Set.union Prod.compare
                (Set.sing(startVar', [tripleToVar(startVar, startStat, q)]),
                 startProds qs)

          fun prodsOfOldProd nil                 = Set.empty
            | prodsOfOldProd ((p, nil) :: prods) =
                let fun emptyProds nil               = Set.empty
                      | emptyProds ((q, r) :: pairs) =
                          if SymSet.memb(r, procStr(Set.sing q, nil))
                          then Set.union Prod.compare
                               (Set.sing(tripleToVar(p, q, r), nil),
                                emptyProds pairs)
                          else emptyProds pairs
                in Set.union Prod.compare
                   (emptyProds(Set.toList statePairs), prodsOfOldProd prods)
                end
            | prodsOfOldProd ((p, bs) :: prods)  =
                let val traces =
                          Set.genTimes(ListAux.repeat(stats, length bs + 1))

                    fun rhs ([p, q],               [b])      =
                         if SymSet.memb(b, alpha)
                         then if SymSet.memb(q, procStr(Set.sing p, [b]))
                              then SOME[b]
                              else NONE
                         else SOME[tripleToVar(b, p, q)]
                     | rhs (p :: (q_rs as q :: rs), b :: bs) =
                         (case rhs(q_rs, bs) of
                               NONE    => NONE
                             | SOME cs =>
                                 if SymSet.memb(b, alpha)
                                 then if SymSet.memb
                                         (q, procStr(Set.sing p, [b]))
                                      then SOME(b :: cs)
                                      else NONE
                                 else SOME(tripleToVar(b, p, q) :: cs))
                     | rhs _                                 =
                         M.cannotHappen()

                    fun prodsOfTraces nil            = Set.empty
                      | prodsOfTraces (tr :: traces) =
                          let val q = hd tr
                              val r = List.last tr
                          in case rhs(tr, bs) of
                                  NONE    => prodsOfTraces traces
                                | SOME cs =>
                                    Set.union Prod.compare
                                    (Set.sing
                                     (tripleToVar(p, q, r), cs),
                                     prodsOfTraces traces)
                          end
                in Set.union Prod.compare
                   (prodsOfTraces(Set.toList traces), prodsOfOldProd prods)
                end

           val prods' =
                 Set.union Prod.compare
                 (startProds(Set.toList accepting),
                  prodsOfOldProd(Set.toList prods))

           val gram' =
                {vars  = vars',
                 start = startVar',
                 prods = prods'}
      in simplify gram' end

fun minus(gram, dfa) =
      inter(gram, DFA.injToEFA(DFA.complement(dfa, alphabet gram)))

end;
