(*********************************** pt.sml **********************************)

(* Copyright (C) 2012 Alley Stoughton

   The file is part of the Forlan toolset for experimenting with
   formal languages.  See the file COPYING.txt for copying and
   usage restrictions. *)

structure PT :> PT =
struct

structure M  = Messages
structure L  = Lex
structure LA = ListAux

(******************************** Main Types ********************************)

(* Node(a, NONE) represents a(%); Node(a, SOME pts) represents
   a(pt's), where pt's are the parse trees represented by pts *)

datatype concr = Node of Sym.sym * concr list option

type pt = concr

fun toConcr(pt : pt) : concr = pt

fun fromConcr(concr : concr) : pt = concr

(*********************************** Input ***********************************)

fun inpPT lts =
      let val (a, lts) = Sym.inputFromLabToks lts
      in case lts of
              (_, L.OpenPar) :: (_, L.Perc) :: lts =>
                let val lts = L.checkInLabToks(L.ClosPar, lts)
                in (Node(a, NONE), lts) end
            | (_, L.OpenPar) :: lts                =>
                let val (pts, lts) = inpNEPTList lts
                    val lts        = L.checkInLabToks(L.ClosPar, lts)
                in (Node(a, SOME pts), lts) end
            | _                                    => (Node(a, SOME nil), lts)
      end

and inpNEPTList lts =
      let val (pt, lts) = inpPT lts
      in case lts of
              (_, L.Comma) :: lts =>
                let val (pts, lts) = inpNEPTList lts
                in (pt :: pts, lts) end
            | _                   => ([pt], lts)
      end

fun fromString s =
      case inpPT(L.lexString s) of
           (pt, [(_, L.EOF)]) => pt
         | (_,  nil)          => M.cannotHappen() 
         | (_,  lt :: _)      => L.unexpectedTok lt

fun input fil =
      case inpPT(L.lexFile fil) of
           (pt, [(_, L.EOF)]) => pt
         | (_,  nil)          => M.cannotHappen() 
         | (_,  lt :: _)      => L.unexpectedTok lt

(********************************** Output **********************************)

fun toPP(Node(a, SOME nil)) = Sym.toPP a
  | toPP(Node(a, SOME pts)) =
      PP.block(false,
               [Sym.toPP a,
                PP.decorate("(",
                            PP.block(true, nePTListToPPList pts),
                            ")")])
  | toPP(Node(a, NONE))      =
      PP.block(false, [Sym.toPP a, PP.fromString "(%)"])

and nePTListToPPList nil         = M.cannotHappen()
  | nePTListToPPList [pt]        = [toPP pt]
  | nePTListToPPList (pt :: pts) = PP.comma(toPP pt) :: nePTListToPPList pts

fun toString pt = PP.toString(toPP pt)

fun output("",  pt) = (print(toString pt); print PP.newline)
  | output(fil, pt) =
      case SOME(TextIO.openOut fil) handle _ => NONE of
           NONE     =>
             M.errorPP
             (fn () =>
                   [PP.fromString "unable", PP.fromString "to",
                    PP.fromString "open", PP.fromString "file:",
                    PP.quote(PP.fromStringSplitEscape fil)])
         | SOME stm =>
             (TextIO.output(stm, toString pt);
              TextIO.output(stm, PP.newline);
              TextIO.closeOut stm)

(******************************* Tree Functions *******************************)

fun validPath (_,                 nil)     = true
  | validPath (Node(_, SOME pts), n :: ns) =
      (case SOME(ListAux.sub(pts, n))
              handle _ => NONE of
            NONE    => false
          | SOME pt => validPath(pt, ns))
  | validPath (Node(_, NONE),     [1])     = true
  | validPath _                            = false

fun height(Node(_, SOME nil)) = 0
  | height(Node(_, SOME pts)) = 1 + ListAux.max(map height pts)
  | height(Node(_, NONE))     = 1

fun size(Node(_, SOME pts)) = 1 + ListAux.sum(map size pts)
  | size(Node(_, NONE))     = 2

fun numLeaves(Node(_, SOME nil)) = 1
  | numLeaves(Node(_, SOME pts)) = ListAux.sum(map numLeaves pts)
  | numLeaves(Node(_, NONE))     = 1

fun selectPT (pt,                nil)     = SOME pt
  | selectPT (Node(_, SOME pts), n :: ns) =
      (case SOME(ListAux.sub(pts, n))
              handle _ => NONE of
            NONE    =>
              M.errorString
              (fn () => ["invalid", "path", "for", "parse", "tree"])
          | SOME pt => selectPT(pt, ns))
  | selectPT (Node(_, NONE),     [1])     = NONE
  | selectPT _                            =
      M.errorString
      (fn () => ["invalid", "path", "for", "parse", "tree"])

fun update (pt,                nil,     pt') = pt'
  | update (Node(a, SOME pts), n :: ns, pt') =
      (case SOME(ListAux.sub(pts, n))
              handle _ => NONE of
            NONE    =>
              M.errorString
              (fn () => ["invalid", "path", "for", "parse", "tree"])
          | SOME pt =>
              Node(a, SOME(ListAux.update(pts, n, update(pt, ns, pt')))))
  | update (Node(a, NONE),     [1],     pt') = Node(a, SOME[pt'])
  | update _                                 =
      M.errorString
      (fn () => ["invalid", "path", "for", "parse", "tree"])

fun maximumLengthPath(Node(_, SOME nil)) = nil
  | maximumLengthPath(Node(_, SOME pts)) =
      let val hs  = map height pts
          val max = ListAux.max hs
          val n   = valOf(ListAux.position (fn y => y = max) hs)
      in n :: maximumLengthPath(ListAux.sub(pts, n)) end
  | maximumLengthPath(Node(_, NONE))     = [1]

fun validLeafPath(pt, ns) =
      validPath(pt, ns) andalso
      case selectPT(pt, ns) of
           NONE                     => true
         | SOME (Node(_, SOME nil)) => true
         | SOME _                   => false

(****************************** Other Functions ******************************)

fun compare(Node(a1, SOME pt1s), Node(a2, SOME pt2s)) =
      (case Sym.compare(a1, a2) of
            LESS    => LESS
          | EQUAL   => Set.compareList compare (pt1s, pt2s)
          | GREATER => GREATER)
  | compare(Node(a1, NONE),      Node(a2, NONE))      = Sym.compare(a1, a2)
  | compare(Node(_, SOME _),     Node(_, NONE))       = LESS
  | compare(Node(_, NONE),       Node(_, SOME _))     = GREATER

fun equal ptPair = compare ptPair = EQUAL

val cons = Node

fun leaf a = Node(a, SOME nil)

fun decons(Node x) = x

fun rootLabel(Node(a, _)) = a

fun yield(Node(a, SOME nil)) = [a]
  | yield(Node(_, SOME pts)) = List.concat(map yield pts)
  | yield(Node(_, NONE))     = nil

type pumping_division = (pt * int list) * (pt * int list) * pt

(* auxiliary function used by pumping division functions; will only
   be called when bs has at least one occurrence of a *)

fun splitAtUniqueOcc(a, bs) =
      let fun split(cs, nil)     = M.cannotHappen()
            | split(cs, b :: bs) =
                if Sym.equal(b, a)
                then if Option.isSome
                        (ListAux.position (fn d => Sym.equal(d, a)) bs)
                     then NONE
                     else SOME(rev cs, bs)
                else split(b :: cs, bs)
      in split(nil, bs) end

fun checkPumpingDivision((pt1, path1), (pt2, path2), pt3) =
      if not(validLeafPath(pt1, path1))
        then M.errorString
             (fn () =>
                   ["first", "path", "doesn't", "select", "leaf",
                    "of", "first", "tree"])
      else if not(validLeafPath(pt2, path2))
        then M.errorString
             (fn () =>
                   ["second", "path", "doesn't", "select", "leaf",
                    "of", "second", "tree"])
      else let val pt1LeafLabOpt = Option.map rootLabel (selectPT(pt1, path1))
               val pt2RootLab    = rootLabel pt2
               val pt2LeafLabOpt = Option.map rootLabel (selectPT(pt2, path2))
               val pt3RootLab    = rootLabel pt3
           in if not(isSome pt1LeafLabOpt)
                then M.errorString
                     (fn () =>
                           ["selected", "leaf", "of", "first",
                            "tree", "is", "labeled", "%"])
              else if not(isSome pt2LeafLabOpt)
                then M.errorString
                     (fn () =>
                           ["selected", "leaf", "of", "second",
                            "tree", "is", "labeled", "%"])
              else if not(Sym.equal(valOf pt1LeafLabOpt, pt2RootLab))
                then M.errorString
                     (fn () =>
                           ["label", "of", "selected", "leaf", "of",
                            "first", "tree", "is", "different", "from",
                            "root", "label", "of", "second", "tree"])
              else if not(Sym.equal(valOf pt2LeafLabOpt, pt2RootLab))
                then M.errorString
                     (fn () =>
                           ["label", "of", "selected", "leaf", "of",
                            "second", "tree", "different", "from",
                            "root", "label", "of", "second", "tree"])
              else if not(Sym.equal(pt3RootLab, pt2RootLab))
                then M.errorString
                     (fn () =>
                           ["root", "label", "of", "third", "tree",
                            "different", "from", "root", "label", "of",
                            "second", "tree"])
              else if length(yield pt2) = 1
                then M.errorString
                     (fn () =>
                           ["yield", "of", "second", "tree", "has",
                            "length", "1"])
              else if not
                      (Option.isSome
                       (splitAtUniqueOcc(rootLabel pt2, yield pt1)))
                then M.errorString
                     (fn () =>
                           ["yield", "of", "first", "tree", "has",
                            "more", "than", "one", "occurrence",
                            "of", "root", "label", "of", "second",
                            "tree"])
              else if not
                      (Option.isSome
                       (splitAtUniqueOcc(rootLabel pt2, yield pt2)))
                then M.errorString
                     (fn () =>
                           ["yield", "of", "second", "tree", "has",
                            "more", "than", "one", "occurrence",
                            "of", "root", "label", "of", "second",
                            "tree"])
              else if List.exists
                      (fn b => Sym.equal(b, rootLabel pt2))
                      (yield pt3)
                then M.errorString
                     (fn () =>
                           ["yield", "of", "third", "tree", "has",
                            "occurrence", "of", "root", "label", "of",
                            "second", "tree"])
              else ()
           end

fun validPumpingDivision pumpDiv =
      (M.quiet(fn () => checkPumpingDivision pumpDiv); true)
        handle _ => false

fun strsOfValidPumpingDivision(pumpDiv as ((pt1, path1), (pt2, path2), pt3)) =
      (checkPumpingDivision pumpDiv;
       let fun divideYield(pt, path) =
                 let val a = rootLabel(valOf(selectPT(pt, path)))
                 in valOf(splitAtUniqueOcc(a, yield pt)) end

           val (u, y) = divideYield(pt1, path1)
           val (v, x) = divideYield(pt2, path2)
           val w      = yield pt3
       in (u, v, w, x, y) end)

fun pumpValidPumpingDivision(pumpDiv as ((pt1, path1), (pt2, path2), pt3), n) =
      (checkPumpingDivision pumpDiv;
       if n < 0
       then M.errorString(fn () => ["negative", "argument"])
       else let fun pow 0 = pt3
                  | pow n = update(pt2, path2, pow(n - 1))
            in update(pt1, path1, pow n) end)

fun findValidPumpingDivision pt =
      let val lookupSym = Tab.lookup Sym.compare
          val updateSym = Tab.update Sym.compare
          val path      = maximumLengthPath pt
          val path      =
                case selectPT(pt, path) of
                     NONE =>
                       M.errorString
                       (fn () =>
                             ["leftmost", "maximum", "length", "path",
                              "points", "to", "%"])
                   | _    => path

          fun findFirstRepetition(path, tab) =
                let val a = rootLabel(valOf(selectPT(pt, path)))
                in case lookupSym(tab, a) of
                        NONE       =>
                          if null path
                          then M.errorString
                               (fn () =>
                                     ["no", "variable", "repetition", "on",
                                      "leftmost", "maximum", "length", "path"])
                          else findFirstRepetition
                               (LA.allButLast path,
                                updateSym(tab, [(a, path)]))
                      | SOME path' => (a, path, path')
                end

          val (q, path1, path') = findFirstRepetition(path, Tab.empty)
          val path2             = List.drop(path', length path1)
          val pt1               = update(pt, path1, leaf q)
          val pt'               = valOf(selectPT(pt, path1))
          val pt2               = update(pt', path2, leaf q)
          val pt3               = valOf(selectPT(pt', path2))
      in if length(yield pt2) < 2
           then M.errorString
                (fn () =>
                      ["in", "found", "pumping", "division,", "yield", "of",
                       "second", "tree", "has", "no", "symbols", "other",
                       "than", "repeated", "variable"])
         else if not(Option.isSome(splitAtUniqueOcc(q, yield pt1)))
           then M.errorString
                (fn () =>
                      ["in", "found", "pumping", "division,", "yield", "of",
                       "first", "tree", "has", "multiple", "occurrences",
                       "of", "repeated", "variable"])
         else if not(Option.isSome(splitAtUniqueOcc(q, yield pt2)))
           then M.errorString
                (fn () =>
                      ["in", "found", "pumping", "division,", "yield", "of",
                       "second", "tree", "has", "multiple", "occurrences",
                       "of", "repeated", "variable"])
         else if List.exists
                 (fn b => Sym.equal(b, rootLabel pt1))
                 (yield pt3)
           then M.errorString
                (fn () =>
                      ["in", "found", "pumping", "division,", "yield", "of",
                       "third", "tree", "has", "occurrence", "of",
                       "repeated", "variable"])
         else ((pt1, path1), (pt2, path2), pt3)
      end

fun findValidPumpingDivisionOpt pt =
      SOME(M.quiet(fn () => findValidPumpingDivision pt))
        handle _ => NONE

(*************************** Interface with JForlan ***************************)

fun jforlanNew() =
      let val file   = System.makeTempFile()
          val status = System.runJForlan("pt new " ^ file)
      in if OS.Process.isSuccess status
         then let val pt = input file
                  val _  = OS.FileSys.remove file
              in pt end
         else (OS.FileSys.remove file;
               M.errorString
               (fn () => ["creation", "of", "parse", "tree", "aborted"]))
      end

fun jforlanEdit pt =
      let val file   = System.makeTempFile()
          val _      = output(file, pt)
          val status = System.runJForlan("pt edit " ^ file)
      in if OS.Process.isSuccess status
         then let val pt = input file
                  val _  = OS.FileSys.remove file
              in pt end
         else (OS.FileSys.remove file;
               M.errorString
               (fn () => ["editing", "of", "parse", "tree", "aborted"]))
      end

(* if called with a string that isn't legal Forlan syntax for a parse
   tree, prints "error" on a line, followed by one or more lines
   of parsing errors

   if called with a string that is legal Forlan syntax for a parse
   tree, prints "valid" on a line, followed by a single line
   consisting of the parse tree, with no whitespace *)

fun jforlanValidate s =
      let val pt = Messages.quiet(fn () => fromString s)
      in print "valid"; print PP.newline;
         print(StringAux.removeWhiteSpace(toString pt));
         print PP.newline
      end
        handle _ =>
                 (print "error"; print PP.newline;
                  (fromString s; ())
                    handle _ => ())

(* when called with a parse tree in Forlan syntax, pretty
   prints on the standard output that parse tree *)

fun jforlanPretty s = (print(toString(fromString s)); print PP.newline)

end;
