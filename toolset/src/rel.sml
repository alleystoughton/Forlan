(********************************** rel.sml **********************************)

(* Copyright (C) 2001-2012 Alley Stoughton

   The file is part of the Forlan toolset for experimenting with
   formal languages.  See the file COPYING.txt for copying and
   usage restrictions. *)

structure Rel :> REL =
struct

structure M = Messages

type ('a, 'b)rel = ('a * 'b)Set.set

fun domain cmp rel = Set.map cmp (fn (a, b) => a) rel

fun range cmp rel = Set.map cmp (fn (a, b) => b) rel

fun relationFromTo (cmp, cmp') (rel, bs, cs) =
      Set.subset cmp (domain cmp rel, bs)  andalso
      Set.subset cmp' (range cmp' rel, cs)

fun relationOn cmp (rel, bs) = relationFromTo (cmp, cmp) (rel, bs, bs)

fun apply (cmp, cmp') (rel, cs) =
      let fun app nil            = Set.empty
            | app ((a, b) :: xs) =
                if Set.memb cmp (a, cs)
                then Set.union cmp' (Set.sing b, app xs)
                else app xs
      in app(Set.toList rel) end

fun reflexive cmp (rel, bs) =
      let val comparePair = Set.comparePair(cmp, cmp)
      in Set.all (fn b => Set.memb comparePair ((b, b), rel)) bs end

fun symmetric cmp rel =
      let val comparePair = Set.comparePair(cmp, cmp)
      in Set.all (fn (a, b) => Set.memb comparePair ((b, a), rel)) rel end

fun antisymmetric cmp rel =
      let val comparePair = Set.comparePair(cmp, cmp)
      in Set.all (fn (a, b) =>
                       Set.all (fn (c, d) =>
                                     cmp(b, c) <> EQUAL orelse
                                     cmp(a, d) <> EQUAL orelse
                                     cmp(a, b) = EQUAL)
                               rel)
                 rel
      end

fun transitive cmp rel =
      let val comparePair = Set.comparePair(cmp, cmp)
      in Set.all (fn (a, b) =>
                       Set.all (fn (c, d) =>
                                     cmp(b, c) <> EQUAL orelse
                                     Set.memb comparePair ((a, d), rel))
                               rel)
                 rel
      end

fun total cmp (rel, bs) =
      let val comparePair = Set.comparePair(cmp, cmp)
      in Set.all
         (fn b =>
               Set.all
               (fn c =>
                     Set.memb comparePair ((b, c), rel) orelse
                     Set.memb comparePair ((c, b), rel))
               bs)
         bs
      end

fun inverse (cmp, cmp') rel =
      let val comparePairRev = Set.comparePair(cmp', cmp)
      in Set.map comparePairRev (fn (a, b) => (b, a)) rel end

fun reflexiveClosure cmp (rel, bs) =
      let val comparePair = Set.comparePair(cmp, cmp)
      in Set.union comparePair (rel, Set.times(bs, bs)) end

fun transitiveClosure cmp rel =
      let val comparePair = Set.comparePair(cmp, cmp)

          (* if for all x, y, z, if (x, y) and (y, z) are in olds,
                then (x, z) is in the union of news and olds,
             then closure(news, olds) is the transitive closure
             of the union of news and old *)

          fun closure(nil,            olds) = olds
            | closure((a, b) :: news, olds) =
                if Set.memb comparePair ((a, b), olds)
                then closure(news, olds)
                else let val lefts  =
                               Set.times
                               (apply
                                (cmp, cmp)
                                (inverse (cmp, cmp) olds, Set.sing a),
                                Set.sing b)
                         val rights =
                               Set.times(Set.sing a,
                                         apply (cmp, cmp) (olds, Set.sing b))
                     in closure(Set.toList lefts @ Set.toList rights @ news,
                                Set.union comparePair (Set.sing(a, b), olds))
                     end
      in closure(Set.toList rel, Set.empty) end

fun reflexiveTransitiveClosure cmp (rel, bs) =
      reflexiveClosure cmp (transitiveClosure cmp rel, bs)

fun symmetricClosure cmp rel =
      let val comparePair = Set.comparePair(cmp, cmp)
      in Set.union comparePair (rel, inverse (cmp, cmp) rel) end

fun transitiveSymmetricClosure cmp rel =
      transitiveClosure cmp (symmetricClosure cmp rel)

fun reflexiveTransitiveSymmetricClosure cmp (rel, bs) =
      reflexiveClosure cmp (transitiveSymmetricClosure cmp rel, bs)

fun compose (cmp, cmp', cmp'') (rel2, rel1) =
      Set.genUnion (Set.comparePair(cmp, cmp''))
                   (Set.mapToList (fn (a, b) =>
                                        Set.times(Set.sing a,
                                                  apply (cmp', cmp'')
                                                        (rel2, Set.sing b)))
                                  rel1)

fun function cmp rel =
      let fun func(_, nil)          = true
            | func(a, (b, c) :: xs) = cmp(a, b) <> EQUAL andalso func(b, xs)
      in case Set.toList rel of
              nil          => true
            | (a, _) :: xs => func(a, xs)
      end

fun functionFromTo (cmp, cmp') (rel, bs, cs) =
      function cmp rel                     andalso
      Set.equal cmp (domain cmp rel, bs)   andalso
      Set.subset cmp' (range cmp' rel, cs)

fun injection (cmp, cmp') rel =
      function cmp rel andalso function cmp' (inverse (cmp, cmp') rel)

fun bijectionFromTo (cmp, cmp') (rel, bs, cs) =
      injection (cmp, cmp') rel           andalso
      Set.equal cmp (domain cmp rel, bs)  andalso
      Set.equal cmp' (range cmp' rel, cs)

fun bijectionFromAvoiding (cmp, cmp') (rel, bs, cs) =
      injection (cmp, cmp') rel                        andalso
      Set.equal cmp (domain cmp rel, bs)               andalso
      Set.isEmpty(Set.inter cmp' (range cmp' rel, cs))

fun bijectionFromSupersetAvoiding (cmp, cmp') (rel, bs, cs) =
      injection (cmp, cmp') rel                        andalso
      Set.subset cmp (bs, domain cmp rel)              andalso
      Set.isEmpty(Set.inter cmp' (range cmp' rel, cs))

fun applyFunction cmp rel =
      if function cmp rel
      then fn a =>
                let fun err() =
                          M.errorString(fn () =>
                                             ["argument", "not", "in",
                                              "domain"])

                    fun find nil            = err()
                      | find ((b, c) :: xs) =
                          case cmp(a, b) of
                               LESS    => err()
                             | EQUAL   => c
                             | GREATER => find xs
                in find(Set.toList rel) end
      else M.errorString(fn () => ["relation", "isn't", "function"])

fun restrictFunction (cmp, cmp') (rel, cs) =
      if not(function cmp rel)
        then M.errorString(fn () => ["relation", "isn't", "function"])
      else if not(Set.subset cmp (cs, domain cmp rel))
        then M.errorString(fn () => ["function", "has", "improper", "domain"])
      else let val comparePair = Set.comparePair(cmp, cmp')

               fun restr nil            = nil
                 | restr ((a, b) :: xs) =
                     if Set.memb cmp (a, cs)
                     then (a, b) :: restr xs
                     else restr xs
           in Set.fromList comparePair (restr(Set.toList rel)) end

fun updateFunction (cmp, cmp') (rel, c, d) =
      if not(function cmp rel)
      then M.errorString(fn () => ["relation", "isn't", "function"])
      else let val comparePair = Set.comparePair(cmp, cmp')

               fun updat nil                  = [(c, d)]
                 | updat (ys as (a, b) :: xs) =
                     case cmp(c, a) of
                          LESS    => (c, d) :: ys
                        | EQUAL   => (a, d) :: xs
                        | GREATER => (a, b) :: updat xs
           in Set.fromList comparePair (updat(Set.toList rel)) end

fun mlFunctionToFunction (cmp, cmp') (f, bs) =
      Set.map (Set.comparePair(cmp, cmp')) (fn a => (a, f a)) bs
        handle _ =>
                 M.errorString(fn () => ["bad", "ML", "function"])

end;
