(* rel.sig
 *
 * COPYRIGHT (c) 2012 Alley Stoughton.
 *
 * extracted from rel.mldoc
 *)

signature REL =
  sig
    type ('a,'b) rel = ('a * 'b) Set.set
    val domain : 'a Sort.total_ordering -> ('a, 'b) rel -> 'a Set.set
    val range : 'b Sort.total_ordering -> ('a, 'b) rel -> 'b Set.set
    val relationFromTo : 'a Sort.total_ordering * 'b Sort.total_ordering
                           -> ('a, 'b) rel * 'a Set.set * 'b Set.set -> bool
    val relationOn : 'a Sort.total_ordering
                       -> ('a, 'a) rel * 'a Set.set -> bool
    val apply : 'a Sort.total_ordering * 'b Sort.total_ordering
                  -> ('a, 'b) rel * 'a Set.set -> 'b Set.set
    val reflexive : 'a Sort.total_ordering -> ('a, 'a) rel * 'a Set.set -> bool
    val symmetric : 'a Sort.total_ordering -> ('a, 'a) rel -> bool
    val antisymmetric : 'a Sort.total_ordering -> ('a, 'a) rel -> bool
    val transitive : 'a Sort.total_ordering -> ('a, 'a) rel -> bool
    val total : 'a Sort.total_ordering -> ('a, 'a) rel * 'a Set.set -> bool
    val inverse : 'a Sort.total_ordering * 'b Sort.total_ordering
                    -> ('a, 'b) rel -> ('b, 'a) rel
    val reflexiveClosure : 'a Sort.total_ordering
                             -> ('a, 'a) rel * 'a Set.set -> ('a, 'a) rel
    val transitiveClosure : 'a Sort.total_ordering
                              -> ('a, 'a) rel -> ('a, 'a) rel
    val reflexiveTransitiveClosure : 'a Sort.total_ordering
                                       -> ('a, 'a) rel * 'a Set.set
                                         -> ('a, 'a) rel
    val symmetricClosure : 'a Sort.total_ordering
                             -> ('a, 'a) rel -> ('a, 'a) rel
    val transitiveSymmetricClosure : 'a Sort.total_ordering
                                       -> ('a, 'a) rel -> ('a, 'a) rel
    val reflexiveTransitiveSymmetricClosure : 'a Sort.total_ordering
                                                -> ('a, 'a) rel * 'a Set.set
                                                  -> ('a, 'a) rel
    val compose : 'a Sort.total_ordering
                    * 'b Sort.total_ordering
                    * 'c Sort.total_ordering
                    -> ('b, 'c) rel * ('a, 'b) rel -> ('a, 'c) rel
    val function : 'a Sort.total_ordering -> ('a, 'b) rel -> bool
    val functionFromTo : 'a Sort.total_ordering * 'b Sort.total_ordering
                           -> ('a, 'b) rel * 'a Set.set * 'b Set.set -> bool
    val injection : 'a Sort.total_ordering * 'b Sort.total_ordering
                      -> ('a, 'b) rel -> bool
    val bijectionFromTo : 'a Sort.total_ordering * 'b Sort.total_ordering
                            -> ('a, 'b) rel * 'a Set.set * 'b Set.set -> bool
    val bijectionFromAvoiding : 'a Sort.total_ordering * 'b Sort.total_ordering
                                  -> ('a, 'b) rel * 'a Set.set * 'b Set.set
                                    -> bool
    val bijectionFromSupersetAvoiding : 'a Sort.total_ordering
                                          * 'b Sort.total_ordering
                                          -> ('a, 'b) rel
                                            * 'a Set.set
                                            * 'b Set.set -> bool
    val applyFunction : 'a Sort.total_ordering -> ('a, 'b) rel -> 'a -> 'b
    val restrictFunction : 'a Sort.total_ordering * 'b Sort.total_ordering
                             -> ('a, 'b) rel * 'a Set.set -> ('a, 'b) rel
    val updateFunction : 'a Sort.total_ordering * 'b Sort.total_ordering
                           -> ('a, 'b) rel * 'a * 'b -> ('a, 'b) rel
    val mlFunctionToFunction : 'a Sort.total_ordering * 'b Sort.total_ordering
                                 -> ('a -> 'b) * 'a Set.set -> ('a, 'b) rel
  end
