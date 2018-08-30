(********************************** tab.sml **********************************)

(* Copyright (C) 2012 Alley Stoughton

   The file is part of the Forlan toolset for experimenting with
   formal languages.  See the file COPYING.txt for copying and
   usage restrictions. *)

structure Tab :> TAB =
struct

type ('a, 'b)tab = ('a * 'b)list

fun lookup cmp =
      let fun lookup(nil,                _)  = NONE
            | lookup((x, y) :: bindings, x') =
                case cmp(x', x) of
                     LESS    => NONE
                   | EQUAL   => SOME y
                   | GREATER => lookup(bindings, x')
      in lookup end

val isEmpty = null

val empty = nil

fun update cmp =
      let fun single(nil,                               x', y') = [(x', y')]
            | single(bindings' as ((x, y) :: bindings), x', y') =
                case cmp(x', x) of
                     LESS    => (x', y') :: bindings'
                   | EQUAL   => (x, y') :: bindings
                   | GREATER => (x, y) :: single(bindings, x', y')

          fun update(tab, nil)                = tab
            | update(tab, (x, y) :: bindings) =
                update(single(tab, x, y), bindings)
      in update end

fun domain cmp =
      let val unionKeySet = Set.union cmp

          fun domain nil                  = Set.empty
            | domain ((x, _) :: bindings) =
                unionKeySet(Set.sing x, domain bindings)
      in domain end

fun toList bindings = bindings

fun valid cmp =
      let fun vali nil                                = true
            | vali [_]                                = true
            | vali ((x, _) :: (bndgs as (y, _) :: _)) =
                     cmp(x, y) = LESS andalso vali bndgs
      in vali end

fun fromList cmp =
      let val update = update cmp
          val valid  = valid cmp

          fun fromList bindings =
                if valid bindings
                then bindings
                else update(empty, bindings)
      in fromList end

end;
