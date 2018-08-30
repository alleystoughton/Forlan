(******************************* list-aux.sml ********************************)

(* Copyright (C) 2001-2012 Alley Stoughton

   The file is part of the Forlan toolset for experimenting with
   formal languages.  See the file COPYING.txt for copying and
   usage restrictions. *)

structure ListAux :> LIST_AUX =
struct

fun sum nil       = 0
  | sum (x :: xs) = x + sum xs

fun prod nil       = 1
  | prod (x :: xs) = x * prod xs

fun max nil       = raise Empty
  | max [x]       = x
  | max (x :: xs) = Int.max(x, max xs)

fun min nil       = raise Empty
  | min [x]       = x
  | min (x :: xs) = Int.min(x, min xs)

fun sub(xs, n) = List.nth(xs, n - 1)

fun update(xs, n, y) =
      if n < 1
      then raise Subscript
      else let fun update(nil,     _) = raise Subscript
                 | update(_ :: xs, 1) = y :: xs
                 | update(x :: xs, n) = x :: update(xs, n - 1)
           in update(xs, n) end;

fun position f xs =
      let fun pos(_, nil)     = NONE
            | pos(n, x :: xs) = if f x then SOME n else pos(n + 1, xs)
      in pos(1, xs) end

fun repeat(x, n) =
      if n < 0
      then raise Domain
      else let fun rep 0 = nil
                 | rep n = x :: rep(n - 1)
           in rep n end

fun allButLast xs =
      List.take(xs, length xs - 1) handle _ => raise Empty

fun splitAt(xs, n) =
      if n < 0 orelse n > length xs
      then raise Domain
      else let fun split(ys, xs,      0) = (rev ys, xs)
                 | split(ys, x :: xs, n) = split(x :: ys, xs, n - 1)
                 | split(_,  _,       _) = raise Fail "cannot happen"
           in split(nil, xs, n) end

fun allSplittings nil               = [(nil, nil)]
  | allSplittings (x_xs as x :: xs) =
      (nil, x_xs) ::
      map (fn (ys, zs) => (x :: ys, zs)) (allSplittings xs)

fun adjacentElts (x :: (zs as y :: _)) = (x, y) :: adjacentElts zs
  | adjacentElts _                     = nil

fun fromTo(n, m) = if n > m then nil else n :: fromTo(n + 1, m)

end;
