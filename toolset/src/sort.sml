(********************************* sort.sml **********************************)

(* Copyright (C) 2012 Alley Stoughton

   The file is part of the Forlan toolset for experimenting with
   formal languages.  See the file COPYING.txt for copying and
   usage restrictions. *)

structure Sort :> SORT =
struct

type 'a total_ordering = 'a * 'a -> order

fun equal cmp (x, y) = cmp(x, y) = EQUAL

fun less cmp (x, y) = cmp(x, y) = LESS

fun lessEqual cmp (x, y) = cmp(x, y) = LESS orelse cmp(x, y) = EQUAL

fun sorted _           nil                   = true
  | sorted _           [_]                   = true
  | sorted (dups, cmp) (x :: (zs as y :: _)) =
      case cmp(x, y) of
           LESS    => sorted (dups, cmp) zs
         | EQUAL   => dups andalso sorted (dups, cmp) zs
         | GREATER => false

fun insert _           (x, nil)             = [x]
  | insert (dups, cmp) (x, y_ys as y :: ys) =
      case cmp(x, y) of
           LESS    => x :: y_ys
         | EQUAL   => if dups then x :: y_ys else y_ys
         | GREATER => y :: insert (dups, cmp) (x, ys)

fun merge _           (nil,           ys)            = ys
  | merge _           (xs,            nil)           = xs
  | merge (dups, cmp) (us as x :: xs, vs as y :: ys) =
      case cmp(x, y) of
           LESS    => x :: merge (dups, cmp) (xs, vs)
         | EQUAL   =>
             if dups
             then x :: x :: merge (dups, cmp) (xs, ys)
             else x :: merge (dups, cmp) (xs, ys)
         | GREATER => y :: merge (dups, cmp) (us, ys)

fun sort (dups, cmp) xs =
      if sorted (dups, cmp) xs
      then xs
      else let fun srt(_, [x]) = [x]
                 | srt(n, xs)  =
                     let val l        = n div 2
                         val m        = l + n mod 2
                         val (ys, zs) = ListAux.splitAt(xs, l)
                     in merge (dups, cmp) (srt(l, ys), srt(m, zs)) end
           in srt(length xs, xs) end

end;
