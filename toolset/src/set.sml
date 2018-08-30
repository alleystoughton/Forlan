(********************************** set.sml **********************************)

(* Copyright (C) 2012 Alley Stoughton

   The file is part of the Forlan toolset for experimenting with
   formal languages.  See the file COPYING.txt for copying and
   usage restrictions. *)

structure Set :> SET =
struct

structure M = Messages

type 'a set = 'a list

fun memb cmp =
      let fun memb(_, nil)     = false
            | memb(x, y :: ys) =
                case cmp(x, y) of
                     LESS    => false
                   | EQUAL   => true
                   | GREATER => memb(x, ys)
      in memb end

val fromList = fn cmp => Sort.sort(false, cmp)

fun toList xs = xs

fun isEmpty nil = true
  | isEmpty _   = false

fun isNonEmpty nil = false
  | isNonEmpty _   = true

fun size xs = length xs

val hd = List.hd

val tl = List.tl

fun compare cmp (xs, ys) =
      let fun lexCmp(nil,     nil)     = EQUAL
            | lexCmp(x :: xs, y :: ys) =
                (case cmp(x, y) of
                      LESS    => LESS
                    | EQUAL   => lexCmp(xs, ys)
                    | GREATER => GREATER)
            | lexCmp(_,       _)       = M.cannotHappen()
      in case Int.compare(length xs, length ys) of
           LESS    => LESS
         | EQUAL   => lexCmp(xs, ys)
         | GREATER => GREATER
      end         

fun subset cmp =
      let fun subset(nil,             _)       = true
            | subset(_,               nil)     = false
            | subset(x_xs as x :: xs, y :: ys) =
                case cmp(x, y) of
                     LESS    => false
                   | EQUAL   => subset(xs, ys)
                   | GREATER => subset(x_xs, ys)
      in subset end

fun equal cmp (xs, ys) = compare cmp (xs, ys) = EQUAL

val all = List.all

val exists = List.exists

val empty = nil

fun sing x = [x]

val filter = List.filter

val partition = List.partition

val position = ListAux.position

fun map cmp f xs = fromList cmp (List.map f xs)

val mapFromList = map

val mapToList = List.map

fun union cmp =
      let fun union(nil,             ys)              = ys
            | union(xs,              nil)             = xs
            | union(x_xs as x :: xs, y_ys as y :: ys) =
                case cmp(x, y) of
                     LESS    => x :: union(xs, y_ys)
                   | EQUAL   => x :: union(xs, ys)
                   | GREATER => y :: union(x_xs, ys)
      in union end

fun genUnion cmp =
      let val union = union cmp

          fun genUnion nil         = nil
            | genUnion (xs :: xss) = union(xs, genUnion xss)
      in genUnion end

fun inter cmp =
      let fun inter(nil,             _)               = nil
            | inter(_,               nil)             = nil
            | inter(x_xs as x :: xs, y_ys as y :: ys) =
                case cmp(x, y) of
                     LESS    => inter(xs, y_ys)
                   | EQUAL   => x :: inter(xs, ys)
                   | GREATER => inter(x_xs, ys)
      in inter end

fun genInter cmp =
      let val inter = inter cmp

          fun genInter nil         =
                M.errorString
                (fn () => ["attempt", "to", "intersect", "empty", "set"])
            | genInter [xs]        = xs
            | genInter (xs :: xss) = inter(xs, genInter xss)
      in genInter end

fun minus cmp =
      let fun minus(nil,             _)               = nil
            | minus(xs,              nil)             = xs
            | minus(x_xs as x :: xs, y_ys as y :: ys) =
                case cmp(x, y) of
                     LESS    => x :: minus(xs, y_ys)
                   | EQUAL   => minus(xs, ys)
                   | GREATER => minus(x_xs, ys)
      in minus end

fun comparePair (cmp1, cmp2) ((x, y), (x', y')) =
      case cmp1(x, x') of
           LESS    => LESS
         | EQUAL   => cmp2(y, y')
         | GREATER => GREATER

fun times(nil,     _)  = nil
  | times(x :: xs, ys) =
      let fun pair nil       = times(xs, ys)
            | pair (y :: ys) = (x, y) :: pair ys
      in pair ys end;

fun compareTriple (cmp1, cmp2, cmp3) ((x, y, z), (x', y', z')) =
      case cmp1(x, x') of
           LESS    => LESS
         | EQUAL   =>
             (case cmp2(y, y') of
                   LESS    => LESS
                 | EQUAL   => cmp3(z, z')
                 | GREATER => GREATER)
         | GREATER => GREATER

fun times3(xs, ys, zs) =
      let val ps = times(ys, zs)

          fun triple nil       = nil
            | triple (x :: xs) =
                let fun triple' nil            = triple xs
                      | triple' ((y, z) :: ps) = (x, y, z) :: triple' ps
                in triple' ps end
      in triple xs end;

fun compareList cmp (xs, ys) =
      let fun compEqLen(nil,       nil)    = EQUAL
            | compEqLen (x :: xs, y :: ys) =
                (case cmp(x, y) of
                      LESS    => LESS
                    | EQUAL   => compEqLen(xs, ys)
                    | GREATER => GREATER)
            | compEqLen _                  = M.cannotHappen()
      in case Int.compare(length xs, length ys) of
              LESS    => LESS
            | EQUAL   => compEqLen(xs, ys)
            | GREATER => GREATER
      end

fun genTimes nil         = [nil]
  | genTimes (xs :: xss) =
      let val yss = genTimes xss

          fun cons nil       = nil
            | cons (x :: xs) =
                let fun cons' nil         = cons xs
                      | cons' (ys :: yss) = (x :: ys) :: cons' yss
                in cons' yss end
      in cons xs end

end;
