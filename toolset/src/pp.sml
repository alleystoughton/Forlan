(********************************** pp.sml ***********************************)

(* Copyright (C) 2001-2012 Alley Stoughton

   The file is part of the Forlan toolset for experimenting with
   formal languages.  See the file COPYING.txt for copying and
   usage restrictions. *)

structure PP :> PP =
struct

val newline =
      if SMLofNJ.SysInfo.getOSKind() = SMLofNJ.SysInfo.WIN32
      then "\r\n"
      else "\n"

fun isRoom n = n <= Params.getPrintingLineLength()

fun spaces n = StringCvt.padRight #" " n ""

datatype pp = Block    of int * bool * pp list
            | Decorate of string * pp * string

fun sizeOfNoBreaks(Block(sizeNoBreaks, _, _)) = sizeNoBreaks
  | sizeOfNoBreaks(Decorate(s1, pp, s2))      =
      size s1 + sizeOfNoBreaks pp + size s2

fun prettyNoBreaks(Block(_, spaceNoBreak, pps)) =
      prettyEntriesNoBreaks(spaceNoBreak, pps)
  | prettyNoBreaks(Decorate(s1, pp, s2))        =
      [s1] @ prettyNoBreaks pp @ [s2]

and prettyEntriesNoBreaks(_,            nil)       = nil
  | prettyEntriesNoBreaks(_,            [pp])      = prettyNoBreaks pp
  | prettyEntriesNoBreaks(spaceNoBreak, pp :: pps) =
      prettyNoBreaks pp                        @
      [if spaceNoBreak then " " else ""]       @
      prettyEntriesNoBreaks(spaceNoBreak, pps)

fun pretty(ind, after, Block(_, spaceNoBreak, pps)) =
      prettyEntries(ind, ind, after, spaceNoBreak, pps)
  | pretty(ind, after, Decorate(s1, pp, s2))        =
      [s1] @ pretty(ind + size s1, size s2 + after, pp) @ [s2]

and prettyEntries(_,      _,      _,     _,            nil)       = nil
  | prettyEntries(gblInd, locInd, after, spaceNoBreak, [pp])      =
      let val sizeNoBreaks = sizeOfNoBreaks pp
      in if locInd = gblInd
           then pretty(gblInd, after, pp)
         else if isRoom
                 (locInd + (if spaceNoBreak then 1 else 0) +
                  sizeNoBreaks + after)
           then [if spaceNoBreak then " " else ""] @ prettyNoBreaks pp
         else [newline, spaces gblInd] @ pretty(gblInd, after, pp)
      end
  | prettyEntries(gblInd, locInd, after, spaceNoBreak, pp :: pps) =
      let val sizeNoBreaks = sizeOfNoBreaks pp
      in if locInd = gblInd
           then if isRoom(gblInd + sizeNoBreaks)
                then prettyNoBreaks pp @
                     prettyEntries
                     (gblInd, gblInd + sizeNoBreaks, after, spaceNoBreak, pps)
                else pretty(gblInd, 0, pp) @
                     [newline, spaces gblInd] @
                     prettyEntries(gblInd, gblInd, after, spaceNoBreak, pps)
         else if isRoom
                 (locInd + (if spaceNoBreak then 1 else 0) + sizeNoBreaks)
           then [if spaceNoBreak then " " else ""] @
                prettyNoBreaks pp @
                prettyEntries
                (gblInd,
                 locInd + (if spaceNoBreak then 1 else 0) + sizeNoBreaks,
                 after, spaceNoBreak, pps)
         else if isRoom(gblInd + sizeNoBreaks)
           then [newline, spaces gblInd] @
                prettyNoBreaks pp @
                prettyEntries
                (gblInd, gblInd + sizeNoBreaks, after, spaceNoBreak, pps)
         else [newline, spaces gblInd] @
              pretty(gblInd, 0, pp) @
              [newline, spaces gblInd] @
              prettyEntries(gblInd, gblInd, after, spaceNoBreak, pps)
      end

fun toString pp = concat(pretty(0, 0, pp))

val empty = Block(0, true, nil)

fun block(spaceNoBreak, pps) =
      let val len          = length pps
          val sizeNoBreaks =
                if len = 0
                then 0
                else ListAux.sum(map sizeOfNoBreaks pps) +
                     (if spaceNoBreak then 1 else 0) * (len - 1)
      in Block(sizeNoBreaks, spaceNoBreak, pps) end

fun decorate(s1, pp, s2) = Decorate(s1, pp, s2)

fun quote pp = decorate("\"", pp, "\"")

fun comma pp = decorate("", pp, ",")

fun colon pp = decorate("", pp, ":")

fun semicolon pp = decorate("", pp, ";")

fun fromString s = Decorate(s, empty, "")

fun fromStringSplitEscape s =
      block(false,
            map (fn c => fromString(Char.toString c))
                (explode s))

end;
