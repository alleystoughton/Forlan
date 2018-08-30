(********************************* debug.sml *********************************)

(* Copyright (C) 2012 Alley Stoughton

   The file is part of the Forlan toolset for experimenting with
   formal languages.  See the file COPYING.txt for copying and
   usage restrictions. *)

structure Debug :> DEBUG =
struct

fun debug h (msgPP, x) =
      let val pp = PP.block(true, [PP.colon msgPP, h x])
      in print(PP.toString pp ^ "\n");
         print("(press RETURN to continue or interrupt character to " ^
               "interrupt) ");
         TextIO.inputLine TextIO.stdIn;
         x
      end

end;
