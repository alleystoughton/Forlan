(****************************** string-aux.sml ********************************)

(* Copyright (C) 2001-2012 Alley Stoughton

   The file is part of the Forlan toolset for experimenting with
   formal languages.  See the file COPYING.txt for copying and
   usage restrictions. *)

structure StringAux :> STRING_AUX =
struct

local
  fun rm nil       = nil
    | rm (x :: xs) =
        if Char.isSpace x
        then rm xs
        else x :: rm xs
in
  fun removeWhiteSpace s = implode(rm(explode s))
end

end;
