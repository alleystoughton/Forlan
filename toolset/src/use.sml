(************************************ use ************************************)

(* Copyright (C) 2001-2012 Alley Stoughton

   The file is part of the Forlan toolset for experimenting with
   formal languages.  See the file COPYING.txt for copying and
   usage restrictions. *)

structure Use :> USE =
struct

structure M = Messages

val standardUse = Compiler.Interact.useFile

val lastUsedFile : string option ref = ref NONE

fun existsFile s = OS.FileSys.access(s, nil)

fun findFile fil =
      let val path = Params.getSearchPath()

          fun err() =
                M.errorPP
                (fn () =>
                      [PP.fromString "file",
                       PP.quote(PP.fromStringSplitEscape fil),
                       PP.fromString "not", PP.fromString "found"])

          fun fnd nil           = err()
            | fnd (dir :: dirs) =
                let val dirFil = OS.Path.concat(dir, fil)
                in if existsFile dirFil then dirFil else fnd dirs end
      in if existsFile fil
           then fil
         else if OS.Path.isAbsolute fil
           then err()
         else fnd path
      end

fun use ""  =
      (case !lastUsedFile of
            NONE     =>
              M.errorString(fn () => ["no", "previously", "used", "file"])
          | SOME fil => (standardUse(findFile fil); ()))
  | use fil = (lastUsedFile := SOME fil; standardUse(findFile fil); ())

end;
