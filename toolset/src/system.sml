(******************************** system.sml **********************************)

(* Copyright (C) 2001-2012 Alley Stoughton

   The file is part of the Forlan toolset for experimenting with
   formal languages.  See the file COPYING.txt for copying and
   usage restrictions. *)

structure System :> SYSTEM =
struct

fun isUnix() = SMLofNJ.SysInfo.getOSKind() = SMLofNJ.SysInfo.UNIX

fun exists path = OS.FileSys.access(path, nil)

fun makeTempFile() =
      let val pathStart =
                if isUnix()
                then "/tmp/forlan-temp"
                else case OS.Process.getEnv "TEMP" of
                          NONE   => "c:\\forlan-temp"
                        | SOME s => s ^ "\\" ^ "forlan-temp"

          fun findTemp s =
                if exists s
                then findTemp(s ^ "+")
                else case SOME(TextIO.openOut s)
                            handle _ => NONE of
                          NONE     =>
                            Messages.errorString
                            (fn () =>
                                  ["unable", "to", "create", "temporary",
                                   "file"])
                        | SOME stm => (TextIO.closeOut stm; s)
      in findTemp pathStart end

fun runJForlan s =
      case SOME(OS.Process.system("jforlan " ^ s))
             handle _ => NONE of
           NONE        =>
             (Messages.messageString
              (fn () => ["unable", "to", "execute", "JForlan"]);
              OS.Process.failure)
         | SOME status => status

end;
