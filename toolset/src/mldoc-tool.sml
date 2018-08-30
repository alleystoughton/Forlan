(***************************** mldoc-tool.sml *******************************)

(* Copyright (C) 2012 Alley Stoughton

   The file is part of the Forlan toolset for experimenting with
   formal languages.  See the file COPYING.txt for copying and
   usage restrictions. *)

(* tell CM to process .mldoc files with the script mldoc-tool,
   producing .sig files *)

structure MLDOCTool =
struct

val _ =
      Tools.registerStdShellCmdTool
      {tool           = "MLDOC Tool",
       class          = "mldoc",
       cmdStdPath     = fn () => ("mldoc-tool", nil),
       template       = NONE,
       extensionStyle =
         Tools.REPLACE(["mldoc"], [("sig", SOME "sml", fn _ => NONE)]),
       dflopts        = nil}

val _ =
      Tools.registerClassifier
      (Tools.stdSfxClassifier {sfx = "mldoc", class = "mldoc"});

end;
