(******************************** export.sml *********************************)

(* Copyright (C) 2001-2019 Alley Stoughton

   The file is part of the Forlan toolset for experimenting with
   formal languages.  See the file COPYING.txt for copying and
   usage restrictions. *)

structure Export :> EXPORT =
struct

structure M  = Messages

fun existsFile s = OS.FileSys.access(s, nil)

local
  val versionList = (*#version_id(Compiler.version)*) [110, 96]

  fun vlToStr (n :: (ms as m :: ls)) =
        Int.toString n ^ "." ^ vlToStr ms
    | vlToStr [n]                    = Int.toString n
    | vlToStr _                      = M.cannotHappen()

in
  val smlVersion = vlToStr versionList
end

local
  val toLower = String.map Char.toLower
in
  fun arch() = toLower(SMLofNJ.SysInfo.getHostArch())

  fun opSys() = toLower(SMLofNJ.SysInfo.getOSName())
end

fun setSearchPath() =
      case OS.Process.getEnv "ForlanPath" of
           NONE      => Params.setSearchPath nil
         | SOME valu =>
             let val pathSep =
                       if SMLofNJ.SysInfo.getOSKind() = SMLofNJ.SysInfo.UNIX
                       then #":"
                       else #";"
                 val path    =
                       String.fields (fn c => c = pathSep) valu
             in Params.setSearchPath path end

fun export() =
      if SMLofNJ.exportML "forlan-heap"
      then ((* line buffer stdOut even if it's not a terminal; done so
               that transcripts are updated line-by-line *)
            TextIO.StreamIO.setBufferMode(TextIO.getOutstream TextIO.stdOut,
                                          IO.LINE_BUF);
            setSearchPath();
            Params.setPrintingListLength 250;
            Params.setPrintingStringSize 250;
            Params.setPrintingDataStructureDepth 20;
            Params.setPrintingLineLength 80;
            Params.setPrintingOfGarbageCollectionMessages false;
            Params.setTrackExceptions false;
            Params.setCompilationManagerVerbosity false;
            M.messageString
            (fn () =>
                  ["Forlan", "Version", Version.getVersion(), "(based",
                   "on", "Standard", "ML", "of", "New", "Jersey",
                   "Version", smlVersion ^ ")"]))
      else (M.messageString
            (fn () =>
                  ["Heap", "image", "written", "to", "file:",
                   concat["\"forlan-heap.", arch(), "-", opSys(), "\""]]);
            OS.Process.exit OS.Process.success)

end;
