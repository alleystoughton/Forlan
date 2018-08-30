(******************************** params.sml *********************************)

(* Copyright (C) 2012 Alley Stoughton

   The file is part of the Forlan toolset for experimenting with
   formal languages.  See the file COPYING.txt for copying and
   usage restrictions. *)

structure Params :> PARAMS =
struct

val searchPath : string list ref = ref nil

fun getSearchPath() = !searchPath

fun setSearchPath xs = searchPath :=
      map
      (fn x =>
            if OS.Path.isAbsolute x
            then x
            else OS.Path.mkAbsolute
                 {path       = x,
                  relativeTo = OS.FileSys.getDir()})
      xs

val getWorkingDirectory = OS.FileSys.getDir

val setWorkingDirectory = OS.FileSys.chDir

fun getPrintingListLength() = !Control.Print.printLength

fun setPrintingListLength n = Control.Print.printLength := (Int.max(n, 0))

fun getPrintingStringSize() = !Control.Print.stringDepth

fun setPrintingStringSize n = Control.Print.stringDepth := (Int.max(n, 0))

fun getPrintingDataStructureDepth() = !Control.Print.printDepth

fun setPrintingDataStructureDepth n =
      Control.Print.printDepth := (Int.max(n, 0))

fun getPrintingLineLength() = !Control.Print.linewidth - 1

fun setPrintingLineLength n = Control.Print.linewidth := Int.max(n + 1, 0)

val setPrintingOfGarbageCollectionMessages = SMLofNJ.Internals.GC.messages

fun getTrackExceptions() = !Control.trackExn

fun setTrackExceptions b = Control.trackExn := b

fun getCompilationManagerVerbosity() = #get (CM.Control.verbose) ()

val setCompilationManagerVerbosity = #set (CM.Control.verbose)

end;
