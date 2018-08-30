(* params.sig
 *
 * COPYRIGHT (c) 2012 Alley Stoughton.
 *
 * extracted from params.mldoc
 *)

signature PARAMS =
  sig
    val getSearchPath : unit -> string list
    val setSearchPath : string list -> unit
    val getWorkingDirectory : unit -> string
    val setWorkingDirectory : string -> unit
    val getPrintingListLength : unit -> int
    val setPrintingListLength : int -> unit
    val getPrintingStringSize : unit -> int
    val setPrintingStringSize : int -> unit
    val getPrintingDataStructureDepth : unit -> int
    val setPrintingDataStructureDepth : int -> unit
    val getPrintingLineLength : unit -> int
    val setPrintingLineLength : int -> unit
    val setPrintingOfGarbageCollectionMessages : bool -> unit
    val getTrackExceptions : unit -> bool
    val setTrackExceptions : bool -> unit
    val getCompilationManagerVerbosity : unit -> bool
    val setCompilationManagerVerbosity : bool -> unit
  end
