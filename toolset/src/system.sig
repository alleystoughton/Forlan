(* system.sig
 *
 * COPYRIGHT (c) 2012 Alley Stoughton.
 *
 * extracted from system.mldoc
 *)

signature SYSTEM =
  sig
    val makeTempFile : unit -> string
    val runJForlan : string -> OS.Process.status
  end
