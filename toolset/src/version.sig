(* version.sig
 *
 * COPYRIGHT (c) 2021 Alley Stoughton.
 *
 * extracted from version.mldoc
 *)

signature VERSION =
  sig
    val getVersion : unit -> string
    val getSMLNJVersion : unit -> string
  end
