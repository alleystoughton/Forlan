(* messages.sig
 *
 * COPYRIGHT (c) 2012 Alley Stoughton.
 *
 * extracted from messages.mldoc
 *)

signature MESSAGES =
  sig
    val messagePP : (unit -> PP.pp list) -> unit
    val messageString : (unit -> string list) -> unit
    exception Error
    val errorPP : (unit -> PP.pp list) -> 'a
    val errorString : (unit -> string list) -> 'a
    val quiet : (unit -> 'a) -> 'a
    exception CannotHappen
    val cannotHappen : unit -> 'a
  end
