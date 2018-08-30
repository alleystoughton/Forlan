(******************************* messages.sml ********************************)

(* Copyright (C) 2012 Alley Stoughton

   The file is part of the Forlan toolset for experimenting with
   formal languages.  See the file COPYING.txt for copying and
   usage restrictions. *)

structure Messages :> MESSAGES =
struct

structure Cont = SMLofNJ.Cont

val quietRef = ref false

fun mask() = Signals.maskSignals Signals.MASKALL

fun unmask() = Signals.unmaskSignals Signals.MASKALL

fun protect f =
      let val _ = mask()
          val x = f()
          val _ = unmask()
      in x end

fun print s = protect(fn () => TextIO.print s)

fun messagePP f =
      (if !quietRef
       then ()
       else (print(PP.toString(PP.block(true, f()))); print "\n"))

fun messageString f = messagePP(fn () => map PP.fromString (f()))

exception Error

fun errorPP f = (messagePP f; raise Error)

fun errorString f = (messageString f; raise Error)

fun quiet f =
      if !quietRef
      then f()
      else let val _          = mask()
               val _          = quietRef := true
               val topLevCont = !Unsafe.topLevelCont
               val _          =
                     Unsafe.topLevelCont :=
                     Cont.callcc
                     (fn k =>
                           (Cont.callcc(fn k' => Cont.throw k k');
                            quietRef := false;
                            Cont.throw topLevCont ()))
               val _          = unmask()
               val x          =                             
                     f()
                       handle e =>
                                (mask();
                                 quietRef := false;
                                 Unsafe.topLevelCont := topLevCont;
                                 unmask();
                                 raise e)
               val _          = mask()
               val _          = quietRef := false
               val _          = Unsafe.topLevelCont := topLevCont
               val _          = unmask()
           in x end

exception CannotHappen

fun cannotHappen() = raise CannotHappen

end;
