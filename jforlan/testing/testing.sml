local
  exception Error

  fun error s = (print s; print "\n"; raise Error)

  fun existsDir dir =
        Posix.FileSys.ST.isDir(Posix.FileSys.stat dir)
          handle _ => false

  fun isTxt s =
        let val n = size s
        in n >= 5 andalso
           substring(s, n - 4, 4) = ".txt"
        end

  fun readDir dir =
        let val stm = Posix.FileSys.opendir dir
  
            fun read() =
                  case Posix.FileSys.readdir stm of
                       NONE   => []
                     | SOME s =>
                         if isTxt s
                         then dir ^ "/" ^ s :: read()
                         else read()
        in read() end

  fun testFA file =
        case SOME(FA.input file) handle _ => NONE of
             NONE   => error(file ^ ": unable to load object")
           | SOME x =>
               case SOME(FA.jforlanEdit x) handle _ => NONE of
                    NONE   => error(file ^ ": failure")
                  | SOME y =>
                      if FA.equal(x, y)
                      then print(file ^ ": success!\n")
                      else error(file ^ ": failure!\n")

  fun testRFA file =
        case SOME(RFA.input file) handle _ => NONE of
             NONE   => error(file ^ ": unable to load object")
           | SOME x =>
               case SOME(RFA.jforlanEdit x) handle _ => NONE of
                    NONE   => error(file ^ ": failure")
                  | SOME y =>
                      if RFA.equal(x, y)
                      then print(file ^ ": success!\n")
                      else error(file ^ ": failure!\n")

  fun testPT file =
        case SOME(PT.input file) handle _ => NONE of
             NONE   => error(file ^ ": unable to load object")
           | SOME x =>
               case SOME(PT.jforlanEdit x) handle _ => NONE of
                    NONE   => error(file ^ ": failure")
                  | SOME y =>
                      if PT.equal(x, y)
                      then print(file ^ ": success!\n")
                      else error(file ^ ": failure!\n")

  fun testReg file =
        case SOME(Reg.input file) handle _ => NONE of
             NONE   => error(file ^ ": unable to load object")
           | SOME x =>
               case SOME(Reg.jforlanEdit x) handle _ => NONE of
                    NONE   => error(file ^ ": failure")
                  | SOME y =>
                      if Reg.equal(x, y)
                      then print(file ^ ": success!\n")
                      else error(file ^ ": failure!\n")

  fun testProg file =
        case SOME(Prog.input file) handle _ => NONE of
             NONE   => error(file ^ ": unable to load object")
           | SOME x =>
               case SOME(Prog.jforlanEdit x) handle _ => NONE of
                    NONE   => error(file ^ ": failure")
                  | SOME y =>
                      if Prog.equal(x, y)
                      then print(file ^ ": success!\n")
                      else error(file ^ ": failure!\n")
in
  fun doit() =
        if not(existsDir "tests")      orelse
           not(existsDir "tests/fa")   orelse
           not(existsDir "tests/rfa")  orelse
           not(existsDir "tests/pt")   orelse
           not(existsDir "tests/reg")  orelse
           not(existsDir "tests/prog")
        then error "testing directory has bad structure"
        else let val fas   = readDir "tests/fa"
                 val rfas  = readDir "tests/rfa"
                 val pts   = readDir "tests/pt"
                 val regs  = readDir "tests/reg"
                 val progs = readDir "tests/prog"
             in app testFA fas;
                app testRFA rfas;
                app testPT pts;
                app testReg regs;
                app testProg progs
             end
end;
