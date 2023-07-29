structure TextIO :> sig
              (*
              type instream
              type outstream
              type vector = string
              type elem = char
              val input : instream -> vector
              val input1 : instream -> elem option
              val inputN : instream * int -> vector
              val inputAll : instream -> vector
              val closeIn : instream -> unit
              val endOfStream : instream -> bool
              val output : outstream * vector -> unit
              val output1 : outstream * elem -> unit
              val flushOut : outstream -> unit
              val closeOut : outstream -> unit
              val inputLine : instream -> string option
              val openIn : string -> instream
              val openOut : string -> outstream
              val openAppend : string -> outstream
              val stdIn : instream
              val stdOut : outstream
              val stdErr : outstream
              *)
              val print : string -> unit
          end = struct
local
    _esImport [pure] { stdout } from "node:process";
in
fun print (s : string) = ( JavaScript.method (stdout, "write") #[JavaScript.unsafeToValue s]; ())
end
end;
val print = TextIO.print;
