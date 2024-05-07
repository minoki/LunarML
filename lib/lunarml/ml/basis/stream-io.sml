signature STREAM_IO = sig
    type elem
    type vector

    type instream
    type outstream
    type out_pos

    type reader
    type writer
    type pos

    val input : instream -> vector * instream
    val input1 : instream -> (elem * instream) option
    val inputN : instream * int -> vector * instream
    val inputAll : instream -> vector * instream
    val canInput : instream * int -> int option
    val closeIn : instream -> unit
    val endOfStream : instream -> bool

    val output : outstream * vector -> unit
    val output1 : outstream * elem -> unit
    val flushOut : outstream -> unit
    val closeOut : outstream -> unit

    val mkInstream : reader * vector -> instream
    val getReader : instream -> reader * vector
    val filePosIn : instream -> pos

    val setBufferMode : outstream * IO.buffer_mode -> unit
    val getBufferMode : outstream -> IO.buffer_mode

    val mkOutstream : writer * IO.buffer_mode -> outstream
    val getWriter : outstream -> writer * IO.buffer_mode
    val getPosOut : outstream -> out_pos
    val setPosOut : out_pos -> outstream
    val filePosOut : out_pos -> pos
end;

signature TEXT_STREAM_IO = sig
    include STREAM_IO
                where type vector = CharVector.vector
                where type elem = Char.char
    val inputLine : instream -> (string * instream) option
    val outputSubstr : outstream * Substring.substring -> unit
end;

(*
functor StreamIO (structure PrimIO : PRIM_IO
                  structure Vector : MONO_VECTOR
                  structure Array : MONO_ARRAY
                  sharing type PrimIO.elem = Vector.elem = Array.elem
                  sharing type PrimIO.vector = Vector.vector = Array.vector
                  sharing type PrimIO.array = Array.array
                  val someElem : PrimIO.elem
                 ) :> STREAM_IO where type elem = PrimIO.elem
                                where type vector = PrimIO.vector
                                where type reader = PrimIO.reader
                                where type writer = PrimIO.writer
                                where type pos = PrimIO.pos
= struct
type elem = PrimIO.elem
type vector = PrimIO.vector
type reader = PrimIO.reader
type writer = PrimIO.writer
type pos = PrimIO.pos
structure Instream = struct
datatype instate = TIP of reader
end
end;
*)
