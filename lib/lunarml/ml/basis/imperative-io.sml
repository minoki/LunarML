signature IMPERATIVE_IO = sig
    structure StreamIO : STREAM_IO

    type vector = StreamIO.vector
    type elem = StreamIO.elem

    type instream
    type outstream

    val input : instream -> vector
    val input1 : instream -> elem option
    val inputN : instream * int -> vector
    val inputAll : instream -> vector
    val canInput : instream * int -> int option
    val lookahead : instream -> elem option
    val closeIn : instream -> unit
    val endOfStream : instream -> bool

    val output : outstream * vector -> unit
    val output1 : outstream * elem -> unit
    val flushOut : outstream -> unit
    val closeOut : outstream -> unit

    val mkInstream : StreamIO.instream -> instream
    val getInstream : instream -> StreamIO.instream
    val setInstream : instream * StreamIO.instream -> unit

    val mkOutstream : StreamIO.outstream -> outstream
    val getOutstream : outstream -> StreamIO.outstream
    val setOutstream : outstream * StreamIO.outstream -> unit
    val getPosOut : outstream -> StreamIO.out_pos
    val setPosOut : outstream * StreamIO.out_pos -> unit
end;

functor ImperativeIO (structure StreamIO : STREAM_IO
                      structure Vector : MONO_VECTOR
                      structure Array : MONO_ARRAY
                      sharing type StreamIO.elem = Vector.elem = Array.elem
                      sharing type StreamIO.vector = Vector.vector = Array.vector
                     ) : IMPERATIVE_IO = struct
structure StreamIO = StreamIO
type vector = StreamIO.vector
type elem = StreamIO.elem
type instream = StreamIO.instream ref
type outstream = StreamIO.outstream ref

fun input stream = case StreamIO.input (!stream) of
                       (chunk, stream') => ( stream := stream'
                                           ; chunk
                                           )
fun input1 stream = case StreamIO.input1 (!stream) of
                        NONE => NONE
                      | SOME (e, stream') => ( stream := stream'
                                             ; SOME e
                                             )
fun inputN (stream, n) = case StreamIO.inputN (!stream, n) of
                             (chunk, stream') => ( stream := stream'
                                                 ; chunk
                                                 )
fun inputAll stream = case StreamIO.inputAll (!stream) of
                          (chunk, stream') => ( stream := stream'
                                              ; chunk
                                              )
fun canInput (stream, n) = StreamIO.canInput (!stream, n)
fun lookahead stream = case StreamIO.input1 (!stream) of
                           NONE => NONE
                         | SOME (e, _) => SOME e
fun closeIn stream = StreamIO.closeIn (!stream)
fun endOfStream stream = StreamIO.endOfStream (!stream)

fun output (stream, chunk) = StreamIO.output (!stream, chunk)
fun output1 (stream, elem) = StreamIO.output1 (!stream, elem)
fun flushOut stream = StreamIO.flushOut (!stream)
fun closeOut stream = StreamIO.closeOut (!stream)

fun mkInstream stream = ref stream
fun getInstream stream = !stream
fun setInstream (stream, stream') = stream := stream'

val mkOutstream = ref : StreamIO.outstream -> outstream
val getOutstream = ! : outstream -> StreamIO.outstream
val setOutstream = op := : outstream * StreamIO.outstream -> unit
fun getPosOut stream = StreamIO.getPosOut (!stream)
fun setPosOut (stream, p) = stream := StreamIO.setPosOut p
end;
