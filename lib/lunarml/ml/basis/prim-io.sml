signature PRIM_IO = sig
    type elem
    type vector
    type vector_slice
    type array
    type array_slice
    eqtype pos
    val compare : pos * pos -> order
    datatype reader = RD of { name : string
                            , chunkSize : int
                            , readVec : (int -> vector) option
                            , readArr : (array_slice -> int) option
                            , readVecNB : (int -> vector option) option
                            , readArrNB : (array_slice -> int option) option
                            , block : (unit -> unit) option
                            , canInput : (unit -> bool) option
                            , avail : unit -> Position.int option (* https://github.com/SMLFamily/BasisLibrary/wiki/2019-001-Correction-to-PRIM_IO *)
                            , getPos : (unit -> pos) option
                            , setPos : (pos -> unit) option
                            , endPos : (unit -> pos) option
                            , verifyPos : (unit -> pos) option
                            , close : unit -> unit
                            , ioDesc : OS.IO.iodesc option
                            }
    datatype writer = WR of { name : string
                            , chunkSize : int
                            , writeVec : (vector_slice -> int) option
                            , writeArr : (array_slice -> int) option
                            , writeVecNB : (vector_slice -> int option) option
                            , writeArrNB : (array_slice -> int option) option
                            , block : (unit -> unit) option
                            , canOutput : (unit -> bool) option
                            , getPos : (unit -> pos) option
                            , setPos : (pos -> unit) option
                            , endPos : (unit -> pos) option
                            , verifyPos : (unit -> pos) option
                            , close : unit -> unit
                            , ioDesc : OS.IO.iodesc option
                            }
    val openVector : vector -> reader
    val nullRd : unit -> reader
    val nullWr : unit -> writer
    val augmentReader : reader -> reader
    val augmentWriter : writer -> writer
end;

functor PrimIO (structure Vector : MONO_VECTOR
                structure VectorSlice : MONO_VECTOR_SLICE
                structure Array : MONO_ARRAY
                structure ArraySlice : MONO_ARRAY_SLICE
                sharing type Vector.elem = VectorSlice.elem = Array.elem = ArraySlice.elem
                sharing type Vector.vector = VectorSlice.vector = Array.vector = ArraySlice.vector
                sharing type VectorSlice.slice = ArraySlice.vector_slice
                sharing type Array.array = ArraySlice.array
                val someElem : Vector.elem
                eqtype pos
                val compare : pos * pos -> order
               ) :> PRIM_IO where type elem = Vector.elem
                            where type vector = Vector.vector
                            where type vector_slice = VectorSlice.slice
                            where type array = Array.array
                            where type array_slice = ArraySlice.slice
                            where type pos = pos
= struct
type elem = Vector.elem
type vector = Vector.vector
type vector_slice = VectorSlice.slice
type array = Array.array
type array_slice = ArraySlice.slice
type pos = pos
val compare = compare
datatype reader = RD of { name : string
                        , chunkSize : int
                        , readVec : (int -> vector) option
                        , readArr : (array_slice -> int) option
                        , readVecNB : (int -> vector option) option
                        , readArrNB : (array_slice -> int option) option
                        , block : (unit -> unit) option
                        , canInput : (unit -> bool) option
                        , avail : unit -> Position.int option
                        , getPos : (unit -> pos) option
                        , setPos : (pos -> unit) option
                        , endPos : (unit -> pos) option
                        , verifyPos : (unit -> pos) option
                        , close : unit -> unit
                        , ioDesc : OS.IO.iodesc option
                        }
datatype writer = WR of { name : string
                        , chunkSize : int
                        , writeVec : (vector_slice -> int) option
                        , writeArr : (array_slice -> int) option
                        , writeVecNB : (vector_slice -> int option) option
                        , writeArrNB : (array_slice -> int option) option
                        , block : (unit -> unit) option
                        , canOutput : (unit -> bool) option
                        , getPos : (unit -> pos) option
                        , setPos : (pos -> unit) option
                        , endPos : (unit -> pos) option
                        , verifyPos : (unit -> pos) option
                        , close : unit -> unit
                        , ioDesc : OS.IO.iodesc option
                        }
fun openVector (content : vector)
    = let val name = "<openVector>"
          val pos = ref 0
          val closed = ref false
          fun check function = if !closed then
                                   raise IO.Io { name = name, function = function, cause = IO.ClosedStream }
                               else
                                   ()
          fun readVec n = if n < 0 then
                              raise Size
                          else
                              let val current = !pos
                                  val total = Vector.length content
                                  val newPos = Int.min (current + n, total)
                              in pos := newPos
                               ; VectorSlice.vector (VectorSlice.slice (content, current, SOME (newPos - current)))
                              end
          fun readArr slice = let val current = !pos
                                  val total = Vector.length content
                                  val newPos = Int.min (current + ArraySlice.length slice, total)
                                  val (baseArr, start, _) = ArraySlice.base slice
                                  val count = newPos - current
                              in pos := newPos
                               ; ArraySlice.copyVec { src = VectorSlice.slice (content, current, SOME count), dst = baseArr, di = start }
                               ; count
                              end
      in RD { name = name
            , chunkSize = Vector.length content
            , readVec = SOME (fn n => (check "readVec"; readVec n))
            , readArr = SOME (fn slice => (check "readArr"; readArr slice))
            , readVecNB = SOME (fn n => (check "readVecNB"; SOME (readVec n)))
            , readArrNB = SOME (fn slice => (check "readArrNB"; SOME (readArr slice)))
            , block = SOME (fn () => check "block")
            , canInput = SOME (fn () => (check "canInput"; true))
            , avail = fn () => (check "avail"; SOME (Position.fromInt (Vector.length content - !pos)))
            , getPos = NONE (* SOME (fn () => Position.fromInt (!pos)) *)
            , setPos = NONE (* SOME (fn newPos => (check "setPos"; pos := Position.toInt newPos)) *)
            , endPos = NONE (* SOME (fn () => (check "endPos"; Position.fromInt (Vector.length content))) *)
            , verifyPos = NONE (* SOME (fn () => (check "verifyPos"; Position.fromInt (!pos))) *)
            , close = fn () => closed := true
            , ioDesc = NONE
            }
      end
fun nullRd () = let val name = "<nullRd>"
                    val closed = ref false
                    fun check function = if !closed then
                                             raise IO.Io { name = name, function = function, cause = IO.ClosedStream }
                                         else
                                             ()
                    val empty = Vector.fromList []
                in RD { name = name
                      , chunkSize = 1
                      , readVec = SOME (fn n => (check "readVec"; if n < 0 then raise Size else empty))
                      , readArr = SOME (fn slice => (check "readArr"; 0))
                      , readVecNB = SOME (fn n => (check "readVecNB"; SOME empty))
                      , readArrNB = SOME (fn slice => (check "readArrNB"; SOME 0))
                      , block = SOME (fn () => check "block")
                      , canInput = SOME (fn () => (check "canInput"; true))
                      , avail = fn () => SOME 0
                      , getPos = NONE (* SOME (fn () => 0) *)
                      , setPos = NONE (* SOME (fn _ => check "setPos") *)
                      , endPos = NONE (* SOME (fn () => (check "endPos"; 0)) *)
                      , verifyPos = NONE (* SOME (fn () => (check "verifyPos"; 0)) *)
                      , close = fn () => closed := true
                      , ioDesc = NONE
                      }
                end
fun nullWr () = let val name = "<nullWr>"
                    val closed = ref false
                    fun check function = if !closed then
                                             raise IO.Io { name = name, function = function, cause = IO.ClosedStream }
                                         else
                                             ()
                in WR { name = name
                      , chunkSize = 1
                      , writeVec = SOME (fn slice => (check "writeVec"; VectorSlice.length slice))
                      , writeArr = SOME (fn slice => (check "writeArr"; ArraySlice.length slice))
                      , writeVecNB = SOME (fn slice => (check "writeVecNB"; SOME (VectorSlice.length slice)))
                      , writeArrNB = SOME (fn slice => (check "writeArrNB"; SOME (ArraySlice.length slice)))
                      , block = SOME (fn () => check "block")
                      , canOutput = SOME (fn () => (check "canOutput"; true))
                      , getPos = NONE
                      , endPos = NONE
                      , setPos = NONE
                      , verifyPos = NONE
                      , close = fn () => closed := true
                      , ioDesc = NONE
                      }
                end
fun augmentReader (RD { name, chunkSize, readVec, readArr, readVecNB, readArrNB, block, canInput, avail, getPos, setPos, endPos, verifyPos, close, ioDesc })
    = let val empty = Vector.fromList []
          val readVec' = case readVec of
                             SOME _ => readVec
                           | NONE => case readArr of
                                         SOME ra => SOME (fn n => let val arr = Array.array (n, someElem)
                                                                      val actual = ra (ArraySlice.full arr)
                                                                  in ArraySlice.vector (ArraySlice.slice (arr, 0, SOME actual))
                                                                  end
                                                         )
                                       | NONE => case (block, readVecNB, readArrNB) of
                                                     (SOME block', SOME rvNB, _) =>
                                                     SOME (fn n => if n < 0 then
                                                                       raise Size
                                                                   else
                                                                       case rvNB n of
                                                                           SOME content => content
                                                                         | NONE => ( block' ()
                                                                                   ; case rvNB n of
                                                                                         SOME content => content
                                                                                       | NONE => empty (* should not occur *)
                                                                                   )
                                                          )
                                                   | (SOME block', NONE, SOME raNB) =>
                                                     SOME (fn n => if n < 0 then
                                                                       raise Size
                                                                   else
                                                                       let val arr = Array.array (n, someElem)
                                                                           val aslice = ArraySlice.full arr
                                                                       in case raNB aslice of
                                                                              SOME actual => ArraySlice.vector (ArraySlice.slice (arr, 0, SOME actual))
                                                                            | NONE => ( block' ()
                                                                                      ; case raNB aslice of
                                                                                            SOME actual => ArraySlice.vector (ArraySlice.slice (arr, 0, SOME actual))
                                                                                          | NONE => empty (* should not occur *)
                                                                                      )
                                                                       end
                                                          )
                                                   | _ => NONE
          val readArr' = case readArr of
                             SOME _ => readArr
                           | NONE => case readVec of
                                         SOME rv => SOME (fn slice => let val v = rv (ArraySlice.length slice)
                                                                          val (base, start, _) = ArraySlice.base slice
                                                                      in Array.copyVec { src = v, dst = base, di = start }
                                                                       ; Vector.length v
                                                                      end
                                                         )
                                       | NONE => case (block, readVecNB, readArrNB) of
                                                     (SOME block', _, SOME raNB) =>
                                                     SOME (fn slice => case raNB slice of
                                                                           SOME actual => actual
                                                                         | NONE => ( block' ()
                                                                                   ; case raNB slice of
                                                                                         SOME actual => actual
                                                                                       | NONE => 0 (* should not occur *)
                                                                                   )
                                                          )
                                                   | (SOME block', SOME rvNB, NONE) =>
                                                     SOME (fn slice => let val n = ArraySlice.length slice
                                                                           val (base, start, _) = ArraySlice.base slice
                                                                       in case rvNB n of
                                                                              SOME v => ( Array.copyVec { src = v, dst = base, di = start }
                                                                                        ; Vector.length v
                                                                                        )
                                                                            | NONE => ( block' ()
                                                                                      ; case rvNB n of
                                                                                            SOME v => ( Array.copyVec { src = v, dst = base, di = start }
                                                                                                      ; Vector.length v
                                                                                                      )
                                                                                          | NONE => 0 (* should not occur *)
                                                                                      )
                                                                       end
                                                          )
                                                   | _ => NONE
          val readVecNB' = case readVecNB of
                               SOME _ => readVecNB
                             | NONE => case readArrNB of
                                           SOME raNB => SOME (fn n => if n < 0 then
                                                                          raise Size
                                                                      else
                                                                          let val arr = Array.array (n, someElem)
                                                                          in case raNB (ArraySlice.full arr) of
                                                                                 SOME actual => SOME (ArraySlice.vector (ArraySlice.slice (arr, 0, SOME actual)))
                                                                               | NONE => NONE
                                                                          end
                                                             )
                                         | NONE => case (canInput, readVec, readArr) of
                                                       (SOME canInput', SOME rv, _) =>
                                                       SOME (fn n => if canInput' () then
                                                                         SOME (rv n)
                                                                     else
                                                                         NONE
                                                            )
                                                     | (SOME canInput', NONE, SOME ra) =>
                                                       SOME (fn n => if canInput' () then
                                                                         if n < 0 then
                                                                             raise Size
                                                                         else
                                                                             let val arr = Array.array (n, someElem)
                                                                                 val actual = ra (ArraySlice.full arr)
                                                                             in SOME (ArraySlice.vector (ArraySlice.slice (arr, 0, SOME actual)))
                                                                             end
                                                                     else
                                                                         NONE
                                                            )
                                                     | _ => NONE
          val readArrNB' = case readArrNB of
                               SOME _ => readArrNB
                             | NONE => case readVecNB of
                                           SOME rvNB =>
                                           SOME (fn slice => case rvNB (ArraySlice.length slice) of
                                                                 SOME v => let val (base, start, _) = ArraySlice.base slice
                                                                           in Array.copyVec { src = v, dst = base, di = start }
                                                                            ; SOME (Vector.length v)
                                                                           end
                                                               | NONE => NONE
                                                )
                                         | NONE => case (canInput, readVec, readArr) of
                                                       (SOME canInput', _, SOME ra) =>
                                                       SOME (fn slice => if canInput' () then
                                                                             SOME (ra slice)
                                                                         else
                                                                             NONE
                                                            )
                                                     | (SOME canInput', SOME rv, NONE) =>
                                                       SOME (fn slice => if canInput' () then
                                                                             let val v = rv (ArraySlice.length slice)
                                                                                 val (base, start, _) = ArraySlice.base slice
                                                                             in Array.copyVec { src = v, dst = base, di = start }
                                                                              ; SOME (Vector.length v)
                                                                             end
                                                                         else
                                                                             NONE
                                                            )
                                                     | _ => NONE
      in RD { name = name
            , chunkSize = chunkSize
            , readVec = readVec'
            , readArr = readArr'
            , readVecNB = readVecNB'
            , readArrNB = readArrNB'
            , block = block
            , canInput = canInput
            , avail = avail
            , getPos = getPos
            , setPos = setPos
            , endPos = endPos
            , verifyPos = verifyPos
            , close = close
            , ioDesc = ioDesc
            }
      end
fun augmentWriter (WR { name, chunkSize, writeVec, writeArr, writeVecNB, writeArrNB, block, canOutput, getPos, setPos, endPos, verifyPos, close, ioDesc })
    = let val writeVec' = case writeVec of
                              SOME _ => writeVec
                            | NONE => case writeArr of
                                          SOME wa =>
                                          SOME (fn slice =>
                                                   let val arr = Array.array (VectorSlice.length slice, someElem)
                                                   in ArraySlice.copyVec { src = slice, dst = arr, di = 0 }
                                                    ; wa (ArraySlice.full arr)
                                                   end
                                               )
                                        | NONE => case (block, writeVecNB, writeArrNB) of
                                                      (SOME block', SOME wvNB, _) =>
                                                      SOME (fn slice =>
                                                               case wvNB slice of
                                                                   SOME n => n
                                                                 | NONE => ( block' ()
                                                                           ; case wvNB slice of
                                                                                 SOME n => n
                                                                               | NONE => 0 (* should not occur *)
                                                                           )
                                                           )
                                                    | (SOME block', NONE, SOME waNB) =>
                                                      SOME (fn slice =>
                                                               let val arr = Array.array (VectorSlice.length slice, someElem)
                                                                   val aslice = ArraySlice.full arr
                                                               in ArraySlice.copyVec { src = slice, dst = arr, di = 0 }
                                                                ; case waNB aslice of
                                                                      SOME n => n
                                                                    | NONE => ( block' ()
                                                                              ; case waNB aslice of
                                                                                    SOME n => n
                                                                                  | NONE => 0 (* should not occur *)
                                                                              )
                                                               end
                                                           )
                                                    | _ => NONE
          val writeArr' = case writeArr of
                              SOME _ => writeArr
                            | NONE => case writeVec of
                                          SOME wv =>
                                          SOME (fn slice => let val v = ArraySlice.vector slice
                                                            in wv (VectorSlice.full v)
                                                            end
                                               )
                                        | NONE => case (block, writeVecNB, writeArrNB) of
                                                      (SOME block', _, SOME waNB) =>
                                                      SOME (fn slice =>
                                                               case waNB slice of
                                                                   SOME n => n
                                                                 | NONE => ( block' ()
                                                                           ; case waNB slice of
                                                                                 SOME n => n
                                                                               | NONE => 0 (* should not occur *)
                                                                           )
                                                           )
                                                    | (SOME block', SOME wvNB, NONE) =>
                                                      SOME (fn slice =>
                                                               let val vslice = VectorSlice.full (ArraySlice.vector slice)
                                                               in case wvNB vslice of
                                                                      SOME n => n
                                                                    | NONE => ( block' ()
                                                                              ; case wvNB vslice of
                                                                                    SOME n => n
                                                                                  | NONE => 0 (* should not occur *)
                                                                              )
                                                               end
                                                           )
                                                    | _ => NONE
          val writeVecNB' = case writeVecNB of
                                SOME _ => writeVecNB
                              | NONE => case writeArrNB of
                                            SOME waNB =>
                                            SOME (fn slice =>
                                                     let val arr = Array.array (VectorSlice.length slice, someElem)
                                                     in ArraySlice.copyVec { src = slice, dst = arr, di = 0 }
                                                      ; waNB (ArraySlice.full arr)
                                                     end
                                                 )
                                          | NONE => case (canOutput, writeVec, writeArr) of
                                                        (SOME canOutput', SOME wv, _) =>
                                                        SOME (fn slice =>
                                                                 if canOutput' () then
                                                                     SOME (wv slice)
                                                                 else
                                                                     NONE
                                                             )
                                                      | (SOME canOutput', NONE, SOME wa) =>
                                                        SOME (fn slice =>
                                                                 if canOutput' () then
                                                                     let val arr = Array.array (VectorSlice.length slice, someElem)
                                                                     in ArraySlice.copyVec { src = slice, dst = arr, di = 0 }
                                                                      ; SOME (wa (ArraySlice.full arr))
                                                                     end
                                                                 else
                                                                     NONE
                                                             )
                                                      | _ => NONE
          val writeArrNB' = case writeArrNB of
                                SOME _ => writeArrNB
                              | NONE => case writeVecNB of
                                            SOME wvNB =>
                                            SOME (fn slice =>
                                                     let val vslice = VectorSlice.full (ArraySlice.vector slice)
                                                     in wvNB vslice
                                                     end
                                                 )
                                          | NONE => case (canOutput, writeVec, writeArr) of
                                                        (SOME canOutput', _, SOME wa) =>
                                                        SOME (fn slice =>
                                                                 if canOutput' () then
                                                                     SOME (wa slice)
                                                                 else
                                                                     NONE
                                                             )
                                                      | (SOME canOutput', SOME wv, NONE) =>
                                                        SOME (fn slice =>
                                                                 if canOutput' () then
                                                                     let val vslice = VectorSlice.full (ArraySlice.vector slice)
                                                                     in SOME (wv vslice)
                                                                     end
                                                                 else
                                                                     NONE
                                                             )
                                                      | _ => NONE
      in WR { name = name
            , chunkSize = chunkSize
            , writeVec = writeVec'
            , writeArr = writeArr'
            , writeVecNB = writeVecNB'
            , writeArrNB = writeArrNB'
            , block = block
            , canOutput = canOutput
            , getPos = getPos
            , setPos = setPos
            , endPos = endPos
            , verifyPos = verifyPos
            , close = close
            , ioDesc = ioDesc
            }
      end
end;

structure BinPrimIO :> PRIM_IO where type elem = Word8.word
                               where type vector = Word8Vector.vector
                               where type vector_slice = Word8VectorSlice.slice (* extension *)
                               where type array = Word8Array.array
                               where type array_slice = Word8ArraySlice.slice (* extension *)
                               where type pos = Position.int
= PrimIO (structure Vector = Word8Vector
          structure VectorSlice = Word8VectorSlice
          structure Array = Word8Array
          structure ArraySlice = Word8ArraySlice
          val someElem = 0w0 : Word8.word
          type pos = Position.int
          val compare = Position.compare
         );

structure TextPrimIO :> PRIM_IO where type elem = char
                                where type vector = CharVector.vector
                                where type vector_slice = CharVectorSlice.slice (* extension *)
                                where type array = CharArray.array
                                where type array_slice = CharArraySlice.slice (* extension *)
                                (* where type pos = Position.int: abstract *)
= PrimIO (structure Vector = CharVector
          structure VectorSlice = CharVectorSlice
          structure Array = CharArray
          structure ArraySlice = CharArraySlice
          val someElem = #"\000" : char
          type pos = Position.int
          val compare = Position.compare
         );
