signature PACK_WORD = sig
    val bytesPerElem : int
    val isBigEndian : bool
    val subVec : Word8Vector.vector * int -> LargeWord.word
    val subVecX : Word8Vector.vector * int -> LargeWord.word
    val subArr : Word8Array.array * int -> LargeWord.word
    val subArrX : Word8Array.array * int -> LargeWord.word
    val update : Word8Array.array * int * LargeWord.word -> unit
end;

local
    val orb = LargeWord.orb
    val << = LargeWord.<<
    val >> = LargeWord.>>
    (*
    fun << (x, y) = _primCall "Word64.<<.unchecked" (x, y)
    fun >> (x, y) = _primCall "Word64.>>.unchecked" (x, y)
    *)
    fun checkRange (v, i, bytesPerElem) = if i < 0 then
                                              raise Subscript
                                          else
                                              let val ii = bytesPerElem * i
                                              in if Word8Vector.length v < ii + bytesPerElem then
                                                     raise Subscript
                                                 else
                                                     ii
                                              end
    fun checkRangeA (a, i, bytesPerElem) = if i < 0 then
                                               raise Subscript
                                           else
                                               let val ii = bytesPerElem * i
                                               in if Word8Array.length a < ii + bytesPerElem then
                                                      raise Subscript
                                                  else
                                                      ii
                                               end
in
structure PackWord8 = struct
val bytesPerElem = 1
fun subVec (v, i) = let val ii = checkRange (v, i, bytesPerElem)
                        val x0 = UnsafeWord8Vector.sub (v, ii)
                    in Word8.toLarge x0
                    end
fun subVecX (v, i) = let val ii = checkRange (v, i, bytesPerElem)
                         val x0 = UnsafeWord8Vector.sub (v, ii)
                     in Word8.toLargeX x0
                     end
fun subArr (a, i) = let val ii = checkRangeA (a, i, bytesPerElem)
                        val x0 = UnsafeWord8Array.sub (a, ii)
                    in Word8.toLarge x0
                    end
fun subArrX (a, i) = let val ii = checkRangeA (a, i, bytesPerElem)
                         val x0 = UnsafeWord8Array.sub (a, ii)
                     in Word8.toLarge x0
                     end
fun update (a, i, w) = let val ii = checkRangeA (a, i, bytesPerElem)
                       in UnsafeWord8Array.update (a, ii, Word8.fromLarge w)
                       end
end

structure PackWord8Big :> PACK_WORD = struct
open PackWord8
val isBigEndian = true
end

structure PackWord8Little :> PACK_WORD = struct
open PackWord8
val isBigEndian = false
end

structure PackWord16Big :> PACK_WORD = struct
val bytesPerElem = 2
val isBigEndian = true
fun construct (x0, x1) = (Word8.toLarge x0 .<<. 0w8) .orb. Word8.toLarge x1
fun constructX (x0, x1) = (Word8.toLargeX x0 .<<. 0w8) .orb. Word8.toLarge x1
fun subVec (v, i) = let val ii = checkRange (v, i, bytesPerElem)
                        val x0 = UnsafeWord8Vector.sub (v, ii)
                        val x1 = UnsafeWord8Vector.sub (v, ii + 1)
                    in construct (x0, x1)
                    end
fun subVecX (v, i) = let val ii = checkRange (v, i, bytesPerElem)
                         val x0 = UnsafeWord8Vector.sub (v, ii)
                         val x1 = UnsafeWord8Vector.sub (v, ii + 1)
                     in constructX (x0, x1)
                     end
fun subArr (a, i) = let val ii = checkRangeA (a, i, bytesPerElem)
                        val x0 = UnsafeWord8Array.sub (a, ii)
                        val x1 = UnsafeWord8Array.sub (a, ii + 1)
                    in construct (x0, x1)
                    end
fun subArrX (a, i) = let val ii = checkRangeA (a, i, bytesPerElem)
                         val x0 = UnsafeWord8Array.sub (a, ii)
                         val x1 = UnsafeWord8Array.sub (a, ii + 1)
                     in constructX (x0, x1)
                     end
fun update (a, i, w) = let val ii = checkRangeA (a, i, bytesPerElem)
                       in UnsafeWord8Array.update (a, ii, Word8.fromLarge (w .>>. 0w8))
                        ; UnsafeWord8Array.update (a, ii + 1, Word8.fromLarge w)
                       end
end

structure PackWord16Little :> PACK_WORD = struct
val bytesPerElem = 2
val isBigEndian = false
fun construct (x0, x1) = (Word8.toLarge x1 .<<. 0w8) .orb. Word8.toLarge x0
fun constructX (x0, x1) = (Word8.toLargeX x1 .<<. 0w8) .orb. Word8.toLarge x0
fun subVec (v, i) = let val ii = checkRange (v, i, bytesPerElem)
                        val x0 = UnsafeWord8Vector.sub (v, ii)
                        val x1 = UnsafeWord8Vector.sub (v, ii + 1)
                    in construct (x0, x1)
                    end
fun subVecX (v, i) = let val ii = checkRange (v, i, bytesPerElem)
                         val x0 = UnsafeWord8Vector.sub (v, ii)
                         val x1 = UnsafeWord8Vector.sub (v, ii + 1)
                     in constructX (x0, x1)
                     end
fun subArr (a, i) = let val ii = checkRangeA (a, i, bytesPerElem)
                        val x0 = UnsafeWord8Array.sub (a, ii)
                        val x1 = UnsafeWord8Array.sub (a, ii + 1)
                    in construct (x0, x1)
                    end
fun subArrX (a, i) = let val ii = checkRangeA (a, i, bytesPerElem)
                         val x0 = UnsafeWord8Array.sub (a, ii)
                         val x1 = UnsafeWord8Array.sub (a, ii + 1)
                     in constructX (x0, x1)
                     end
fun update (a, i, w) = let val ii = checkRangeA (a, i, bytesPerElem)
                       in UnsafeWord8Array.update (a, ii, Word8.fromLarge (w .>>. 0w8))
                        ; UnsafeWord8Array.update (a, ii + 1, Word8.fromLarge w)
                       end
end

structure PackWord32Big :> PACK_WORD = struct
val bytesPerElem = 4
val isBigEndian = true
fun construct (x0, x1, x2, x3) = (Word8.toLarge x0 .<<. 0w24) .orb. (Word8.toLarge x1 .<<. 0w16) .orb. (Word8.toLarge x2 .<<. 0w8) .orb. Word8.toLarge x3
fun constructX (x0, x1, x2, x3) = (Word8.toLargeX x0 .<<. 0w24) .orb. (Word8.toLarge x1 .<<. 0w16) .orb. (Word8.toLarge x2 .<<. 0w8) .orb. Word8.toLarge x3
fun subVec (v, i) = let val ii = checkRange (v, i, bytesPerElem)
                        val x0 = UnsafeWord8Vector.sub (v, ii)
                        val x1 = UnsafeWord8Vector.sub (v, ii + 1)
                        val x2 = UnsafeWord8Vector.sub (v, ii + 2)
                        val x3 = UnsafeWord8Vector.sub (v, ii + 3)
                    in construct (x0, x1, x2, x3)
                    end
fun subVecX (v, i) = let val ii = checkRange (v, i, bytesPerElem)
                         val x0 = UnsafeWord8Vector.sub (v, ii)
                         val x1 = UnsafeWord8Vector.sub (v, ii + 1)
                         val x2 = UnsafeWord8Vector.sub (v, ii + 2)
                         val x3 = UnsafeWord8Vector.sub (v, ii + 3)
                     in constructX (x0, x1, x2, x3)
                     end
fun subArr (a, i) = let val ii = checkRangeA (a, i, bytesPerElem)
                        val x0 = UnsafeWord8Array.sub (a, ii)
                        val x1 = UnsafeWord8Array.sub (a, ii + 1)
                        val x2 = UnsafeWord8Array.sub (a, ii + 2)
                        val x3 = UnsafeWord8Array.sub (a, ii + 3)
                    in construct (x0, x1, x2, x3)
                    end
fun subArrX (a, i) = let val ii = checkRangeA (a, i, bytesPerElem)
                         val x0 = UnsafeWord8Array.sub (a, ii)
                         val x1 = UnsafeWord8Array.sub (a, ii + 1)
                         val x2 = UnsafeWord8Array.sub (a, ii + 2)
                         val x3 = UnsafeWord8Array.sub (a, ii + 3)
                     in constructX (x0, x1, x2, x3)
                     end
fun update (a, i, w) = let val ii = checkRangeA (a, i, bytesPerElem)
                       in UnsafeWord8Array.update (a, ii, Word8.fromLarge (w .>>. 0w24))
                        ; UnsafeWord8Array.update (a, ii + 1, Word8.fromLarge (w .>>. 0w16))
                        ; UnsafeWord8Array.update (a, ii + 2, Word8.fromLarge (w .>>. 0w8))
                        ; UnsafeWord8Array.update (a, ii + 3, Word8.fromLarge w)
                       end
end

structure PackWord32Little :> PACK_WORD = struct
val bytesPerElem = 4
val isBigEndian = false
fun construct (x0, x1, x2, x3) = (Word8.toLarge x3 .<<. 0w24) .orb. (Word8.toLarge x2 .<<. 0w16) .orb. (Word8.toLarge x1 .<<. 0w8) .orb. Word8.toLarge x0
fun constructX (x0, x1, x2, x3) = (Word8.toLargeX x3 .<<. 0w24) .orb. (Word8.toLarge x2 .<<. 0w16) .orb. (Word8.toLarge x1 .<<. 0w8) .orb. Word8.toLarge x0
fun subVec (v, i) = let val ii = checkRange (v, i, bytesPerElem)
                        val x0 = UnsafeWord8Vector.sub (v, ii)
                        val x1 = UnsafeWord8Vector.sub (v, ii + 1)
                        val x2 = UnsafeWord8Vector.sub (v, ii + 2)
                        val x3 = UnsafeWord8Vector.sub (v, ii + 3)
                    in construct (x0, x1, x2, x3)
                    end
fun subVecX (v, i) = let val ii = checkRange (v, i, bytesPerElem)
                         val x0 = UnsafeWord8Vector.sub (v, ii)
                         val x1 = UnsafeWord8Vector.sub (v, ii + 1)
                         val x2 = UnsafeWord8Vector.sub (v, ii + 2)
                         val x3 = UnsafeWord8Vector.sub (v, ii + 3)
                     in constructX (x0, x1, x2, x3)
                     end
fun subArr (a, i) = let val ii = checkRangeA (a, i, bytesPerElem)
                        val x0 = UnsafeWord8Array.sub (a, ii)
                        val x1 = UnsafeWord8Array.sub (a, ii + 1)
                        val x2 = UnsafeWord8Array.sub (a, ii + 2)
                        val x3 = UnsafeWord8Array.sub (a, ii + 3)
                    in construct (x0, x1, x2, x3)
                    end
fun subArrX (a, i) = let val ii = checkRangeA (a, i, bytesPerElem)
                         val x0 = UnsafeWord8Array.sub (a, ii)
                         val x1 = UnsafeWord8Array.sub (a, ii + 1)
                         val x2 = UnsafeWord8Array.sub (a, ii + 2)
                         val x3 = UnsafeWord8Array.sub (a, ii + 3)
                     in constructX (x0, x1, x2, x3)
                     end
fun update (a, i, w) = let val ii = checkRangeA (a, i, bytesPerElem)
                       in UnsafeWord8Array.update (a, ii, Word8.fromLarge w)
                        ; UnsafeWord8Array.update (a, ii + 1, Word8.fromLarge (w .>>. 0w8))
                        ; UnsafeWord8Array.update (a, ii + 2, Word8.fromLarge (w .>>. 0w16))
                        ; UnsafeWord8Array.update (a, ii + 3, Word8.fromLarge (w .>>. 0w24))
                       end
end

structure PackWord64Big :> PACK_WORD = struct
val bytesPerElem = 8
val isBigEndian = true
fun construct (x0, x1, x2, x3, x4, x5, x6, x7) = (Word8.toLarge x0 .<<. 0w56) .orb. (Word8.toLarge x1 .<<. 0w48) .orb. (Word8.toLarge x2 .<<. 0w40) .orb. (Word8.toLarge x3 .<<. 0w32) .orb. (Word8.toLarge x4 .<<. 0w24) .orb. (Word8.toLarge x5 .<<. 0w16) .orb. (Word8.toLarge x6 .<<. 0w8) .orb. Word8.toLarge x7
fun constructX (x0, x1, x2, x3, x4, x5, x6, x7) = (Word8.toLargeX x0 .<<. 0w56) .orb. (Word8.toLarge x1 .<<. 0w48) .orb. (Word8.toLarge x2 .<<. 0w40) .orb. (Word8.toLarge x3 .<<. 0w32) .orb. (Word8.toLarge x4 .<<. 0w24) .orb. (Word8.toLarge x5 .<<. 0w16) .orb. (Word8.toLarge x6 .<<. 0w8) .orb. Word8.toLarge x7
fun subVec (v, i) = let val ii = checkRange (v, i, bytesPerElem)
                        val x0 = UnsafeWord8Vector.sub (v, ii)
                        val x1 = UnsafeWord8Vector.sub (v, ii + 1)
                        val x2 = UnsafeWord8Vector.sub (v, ii + 2)
                        val x3 = UnsafeWord8Vector.sub (v, ii + 3)
                        val x4 = UnsafeWord8Vector.sub (v, ii + 4)
                        val x5 = UnsafeWord8Vector.sub (v, ii + 5)
                        val x6 = UnsafeWord8Vector.sub (v, ii + 6)
                        val x7 = UnsafeWord8Vector.sub (v, ii + 7)
                    in construct (x0, x1, x2, x3, x4, x5, x6, x7)
                    end
fun subVecX (v, i) = let val ii = checkRange (v, i, bytesPerElem)
                         val x0 = UnsafeWord8Vector.sub (v, ii)
                         val x1 = UnsafeWord8Vector.sub (v, ii + 1)
                         val x2 = UnsafeWord8Vector.sub (v, ii + 2)
                         val x3 = UnsafeWord8Vector.sub (v, ii + 3)
                         val x4 = UnsafeWord8Vector.sub (v, ii + 4)
                         val x5 = UnsafeWord8Vector.sub (v, ii + 5)
                         val x6 = UnsafeWord8Vector.sub (v, ii + 6)
                         val x7 = UnsafeWord8Vector.sub (v, ii + 7)
                     in constructX (x0, x1, x2, x3, x4, x5, x6, x7)
                     end
fun subArr (a, i) = let val ii = checkRangeA (a, i, bytesPerElem)
                        val x0 = UnsafeWord8Array.sub (a, ii)
                        val x1 = UnsafeWord8Array.sub (a, ii + 1)
                        val x2 = UnsafeWord8Array.sub (a, ii + 2)
                        val x3 = UnsafeWord8Array.sub (a, ii + 3)
                        val x4 = UnsafeWord8Array.sub (a, ii + 4)
                        val x5 = UnsafeWord8Array.sub (a, ii + 5)
                        val x6 = UnsafeWord8Array.sub (a, ii + 6)
                        val x7 = UnsafeWord8Array.sub (a, ii + 7)
                    in construct (x0, x1, x2, x3, x4, x5, x6, x7)
                    end
fun subArrX (a, i) = let val ii = checkRangeA (a, i, bytesPerElem)
                         val x0 = UnsafeWord8Array.sub (a, ii)
                         val x1 = UnsafeWord8Array.sub (a, ii + 1)
                         val x2 = UnsafeWord8Array.sub (a, ii + 2)
                         val x3 = UnsafeWord8Array.sub (a, ii + 3)
                         val x4 = UnsafeWord8Array.sub (a, ii + 4)
                         val x5 = UnsafeWord8Array.sub (a, ii + 5)
                         val x6 = UnsafeWord8Array.sub (a, ii + 6)
                         val x7 = UnsafeWord8Array.sub (a, ii + 7)
                     in constructX (x0, x1, x2, x3, x4, x5, x6, x7)
                     end
fun update (a, i, w) = let val ii = checkRangeA (a, i, bytesPerElem)
                       in UnsafeWord8Array.update (a, ii, Word8.fromLarge (w .>>. 0w56))
                        ; UnsafeWord8Array.update (a, ii + 1, Word8.fromLarge (w .>>. 0w48))
                        ; UnsafeWord8Array.update (a, ii + 2, Word8.fromLarge (w .>>. 0w40))
                        ; UnsafeWord8Array.update (a, ii + 3, Word8.fromLarge (w .>>. 0w32))
                        ; UnsafeWord8Array.update (a, ii + 4, Word8.fromLarge (w .>>. 0w24))
                        ; UnsafeWord8Array.update (a, ii + 5, Word8.fromLarge (w .>>. 0w16))
                        ; UnsafeWord8Array.update (a, ii + 6, Word8.fromLarge (w .>>. 0w8))
                        ; UnsafeWord8Array.update (a, ii + 7, Word8.fromLarge w)
                       end
end

structure PackWord64Little :> PACK_WORD = struct
val bytesPerElem = 8
val isBigEndian = false
fun construct (x0, x1, x2, x3, x4, x5, x6, x7) = (Word8.toLarge x0 .<<. 0w56) .orb. (Word8.toLarge x1 .<<. 0w48) .orb. (Word8.toLarge x2 .<<. 0w40) .orb. (Word8.toLarge x3 .<<. 0w32) .orb. (Word8.toLarge x4 .<<. 0w24) .orb. (Word8.toLarge x5 .<<. 0w16) .orb. (Word8.toLarge x6 .<<. 0w8) .orb. Word8.toLarge x7
fun constructX (x0, x1, x2, x3, x4, x5, x6, x7) = (Word8.toLargeX x0 .<<. 0w56) .orb. (Word8.toLarge x1 .<<. 0w48) .orb. (Word8.toLarge x2 .<<. 0w40) .orb. (Word8.toLarge x3 .<<. 0w32) .orb. (Word8.toLarge x4 .<<. 0w24) .orb. (Word8.toLarge x5 .<<. 0w16) .orb. (Word8.toLarge x6 .<<. 0w8) .orb. Word8.toLarge x7
fun subVec (v, i) = let val ii = checkRange (v, i, bytesPerElem)
                        val x0 = UnsafeWord8Vector.sub (v, ii)
                        val x1 = UnsafeWord8Vector.sub (v, ii + 1)
                        val x2 = UnsafeWord8Vector.sub (v, ii + 2)
                        val x3 = UnsafeWord8Vector.sub (v, ii + 3)
                        val x4 = UnsafeWord8Vector.sub (v, ii + 4)
                        val x5 = UnsafeWord8Vector.sub (v, ii + 5)
                        val x6 = UnsafeWord8Vector.sub (v, ii + 6)
                        val x7 = UnsafeWord8Vector.sub (v, ii + 7)
                    in construct (x0, x1, x2, x3, x4, x5, x6, x7)
                    end
fun subVecX (v, i) = let val ii = checkRange (v, i, bytesPerElem)
                         val x0 = UnsafeWord8Vector.sub (v, ii)
                         val x1 = UnsafeWord8Vector.sub (v, ii + 1)
                         val x2 = UnsafeWord8Vector.sub (v, ii + 2)
                         val x3 = UnsafeWord8Vector.sub (v, ii + 3)
                         val x4 = UnsafeWord8Vector.sub (v, ii + 4)
                         val x5 = UnsafeWord8Vector.sub (v, ii + 5)
                         val x6 = UnsafeWord8Vector.sub (v, ii + 6)
                         val x7 = UnsafeWord8Vector.sub (v, ii + 7)
                     in constructX (x0, x1, x2, x3, x4, x5, x6, x7)
                     end
fun subArr (a, i) = let val ii = checkRangeA (a, i, bytesPerElem)
                        val x0 = UnsafeWord8Array.sub (a, ii)
                        val x1 = UnsafeWord8Array.sub (a, ii + 1)
                        val x2 = UnsafeWord8Array.sub (a, ii + 2)
                        val x3 = UnsafeWord8Array.sub (a, ii + 3)
                        val x4 = UnsafeWord8Array.sub (a, ii + 4)
                        val x5 = UnsafeWord8Array.sub (a, ii + 5)
                        val x6 = UnsafeWord8Array.sub (a, ii + 6)
                        val x7 = UnsafeWord8Array.sub (a, ii + 7)
                    in construct (x0, x1, x2, x3, x4, x5, x6, x7)
                    end
fun subArrX (a, i) = let val ii = checkRangeA (a, i, bytesPerElem)
                         val x0 = UnsafeWord8Array.sub (a, ii)
                         val x1 = UnsafeWord8Array.sub (a, ii + 1)
                         val x2 = UnsafeWord8Array.sub (a, ii + 2)
                         val x3 = UnsafeWord8Array.sub (a, ii + 3)
                         val x4 = UnsafeWord8Array.sub (a, ii + 4)
                         val x5 = UnsafeWord8Array.sub (a, ii + 5)
                         val x6 = UnsafeWord8Array.sub (a, ii + 6)
                         val x7 = UnsafeWord8Array.sub (a, ii + 7)
                     in constructX (x0, x1, x2, x3, x4, x5, x6, x7)
                     end
fun update (a, i, w) = let val ii = checkRangeA (a, i, bytesPerElem)
                       in UnsafeWord8Array.update (a, ii, Word8.fromLarge w)
                        ; UnsafeWord8Array.update (a, ii + 1, Word8.fromLarge (w .>>. 0w8))
                        ; UnsafeWord8Array.update (a, ii + 2, Word8.fromLarge (w .>>. 0w16))
                        ; UnsafeWord8Array.update (a, ii + 3, Word8.fromLarge (w .>>. 0w24))
                        ; UnsafeWord8Array.update (a, ii + 4, Word8.fromLarge (w .>>. 0w32))
                        ; UnsafeWord8Array.update (a, ii + 5, Word8.fromLarge (w .>>. 0w40))
                        ; UnsafeWord8Array.update (a, ii + 6, Word8.fromLarge (w .>>. 0w48))
                        ; UnsafeWord8Array.update (a, ii + 7, Word8.fromLarge (w .>>. 0w56))
                       end
end

end; (* local *)
