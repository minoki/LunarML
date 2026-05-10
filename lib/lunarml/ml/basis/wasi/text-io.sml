structure TextIO = struct
  local
    (* layout:
     * 0x0000: iov[0].buffer
     * 0x0004: iov[0].length
     * 0x0008: result
     * 0x0100..<0x0200: buffer
     *)
    val iovBufferPtr = WasmMemory.ofWord32 0w0
    val iovLengthPtr = WasmMemory.ofWord32 0w4
    val resultPtr = WasmMemory.ofWord32 0w8
    val bufferPtr = WasmMemory.ofWord32 0wx100
    val bufferCapacity : Word32.word = 0wx100
    val fd_write = _wasmImportFunction "wasi_snapshot_preview1" "fd_write"
      : int * WasmMemory.ptr * int * WasmMemory.ptr -> int;
  in
    fun print s =
      let val n = String.size s
          fun flush bufferSize =
            let val () = WasmMemory.storeWord32 (iovBufferPtr, WasmMemory.toWord32 bufferPtr);
                val () = WasmMemory.storeWord32 (iovLengthPtr, bufferSize);
                val _ = fd_write ((* stdout *) 1, iovBufferPtr, 1, resultPtr)
                (* For now, ignore the error *)
            in ()
            end
          fun go (i, bufferIndex) =
            if i = n then
              flush bufferIndex
            else
              let val bufferIndex =
                    if bufferIndex = bufferCapacity then
                      (flush bufferIndex; 0w0)
                    else
                      bufferIndex
                  val c = _primCall "Unsafe.CharVector.sub" (s, i)
                  val bufferPtr = WasmMemory.add (bufferPtr, bufferIndex)
                  val () = WasmMemory.storeChar8 (bufferPtr, c)
              in go (i + 1, bufferIndex + 0w1)
              end
      in go (0, 0w0)
      end
  end
end;
