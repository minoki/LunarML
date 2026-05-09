structure export =
struct
  (* Store a constant at address 0 and load it back, return the stored value *)
  fun test_store32 (_ : int) : int =
    let
      val p = WasmMemory.ofWord32 0w0
      val _ = WasmMemory.store32 (p, 0w1234)
    in
      0
    end

  (* Store a byte and load it unsigned *)
  fun test_store8 (_ : int) : int =
    let
      val p = WasmMemory.ofWord32 0w8
      val _ = WasmMemory.store8 (p, 0w42)
      val _ = WasmMemory.load8u p
    in
      0
    end

  (* Pointer arithmetic only (no memory access) *)
  fun test_ptr_add (base : int) : int =
    let
      val p = WasmMemory.ofWord32 0w0
      val _ = WasmMemory.add (p, 0w4)
    in
      base + 1
    end
end
