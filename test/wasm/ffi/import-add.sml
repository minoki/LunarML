(* Test: import a Wasm function that adds two int32 values *)
structure export =
struct
  val add = _wasmImportFunction "math" "add" : Int32.int * Int32.int -> Int32.int
  fun callAdd (x : Int32.int, y : Int32.int) : Int32.int = add (x, y)
end
