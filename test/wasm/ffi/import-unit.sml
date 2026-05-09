(* Test: import functions with unit and scalar arguments *)
structure export =
struct
  (* arity 0: unit -> int32 *)
  val getAnswer = _wasmImportFunction "env" "getAnswer" : unit -> Int32.int
  (* arity 1: scalar *)
  val negate = _wasmImportFunction "math" "negate" : Int32.int -> Int32.int
  (* arity 3: triple *)
  val clamp = _wasmImportFunction "math" "clamp" : Int32.int * Int32.int * Int32.int -> Int32.int

  fun callGetAnswer () : Int32.int = getAnswer ()
  fun callNegate (x : Int32.int) : Int32.int = negate x
  fun callClamp (x : Int32.int, lo : Int32.int, hi : Int32.int) : Int32.int =
    clamp (x, lo, hi)
end
