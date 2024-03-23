structure IODesc :> sig
              eqtype iodesc
              val hash : iodesc -> word
              val compare : iodesc * iodesc -> order
              val toDesc : JavaScript.value -> iodesc
              val fromDesc : iodesc -> JavaScript.value
              val release : iodesc -> unit
          end = struct
type iodesc = int
val hash = Word.fromInt
val compare = Int.compare
val objToDescMap = LunarML.assumeDiscardable (fn () => JavaScript.new JavaScript.Lib.WeakMap #[]) () (* key: object, value: int *)
val descToObjMap = LunarML.assumeDiscardable (fn () => JavaScript.new JavaScript.Lib.Map #[]) () (* key: int, value: object *)
val freeList : (int * int list) ref = ref (0, [])
fun newDesc () : int = case !freeList of
                           (n, []) => ( freeList := (n + 1, [])
                                      ; n
                                      )
                         | (n, i :: is) => ( freeList := (n, is)
                                           ; i
                                           )
fun toDesc obj = let val v = JavaScript.method (objToDescMap, "get") #[obj]
                 in if JavaScript.=== (v, JavaScript.undefined) then
                        let val d = newDesc ()
                        in JavaScript.method (objToDescMap, "set") #[obj, JavaScript.fromInt d]
                         ; JavaScript.method (descToObjMap, "set") #[JavaScript.fromInt d, obj]
                         ; d
                        end
                    else
                        JavaScript.unsafeFromValue v : int
                 end
fun fromDesc i = JavaScript.method (descToObjMap, "get") #[JavaScript.fromInt i]
fun release i = if JavaScript.unsafeFromValue (JavaScript.method (descToObjMap, "has") #[JavaScript.fromInt i]) then
                    let val (n, is) = !freeList
                        val () = freeList := (n, i :: is)
                        val obj = JavaScript.method (descToObjMap, "get") #[JavaScript.fromInt i]
                    in JavaScript.method (descToObjMap, "delete") #[JavaScript.fromInt i]
                     ; JavaScript.method (objToDescMap, "delete") #[obj]
                     ; ()
                    end
                else
                    ()
end;
