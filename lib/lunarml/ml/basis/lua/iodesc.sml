structure IODesc :> sig
              eqtype iodesc
              val hash : iodesc -> word
              val compare : iodesc * iodesc -> order
              val toDesc : Lua.value -> iodesc (* The value must not be nil *)
              val fromDesc : iodesc -> Lua.value
              val release : iodesc -> unit
          end = struct
type iodesc = int
val hash = Word.fromInt
val compare = Int.compare
val objToDescMap = let val meta = Lua.newTableWith #[("__mode", Lua.fromString "k")]
                    in Lua.newTableWithMetatable (#[], meta)
                    end (* key: object, value: int *)
val descToObjMap = let val meta = Lua.newTableWith #[("__mode", Lua.fromString "v")]
                   in Lua.newTableWithMetatable (#[], meta)
                   end (* key: int, value: object *)
val freeList : (int * int list) ref = ref (0, [])
fun newDesc () : int = case !freeList of
                           (n, []) => ( freeList := (n + 1, [])
                                      ; n
                                      )
                         | (n, i :: is) => ( freeList := (n, is)
                                           ; i
                                           )
fun toDesc obj = let val v = Lua.sub (objToDescMap, obj)
                 in if Lua.isNil v then
                        let val d = newDesc ()
                        in Lua.set (objToDescMap, obj, Lua.fromInt d)
                         ; Lua.set (descToObjMap, Lua.fromInt d, obj)
                         ; d
                        end
                    else
                        Lua.unsafeFromValue v : int
                 end
fun fromDesc i = Lua.sub (descToObjMap, Lua.fromInt i)
fun release i = let val obj = Lua.sub (descToObjMap, Lua.fromInt i)
                in if not (Lua.isNil obj) then
                       let val (n, is) = !freeList
                           val () = freeList := (n, i :: is)
                       in Lua.set (descToObjMap, Lua.fromInt i, Lua.NIL)
                        ; Lua.set (objToDescMap, obj, Lua.NIL)
                        ; ()
                       end
                   else
                       ()
                end
end; (* structure IODesc *)
