structure Bool : sig
              datatype bool = datatype bool
              val not : bool -> bool
              val toString : bool -> string
          end = struct
open Bool (* datatype bool, not *)
fun toString (b : bool) : string = Lua.unsafeFromValue (Lua.call1 Lua.Lib.tostring #[Lua.fromBool b])
(* scan, fromString *)
end;
