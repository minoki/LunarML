structure LunarML : sig
              val assumePure : 'a -> 'a
              val assumeDiscardable : 'a -> 'a
          end = struct
val assumePure = _Prim.assumePure
val assumeDiscardable = _Prim.assumeDiscardable
end;
