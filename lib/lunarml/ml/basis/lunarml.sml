structure LunarML : sig
              val assumeDiscardable : ('a -> 'b) -> 'a -> 'b
          end = struct
fun assumeDiscardable f x = _primCall "assumeDiscardable" (f, x)
end;
