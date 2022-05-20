structure LunarML = struct
open LunarML
structure DelimCont : sig
              type 'a prompt
              type ('a,'b) subcont
              val newPrompt : unit -> 'a prompt
              val pushPrompt : 'a prompt * (unit -> 'a) -> 'a
              val withSubCont : 'b prompt * (('a,'b) subcont -> 'b) -> 'a
              val pushSubCont : ('a,'b) subcont * (unit -> 'a) -> 'b
              val shift : 'a prompt * (('b -> 'a) -> 'a) -> 'b
              val control : 'a prompt * (('b -> 'a) -> 'a) -> 'b
              val abort : 'a prompt * 'a -> 'b
              val topLevel : unit prompt
          end = struct
datatype prompt = datatype _Prim.DelimCont.prompt
datatype subcont = datatype _Prim.DelimCont.subcont
fun newPrompt () = _primCall "DelimCont.newPrompt" ()
fun pushPrompt (p, f) = _primCall "DelimCont.pushPrompt" (p, f)
fun withSubCont (p, f) = _primCall "DelimCont.withSubCont" (p, f)
fun pushSubCont (sk, f) = _primCall "DelimCont.pushSubCont" (sk, f)
fun reifyP (p : 'a prompt, sk : ('b,'a) subcont) : 'b -> 'a = fn v => pushPrompt (p, fn () => pushSubCont (sk, fn () => v))
fun shift (p : 'a prompt, f : ('b -> 'a) -> 'a) : 'b = withSubCont (p, fn sk : ('b,'a) subcont => pushPrompt (p, fn () => f (reifyP (p, sk))))
fun control (p : 'a prompt, f : ('b -> 'a) -> 'a) : 'b = withSubCont (p, fn sk : ('b,'a) subcont => pushPrompt (p, fn () => f (fn v : 'b => pushSubCont (sk, fn () => v))))
fun abort (p, x) = withSubCont (p, fn _ => x)
val topLevel = _Prim.DelimCont.topLevel
end
end;
