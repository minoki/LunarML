structure LunarML = struct
open LunarML
structure DelimCont : sig
              type 'a prompt_tag
              type ('a,'b) subcont
              val supportsMultishot : bool
              val newPromptTag : unit -> 'a prompt_tag
              val pushPrompt : 'a prompt_tag * (unit -> 'a) -> 'a
              val withSubCont : 'b prompt_tag * (('a,'b) subcont -> 'b) -> 'a
              val pushSubCont : ('a,'b) subcont * (unit -> 'a) -> 'b
              val shift : 'a prompt_tag * (('b -> 'a) -> 'a) -> 'b
              val control : 'a prompt_tag * (('b -> 'a) -> 'a) -> 'b
              val abort : 'a prompt_tag * 'a -> 'b
              val topLevel : unit prompt_tag
          end = struct
datatype prompt_tag = datatype _Prim.DelimCont.prompt_tag
datatype subcont = datatype _Prim.DelimCont.subcont
val supportsMultishot = true
fun newPromptTag () = _primCall "DelimCont.newPromptTag" ()
fun pushPrompt (p, f) = _primCall "DelimCont.pushPrompt" (p, f)
fun withSubCont (p, f) = _primCall "DelimCont.withSubCont" (p, f)
fun pushSubCont (sk, f) = _primCall "DelimCont.pushSubCont" (sk, f)
fun reifyP (p : 'a prompt_tag, sk : ('b,'a) subcont) : 'b -> 'a = fn v => pushPrompt (p, fn () => pushSubCont (sk, fn () => v))
fun shift (p : 'a prompt_tag, f : ('b -> 'a) -> 'a) : 'b = withSubCont (p, fn sk : ('b,'a) subcont => pushPrompt (p, fn () => f (reifyP (p, sk))))
fun control (p : 'a prompt_tag, f : ('b -> 'a) -> 'a) : 'b = withSubCont (p, fn sk : ('b,'a) subcont => pushPrompt (p, fn () => f (fn v : 'b => pushSubCont (sk, fn () => v))))
fun abort (p, x) = withSubCont (p, fn _ => x)
val topLevel = _Prim.DelimCont.topLevel
end
end;
