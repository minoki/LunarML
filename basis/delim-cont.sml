structure DelimCont : sig
              type 'a prompt
              type ('a,'b) subcont
              val newPrompt : unit -> 'a prompt
              val pushPrompt : 'a prompt * (unit -> 'a) -> 'a
              val withSubCont : 'b prompt * (('a,'b) subcont -> 'b) -> 'a
              val pushSubCont : ('a,'b) subcont * (unit -> 'a) -> 'b
              val abort : 'b prompt * 'b -> 'a
          end = struct
datatype prompt = datatype _Prim.DelimCont.prompt
datatype subcont = datatype _Prim.DelimCont.subcont
fun newPrompt () = _primCall "DelimCont.newPrompt" ()
fun pushPrompt (p, f) = _primCall "DelimCont.pushPrompt" (p, f)
fun withSubCont (p, f) = _primCall "DelimCont.withSubCont" (p, f)
fun pushSubCont (sc, f) = _primCall "DelimCont.pushSubCont" (sc, f)
fun abort (p, x) = withSubCont (p, fn _ => x)
end;
