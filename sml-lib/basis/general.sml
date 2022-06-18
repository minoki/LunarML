structure General : sig
              type unit = {}
              type exn = exn
              exception Bind
              exception Match
              exception Chr
              exception Div
              exception Domain
              exception Fail of string
              exception Overflow
              exception Size
              exception Span
              exception Subscript
              val exnName : exn -> string
              datatype order = LESS | EQUAL | GREATER
              val ! : 'a ref -> 'a
              val := : 'a ref * 'a -> unit
              val before : 'a * unit -> 'a
              val ignore : 'a -> unit
              val o : ('b -> 'c) * ('a -> 'b) -> 'a -> 'c
          end = struct
type unit = {}
type exn = exn
exception Bind = Bind
exception Match = Match
exception Chr
exception Div = Div
exception Domain
exception Fail = Fail
exception Overflow = Overflow
exception Size = Size
exception Span
exception Subscript = Subscript
val exnName = exnName
(*
val exnMessage : exn -> string
*)
datatype order = LESS | EQUAL | GREATER
fun ! (ref x) = x
fun x := y = _primCall "Ref.:=" (x, y)
fun x before () = x
fun ignore _ = ()
fun (f o g) x = f (g x)
end (* structure General *)
open General;
