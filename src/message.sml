(*
 * Copyright (c) 2023 ARATA Mizuki
 * This file is part of LunarML.
 *)
structure Message :> sig
              datatype message_type = WARNING | ERROR
              type message = { spans : SourcePos.span list
                             , domain : string
                             , message : string
                             , type_ : message_type
                             }
              type counter
              type handler
              exception Abort
              val newCounter : { errorTolerance : int } -> counter
              val newHandler : counter * (message -> unit) -> handler
              val warning : handler * SourcePos.span list * string * string -> unit
              val error : handler * SourcePos.span list * string * string -> unit
              val fatalError : handler * SourcePos.span list * string * string -> 'a
              val anyError : counter -> bool
          end = struct
datatype message_type = WARNING | ERROR
type message = { spans : SourcePos.span list
               , domain : string
               , message : string
               , type_ : message_type
               }
type counter = { counter : int ref
               , errorTolerance : int
               }
type handler = { counter : counter
               , handler : message -> unit
               }
exception Abort
fun newCounter { errorTolerance } : counter = { counter = ref 0, errorTolerance = errorTolerance }
fun newHandler (counter, handler) : handler = { counter = counter, handler = handler }
fun warning ({ counter = _, handler } : handler, spans, domain, message)
    = handler { spans = spans, domain = domain, message = message, type_ = WARNING }
fun error ({ counter = { counter, errorTolerance }, handler } : handler, spans, domain, message)
    = let val () = handler { spans = spans, domain = domain, message = message, type_ = ERROR }
          val numberOfErrors = !counter + 1
      in counter := numberOfErrors
       ; if numberOfErrors > errorTolerance then
             raise Abort
         else
             ()
      end
fun fatalError ({ counter = _, handler } : handler, spans, domain, message)
    = ( handler { spans = spans, domain = domain, message = message, type_ = ERROR }
      ; raise Abort
      )
fun anyError ({ counter = ref n, errorTolerance = _ } : counter) = n > 0
end; (* structure Message *)
