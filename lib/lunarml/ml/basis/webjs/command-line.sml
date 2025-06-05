structure CommandLine : sig
              val name : unit -> string
              val arguments : unit -> string list
          end = struct
fun name () = ""
fun arguments (): string list = []
end;
