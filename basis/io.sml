structure IO : sig
              exception Io of { name : string
                              , function : string
                              , cause : exn
                              }
          end = struct
exception Io of { name : string
                , function : string
                , cause : exn
                }
end; (* structure IO *)
