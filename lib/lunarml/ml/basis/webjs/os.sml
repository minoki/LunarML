structure OS :> sig
              structure FileSys : sig end
              structure IO : sig
                            eqtype iodesc
                        end
              structure Path : OS_PATH
              structure Process : sig end
              eqtype syserror
              exception SysErr of string * syserror option
          end = struct
type syserror = string
exception SysErr of string * syserror option
structure FileSys = struct
end (* structure FileSys *)
structure IO = struct
type iodesc = int (* tentative *)
end
structure Path = UnixPath (exception Path
                           exception InvalidArc
                          )
structure Process = struct
(* val sleep : Time.time -> unit *)
end (* structure Process *)
end; (* structure OS *)
