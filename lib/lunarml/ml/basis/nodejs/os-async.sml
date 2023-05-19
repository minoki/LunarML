structure OS :> sig
              structure FileSys : sig
                            val chDir : string -> unit
                            val getDir : unit -> string
                            val mkDir : string -> unit
                            val rmDir : string -> unit
                            val isDir : string -> bool
                            val isLink : string -> bool
                            val readLink : string -> string
                            val remove : string -> unit
                            val rename : { old : string, new : string } -> unit
                        end
              structure IO : sig
                        end
              structure Path : sig
                            exception Path
                            exception InvalidArc
                            val parentArc : string
                            val currentArc : string
                            val fromString : string -> { isAbs : bool, vol : string, arcs : string list }
                            val toString : { isAbs : bool, vol : string, arcs : string list } -> string
                            val splitDirFile : string -> { dir : string, file : string }
                            val joinDirFile : { dir : string, file : string } -> string
                            val dir : string -> string
                            val file : string -> string
                            val splitBaseExt : string -> { base : string, ext : string option }
                            val joinBaseExt : { base : string, ext : string option } -> string
                            val base : string -> string
                            val ext : string -> string option
                            val mkCanonical : string -> string
                            val mkAbsolute : { path : string, relativeTo : string } -> string
                            val mkRelative : { path : string, relativeTo : string } -> string
                            val isAbsolute : string -> bool
                            val isRelative : string -> bool
                            val concat : string * string -> string
                        end
              structure Process : sig
                            type status
                            val success : status
                            val failure : status
                            val isSuccess : status -> bool
                            val system : string -> status
                            val exit : status -> 'a
                            val terminate : status -> 'a
                            val getEnv : string -> string option
                        end
              eqtype syserror
              exception SysErr of string * syserror option
          end = struct
type syserror = string
exception SysErr of string * syserror option
local val fs = LunarML.assumeDiscardable (fn () => JavaScript.call JavaScript.require #[JavaScript.fromWideString "fs"]) ()
      val process = LunarML.assumeDiscardable (fn () => JavaScript.call JavaScript.require #[JavaScript.fromWideString "process"]) ()
      val child_process = LunarML.assumeDiscardable (fn () => JavaScript.call JavaScript.require #[JavaScript.fromWideString "child_process"]) ()
in
structure FileSys = struct
(*
type dirstream
val openDir : string -> dirstream
val readDir : dirstream -> string open
val rewindDir : dirstream -> unit
val closeDir : dirstream -> unit
val fullPath : string -> string
val realPath : string -> string
datatype access_mode = A_READ | A_WRITE | A_EXEC
val access : string * access_mode list -> bool
val tmpName : unit -> string
eqtype file_id
val fileId : string -> file_id
val hash : file_id -> word
val compare : file_id * file_id -> order
*)
fun chDir (dir : string) = ( JavaScript.method (process, "chdir") #[JavaScript.fromWideString (JavaScript.decodeUtf8 dir)]
                           ; () (* TODO: raise SysErr *)
                           )
fun getDir () : string = JavaScript.encodeUtf8 (JavaScript.unsafeFromValue (JavaScript.method (process, "cwd") #[]))
fun mkDir (path : string) = let val options = JavaScript.newObject ()
                            in JavaScript.set (options, JavaScript.fromWideString "recursive", JavaScript.fromBool true)
                             ; JavaScript.method (fs, "mkdirSync") #[JavaScript.fromWideString (JavaScript.decodeUtf8 path), options]
                             ; ()
                            end
fun rmDir (path : string) = ( JavaScript.method (fs, "rmdirSync") #[JavaScript.fromWideString (JavaScript.decodeUtf8 path)]
                            ; ()
                            )
fun isDir (path : string) = let val stat = JavaScript.method (fs, "statSync") #[JavaScript.fromWideString (JavaScript.decodeUtf8 path)]
                            in JavaScript.unsafeFromValue (JavaScript.method (stat, "isDirectory") #[])
                            end
fun isLink (path : string) = let val stat = JavaScript.method (fs, "lstatSync") #[JavaScript.fromWideString (JavaScript.decodeUtf8 path)]
                             in JavaScript.unsafeFromValue (JavaScript.method (stat, "isSymbolicLink") #[])
                             end
fun readLink (path : string) : string = JavaScript.encodeUtf8 (JavaScript.unsafeFromValue (JavaScript.method (fs, "readlinkSync") #[JavaScript.fromWideString (JavaScript.decodeUtf8 path)]))
(* fullPath, realPath, modTime, fileSize, setTime *)
fun remove (path : string) = ( JavaScript.method (fs, "rmSync") #[JavaScript.fromWideString (JavaScript.decodeUtf8 path)]
                             ; ()
                             )
fun rename {old : string, new : string} = ( JavaScript.method (fs, "renameSync") #[JavaScript.fromWideString (JavaScript.decodeUtf8 old), JavaScript.fromWideString (JavaScript.decodeUtf8 new)]
                                          ; ()
                                          )
end (* structure FileSys *)
structure IO = struct end
structure Path = struct
exception Path
exception InvalidArc
val parentArc = ".."
val currentArc = "."
fun isAbsolute path = if String.isPrefix "/" path then (* TODO: Windows *)
                          true
                      else
                          false
fun isRelative path = not (isAbsolute path)
fun fromString path = case String.fields (fn c => c = #"/") path of (* TODO: Windows *)
                          [""] => { isAbs = false, vol = "", arcs = [] }
                        | "" :: xs => { isAbs = true, vol = "", arcs = xs }
                        | xs => { isAbs = false, vol = "", arcs = xs }
local
    fun isValidArc arc = CharVector.all (fn c => c <> #"/") arc
in
fun toString { isAbs, vol, arcs } = if vol <> "" then
                                        raise Path (* invalid volume *)
                                    else
                                        case (isAbs, arcs) of
                                            (false, "" :: _) => raise Path
                                          | _ => if List.all isValidArc arcs then
                                                     if isAbs then
                                                         "/" ^ String.concatWith "/" arcs
                                                     else
                                                         String.concatWith "/" arcs
                                                 else
                                                     raise InvalidArc
end
fun splitDirFile path = let val { isAbs, vol, arcs } = fromString path
                            fun go (revAcc, [last]) = { dir = toString { isAbs = isAbs, vol = vol, arcs = List.rev revAcc }, file = last }
                              | go (revAcc, x :: xs) = go (x :: revAcc, xs)
                              | go (revAcc, []) = { dir = toString { isAbs = isAbs, vol = vol, arcs = [] }, file = "" }
                        in go ([], arcs)
                        end
fun joinDirFile { dir, file } = let val { isAbs, vol, arcs } = fromString dir
                                in toString { isAbs = isAbs, vol = vol, arcs = arcs @ [file] }
                                end
val dir = #dir o splitDirFile
val file = #file o splitDirFile
fun splitBaseExt path = let val { isAbs, vol, arcs } = fromString path
                            fun go (revAcc, [lastArc]) = let val (l, r) = Substring.splitr (fn c => c <> #".") (Substring.full lastArc)
                                                             val l = Substring.string l
                                                             and r = Substring.string r
                                                             val (base, ext) = case (l, r) of
                                                                                   ("", _) => (lastArc, NONE)
                                                                                 | (_, "") => (lastArc, NONE)
                                                                                 | (".", _) => (lastArc, NONE)
                                                                                 | (base, ext) => (String.substring (base, 0, String.size base - 1), SOME ext)
                                                         in { base = toString { isAbs = isAbs, vol = vol, arcs = List.rev (base :: revAcc) }, ext = ext }
                                                         end
                              | go (revAcc, x :: xs) = go (x :: revAcc, xs)
                              | go (revAcc, []) = { base = toString { isAbs = isAbs, vol = vol, arcs = List.rev revAcc }, ext = NONE }
                        in go ([], arcs)
                        end
fun joinBaseExt { base, ext } = case ext of
                                    NONE => base
                                  | SOME "" => base
                                  | SOME x => base ^ "." ^ x
fun base path = #base (splitBaseExt path)
fun ext path = #ext (splitBaseExt path)
local
    fun go (revArcs, []) = String.concatWith "/" (List.rev revArcs)
      | go ([], #"." :: #"." :: nil) = ""
      | go (_ :: revArcs, #"." :: #"." :: nil) = go (revArcs, [])
      | go (_ :: revArcs, #"." :: #"." :: #"/" :: xs) = go (revArcs, xs)
      | go (revArcs, #"." :: nil) = go (revArcs, [])
      | go (revArcs, #"." :: #"/" :: xs) = go (revArcs, xs)
      | go (revArcs, #"/" :: xs) = go (revArcs, xs)
      | go (revArcs, xs) = let val (arc, rest) = takeArc ([], xs)
                           in go (arc :: revArcs, rest)
                           end
    and takeArc (acc, #"/" :: xs) = (String.implodeRev acc, xs)
      | takeArc (acc, x :: xs) = takeArc (x :: acc, xs)
      | takeArc (acc, xs as []) = (String.implodeRev acc, xs)
in
fun mkCanonical path = case String.explode path of
                           [] => "."
                         | #"/" :: xs => "/" ^ go ([], xs)
                         | xs => go ([], xs)
end
fun concat (path, t) = case (fromString path, fromString t) of
                           (_, { isAbs = true, ... }) => raise Path
                         | ({ isAbs, vol = v1, arcs = arcs1 }, { vol = v2, arcs = arcs2, ... }) => if v2 = "" orelse v1 = v2 then
                                                                                                       toString { isAbs = isAbs, vol = v1, arcs = concatArcs (arcs1, arcs2) }
                                                                                                   else
                                                                                                       raise Path
and concatArcs ([], arcs2) = arcs2
  | concatArcs ([""], arcs2) = arcs2
  | concatArcs (x :: xs, arcs2) = x :: concatArcs (xs, arcs2)
fun mkAbsolute { path, relativeTo } = if isAbsolute path then
                                          path
                                      else
                                          mkCanonical (concat (relativeTo, path))
fun mkRelative { path, relativeTo } = case (fromString path, fromString relativeTo) of
                                          ({ isAbs = false, ... }, _) => path (* path is relative *)
                                        | (_, { isAbs = false, ... }) => raise Path (* relativeTo is relative *)
                                        | ({ isAbs = true, vol = pVol, arcs = pArcs }, { isAbs = true, vol = rVol, arcs = _ }) =>
                                          if pVol <> rVol then
                                              raise Path
                                          else
                                              let val abs = mkCanonical relativeTo
                                              in if path = abs then
                                                     currentArc
                                                 else
                                                     let val rArcs = #arcs (fromString abs)
                                                         fun stripCommonPrefix (xs as (x :: xs'), ys as (y :: ys')) = if x = y then
                                                                                                                          stripCommonPrefix (xs', ys')
                                                                                                                      else
                                                                                                                          (xs, ys)
                                                           | stripCommonPrefix (xs, ys) = (xs, ys)
                                                         val (path', abs') = stripCommonPrefix (pArcs, rArcs)
                                                     in toString { isAbs = false, vol = pVol, arcs = List.map (fn _ => "..") abs' @ (case path' of [""] => [] | _ => path') }
                                                     end
                                              end
end
structure Process = struct
type status = int
val success : status = 0
val failure : status = 1
val isSuccess : status -> bool = fn 0 => true | _ => false
fun system (command : string) = ( JavaScript.method (child_process, "execSync") #[JavaScript.fromWideString (JavaScript.decodeUtf8 command)]
                                ; success (* TODO: catch failures *)
                                )
(* val atExit : (unit -> unit) -> unit *)
fun exit (status : status) : 'a = ( JavaScript.method (process, "exit") #[JavaScript.fromInt status]
                                  ; _primCall "unreachable" ()
                                  )
fun terminate (status : status) : 'a = ( JavaScript.method (process, "exit") #[JavaScript.fromInt status]
                                       ; _primCall "unreachable" ()
                                       )
fun getEnv (name : string) : string option = let val env = JavaScript.field (process, "env")
                                                 val value = JavaScript.field (env, JavaScript.decodeUtf8 name)
                                             in if JavaScript.typeof value = "string" then
                                                    SOME (JavaScript.encodeUtf8 (JavaScript.unsafeFromValue value))
                                                else
                                                    NONE
                                             end
(* val sleep : Time.time -> unit *)
end (* structure Process *)
end (* local *)
(*
eqtype syserror
exception SysErr of string * syserror option
val errorMsg : syserror -> string
val errorName : syserror -> string
val syserror : string -> syserror option
*)
end; (* structure OS *)
