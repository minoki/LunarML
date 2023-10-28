structure OS :> sig
              structure FileSys : sig
                            val chDir : string -> unit (* requires LuaFileSystem *)
                            val getDir : unit -> string (* requires LuaFileSystem *)
                            val mkDir : string -> unit (* requires LuaFileSystem *)
                            val rmDir : string -> unit (* requires LuaFileSystem *)
                            val isDir : string -> bool (* requires LuaFileSystem *)
                            val isLink : string -> bool (* requires LuaFileSystem *)
                            val readLink : string -> string (* requires LuaFileSystem 1.7.0 or later *)
                            val modTime : string -> Time.time (* requires LuaFileSystem *)
                            val fileSize : string -> Position.int (* requires LuaFileSystem *)
                            val setTime : string * Time.time option -> unit (* requires LuaFileSystem *)
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
structure FileSys = struct
(*
type dirstream
val openDir : string -> dirstream : lfs.dir?
val readDir : dirstream -> string open : dir_obj:next()
val rewindDir : dirstream -> unit
val closeDir : dirstream -> unit : dir_obj:close()
val fullPath : string -> string
val realPath : string -> string
datatype access_mode = A_READ | A_WRITE | A_EXEC
val access : string * access_mode list -> bool
val tmpName : unit -> string : os.tmpname
eqtype file_id
val fileId : string -> file_id : lfs.attributes "ino"?
val hash : file_id -> word
val compare : file_id * file_id -> order
*)
fun use_lfs (field, f : Lua.value -> 'a -> 'b) = case Lua.Lib.lfs of
                                                     SOME lfs => f (Lua.field (lfs, field))
                                                   | NONE => fn _ => raise SysErr ("LuaFileSystem not available", NONE)
val chDir : string -> unit = LunarML.assumeDiscardable use_lfs ("chdir", fn lfs_chdir =>
                                                                            fn path => let val (ok, message) = Lua.call2 lfs_chdir #[Lua.fromString path]
                                                                                       in if Lua.isFalsy ok then
                                                                                              let val message = Lua.checkString message
                                                                                              in raise SysErr (message, SOME message)
                                                                                              end
                                                                                          else
                                                                                              ()
                                                                                       end
                                                               )
val getDir : unit -> string = LunarML.assumeDiscardable use_lfs ("currentdir", fn lfs_currentdir =>
                                                                                  fn () => let val (r0, message) = Lua.call2 lfs_currentdir #[]
                                                                                           in if Lua.typeof r0 = "string" then
                                                                                                  Lua.unsafeFromValue r0
                                                                                              else
                                                                                                  let val message = Lua.checkString message
                                                                                                  in raise SysErr (message, SOME message)
                                                                                                  end
                                                                                           end
                                                                )
val mkDir : string -> unit = LunarML.assumeDiscardable use_lfs ("mkdir", fn lfs_mkdir =>
                                                                            fn path => let val (ok, message) = Lua.call2 lfs_mkdir #[Lua.fromString path]
                                                                                       in if Lua.isFalsy ok then
                                                                                              let val message = Lua.checkString message
                                                                                              in raise SysErr (message, SOME message)
                                                                                              end
                                                                                          else
                                                                                              ()
                                                                                       end
                                                               )
val rmDir : string -> unit = LunarML.assumeDiscardable use_lfs ("rmdir", fn lfs_rmdir =>
                                                                            fn path => let val (ok, message) = Lua.call2 lfs_rmdir #[Lua.fromString path]
                                                                                       in if Lua.isFalsy ok then
                                                                                              let val message = Lua.checkString message
                                                                                              in raise SysErr (message, SOME message)
                                                                                              end
                                                                                          else
                                                                                              ()
                                                                                       end
                                                               )
val isDir : string -> bool = LunarML.assumeDiscardable use_lfs ("attributes", fn lfs_attributes =>
                                                                                 fn path => let val (r0, message) = Lua.call2 lfs_attributes #[Lua.fromString path, Lua.fromString "mode"]
                                                                                            in if Lua.isFalsy r0 then
                                                                                                   let val message = Lua.checkString message
                                                                                                   in raise SysErr (message, SOME message)
                                                                                                   end
                                                                                               else
                                                                                                   Lua.== (r0, Lua.fromString "directory")
                                                                                            end
                                                               )
val isLink : string -> bool = LunarML.assumeDiscardable use_lfs ("symlinkattributes", fn lfs_symlinkattributes =>
                                                                                         fn path => let val (r0, message) = Lua.call2 lfs_symlinkattributes #[Lua.fromString path, Lua.fromString "mode"]
                                                                                                    in if Lua.isFalsy r0 then
                                                                                                           let val message = Lua.checkString message
                                                                                                           in raise SysErr (message, SOME message)
                                                                                                           end
                                                                                                       else
                                                                                                           Lua.== (r0, Lua.fromString "link")
                                                                                                    end
                                                                )
val readLink : string -> string = LunarML.assumeDiscardable use_lfs ("symlinkattributes", fn lfs_symlinkattributes =>
                                                                                             fn path => let val (r0, message) = Lua.call2 lfs_symlinkattributes #[Lua.fromString path, Lua.fromString "target"]
                                                                                                        in if Lua.isFalsy r0 then
                                                                                                               let val message = Lua.checkString message
                                                                                                               in raise SysErr (message, SOME message)
                                                                                                               end
                                                                                                           else
                                                                                                               Lua.checkString r0
                                                                                                        end
                                                                    )
(* fullPath, realPath *)
val modTime : string -> Time.time = LunarML.assumeDiscardable use_lfs ("attributes", fn lfs_attributes =>
                                                                                 fn path => let val (r0, message) = Lua.call2 lfs_attributes #[Lua.fromString path, Lua.fromString "modification"]
                                                                                            in if Lua.isFalsy r0 then
                                                                                                   let val message = Lua.checkString message
                                                                                                   in raise SysErr (message, SOME message)
                                                                                                   end
                                                                                               else
                                                                                                   TimeImpl.fromLuaTime r0
                                                                                            end
                                                                      )
val fileSize : string -> Position.int = LunarML.assumeDiscardable use_lfs ("attributes", fn lfs_attributes =>
                                                                                            fn path => let val (r0, message) = Lua.call2 lfs_attributes #[Lua.fromString path, Lua.fromString "size"]
                                                                                                       in if Lua.isFalsy r0 then
                                                                                                              let val message = Lua.checkString message
                                                                                                              in raise SysErr (message, SOME message)
                                                                                                              end
                                                                                                          else
                                                                                                              Lua.unsafeFromValue r0
                                                                                                       end
                                                                          )
val setTime : string * Time.time option -> unit = LunarML.assumeDiscardable use_lfs ("touch", fn lfs_touch =>
                                                                                                 (fn (path, NONE) => let val (r0, message) = Lua.call2 lfs_touch #[Lua.fromString path]
                                                                                                                     in if Lua.isFalsy r0 then
                                                                                                                            let val message = Lua.checkString message
                                                                                                                            in raise SysErr (message, SOME message)
                                                                                                                            end
                                                                                                                        else
                                                                                                                            ()
                                                                                                                     end
                                                                                                 | (path, SOME t) => let val u = TimeImpl.toLuaTime t
                                                                                                                         val (r0, message) = Lua.call2 lfs_touch #[Lua.fromString path, u, u]
                                                                                                                     in if Lua.isFalsy r0 then
                                                                                                                            let val message = Lua.checkString message
                                                                                                                            in raise SysErr (message, SOME message)
                                                                                                                            end
                                                                                                                        else
                                                                                                                            ()
                                                                                                                     end
                                                                                                 )
                                                                                    )
val remove : string -> unit = fn filename => Lua.call0 Lua.Lib.os.remove #[Lua.fromString filename]
val rename : { old : string, new : string } -> unit = fn { old, new } => Lua.call0 Lua.Lib.os.rename #[Lua.fromString old, Lua.fromString new]
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
val system : string -> status = fn command => let val () = Lua.call0 Lua.Lib.os.execute #[Lua.fromString command]
                                              in failure (* TODO *)
                                              end
(* val atExit : (unit -> unit) -> unit *)
val exit : status -> 'a = fn status => let val () = Lua.call0 Lua.Lib.os.exit #[Lua.fromInt status, Lua.fromBool true]
                                       in _primCall "unreachable" ()
                                       end
val terminate : status -> 'a = fn status => let val () = Lua.call0 Lua.Lib.os.exit #[Lua.fromInt status, Lua.fromBool false]
                                            in _primCall "unreachable" ()
                                            end
val getEnv : string -> string option = fn name => let val result = Lua.call1 Lua.Lib.os.getenv #[Lua.fromString name]
                                                  in if Lua.isNil result then
                                                         NONE
                                                     else
                                                         SOME (Lua.unsafeFromValue result : string)
                                                  end
(* val sleep : Time.time -> unit : LuaSocket's socket.sleep or luaposix's posix.time.nanosleep or use native API (nanosleep or Sleep) via FFI or system command via os.execute or busy loop *)
end (* structure Process *)
(*
eqtype syserror
exception SysErr of string * syserror option
val errorMsg : syserror -> string
val errorName : syserror -> string
val syserror : string -> syserror option
*)
end; (* structure OS *)
