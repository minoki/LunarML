structure OS :> sig
              structure FileSys : sig
                            val chDir : string -> unit (* requires LuaFileSystem *)
                            val getDir : unit -> string (* requires LuaFileSystem *)
                            val mkDir : string -> unit (* requires LuaFileSystem *)
                            val rmDir : string -> unit (* requires LuaFileSystem *)
                            val isDir : string -> bool (* requires LuaFileSystem *)
                            val isLink : string -> bool (* requires LuaFileSystem *)
                            val readLink : string -> string (* requires LuaFileSystem 1.7.0 or later *)
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
local
    val oslib = LunarML.assumeDiscardable (Lua.global "os")
    val os_execute = LunarML.assumeDiscardable (Lua.field (oslib, "execute"))
    val os_exit = LunarML.assumeDiscardable (Lua.field (oslib, "exit"))
    val os_getenv = LunarML.assumeDiscardable (Lua.field (oslib, "getenv"))
    val os_remove = LunarML.assumeDiscardable (Lua.field (oslib, "remove"))
    val os_rename = LunarML.assumeDiscardable (Lua.field (oslib, "rename"))
in
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
val chDir : string -> unit = LunarML.assumeDiscardable (use_lfs ("chdir", fn lfs_chdir =>
                                                                             fn path => let val results = Lua.call lfs_chdir #[Lua.fromString path]
                                                                                        in if Lua.isFalsy (Vector.sub (results, 0)) then
                                                                                               let val message = Lua.checkString (Vector.sub (results, 1))
                                                                                               in raise SysErr (message, SOME message)
                                                                                               end
                                                                                           else
                                                                                               ()
                                                                                        end
                                                                )
                                                       )
val getDir : unit -> string = LunarML.assumeDiscardable (use_lfs ("currentdir", fn lfs_currentdir =>
                                                                                   fn () => let val results = Lua.call lfs_currentdir #[]
                                                                                                val r0 = Vector.sub (results, 0)
                                                                                            in if Lua.typeof r0 = "string" then
                                                                                                   Lua.unsafeFromValue r0
                                                                                               else
                                                                                                   let val message = Lua.checkString (Vector.sub (results, 1))
                                                                                                   in raise SysErr (message, SOME message)
                                                                                                   end
                                                                                            end
                                                                 )
                                                        )
val mkDir : string -> unit = LunarML.assumeDiscardable (use_lfs ("mkdir", fn lfs_mkdir =>
                                                                             fn path => let val results = Lua.call lfs_mkdir #[Lua.fromString path]
                                                                                        in if Lua.isFalsy (Vector.sub (results, 0)) then
                                                                                               let val message = Lua.checkString (Vector.sub (results, 1))
                                                                                               in raise SysErr (message, SOME message)
                                                                                               end
                                                                                           else
                                                                                               ()
                                                                                        end
                                                                )
                                                       )
val rmDir : string -> unit = LunarML.assumeDiscardable (use_lfs ("rmdir", fn lfs_rmdir =>
                                                                             fn path => let val results = Lua.call lfs_rmdir #[Lua.fromString path]
                                                                                        in if Lua.isFalsy (Vector.sub (results, 0)) then
                                                                                               let val message = Lua.checkString (Vector.sub (results, 1))
                                                                                               in raise SysErr (message, SOME message)
                                                                                               end
                                                                                           else
                                                                                               ()
                                                                                        end
                                                                )
                                                       )
val isDir : string -> bool = LunarML.assumeDiscardable (use_lfs ("attributes", fn lfs_attributes =>
                                                                                  fn path => let val results = Lua.call lfs_attributes #[Lua.fromString path, Lua.fromString "mode"]
                                                                                                 val r0 = Vector.sub (results, 0)
                                                                                             in if Lua.isFalsy r0 then
                                                                                                    let val message = Lua.checkString (Vector.sub (results, 1))
                                                                                                    in raise SysErr (message, SOME message)
                                                                                                    end
                                                                                                else
                                                                                                    Lua.== (r0, Lua.fromString "directory")
                                                                                             end
                                                                )
                                                       )
val isLink : string -> bool = LunarML.assumeDiscardable (use_lfs ("symlinkattributes", fn lfs_symlinkattributes =>
                                                                                          fn path => let val results = Lua.call lfs_symlinkattributes #[Lua.fromString path, Lua.fromString "mode"]
                                                                                                         val r0 = Vector.sub (results, 0)
                                                                                                     in if Lua.isFalsy r0 then
                                                                                                            let val message = Lua.checkString (Vector.sub (results, 1))
                                                                                                            in raise SysErr (message, SOME message)
                                                                                                            end
                                                                                                        else
                                                                                                            Lua.== (r0, Lua.fromString "link")
                                                                                                     end
                                                                 )
                                                        )
val readLink : string -> string = LunarML.assumeDiscardable (use_lfs ("symlinkattributes", fn lfs_symlinkattributes =>
                                                                                              fn path => let val results = Lua.call lfs_symlinkattributes #[Lua.fromString path, Lua.fromString "target"]
                                                                                                             val r0 = Vector.sub (results, 0)
                                                                                                         in if Lua.isFalsy r0 then
                                                                                                                let val message = Lua.checkString (Vector.sub (results, 1))
                                                                                                                in raise SysErr (message, SOME message)
                                                                                                                end
                                                                                                            else
                                                                                                                Lua.checkString r0
                                                                                                         end
                                                                     )
                                                            )
(* fullPath, realPath *)
(*
val modTime : string -> Time.time = LunarML.assumeDiscardable (use_lfs ("attributes", fn lfs_attributes =>
                                                                                  fn path => let val results = Lua.call lfs_attributes #[Lua.fromString path, Lua.fromString "modification"]
                                                                                                 val r0 = Vector.sub (results, 0)
                                                                                             in if Lua.isFalsy r0 then
                                                                                                    let val message = Lua.checkString (Vector.sub (results, 1))
                                                                                                    in raise SysErr (message, SOME message)
                                                                                                    end
                                                                                                else
                                                                                                    raise Fail "modTime: not implemented yet"
                                                                                             end
                                                                       )
                                                              )
val fileSize : string -> Position.int = LunarML.assumeDiscardable (use_lfs ("attributes", fn lfs_attributes =>
                                                                                             fn path => let val results = Lua.call lfs_attributes #[Lua.fromString path, Lua.fromString "size"]
                                                                                                            val r0 = Vector.sub (results, 0)
                                                                                                        in if Lua.isFalsy r0 then
                                                                                                               let val message = Lua.checkString (Vector.sub (results, 1))
                                                                                                               in raise SysErr (message, SOME message)
                                                                                                               end
                                                                                                           else
                                                                                                               raise Fail "fileSize: not implemented yet"
                                                                                                        end
                                                                           )
                                                                  )
val setTime : string * Time.time option -> unit = LunarML.assumeDiscardable (use_lfs ("touch", fn lfs_touch =>
                                                                                                  (fn (path, NONE) => let val results = Lua.call lfs_touch #[Lua.fromString path]
                                                                                                                          val r0 = Vector.sub (results, 0)
                                                                                                                      in if Lua.isFalsy r0 then
                                                                                                                             let val message = Lua.checkString (Vector.sub (results, 1))
                                                                                                                             in raise SysErr (message, SOME message)
                                                                                                                             end
                                                                                                                         else
                                                                                                                             ()
                                                                                                                      end
                                                                                                  | (path, SOME t) => let val results = Lua.call lfs_touch #[Lua.fromString path, (* TODO *) t, (* TODO *) t]
                                                                                                                          val r0 = Vector.sub (results, 0)
                                                                                                                      in if Lua.isFalsy r0 then
                                                                                                                             let val message = Lua.checkString (Vector.sub (results, 1))
                                                                                                                             in raise SysErr (message, SOME message)
                                                                                                                             end
                                                                                                                         else
                                                                                                                             ()
                                                                                                                      end
                                                                                                  )
                                                                                     )
                                                                            )
*)
val remove : string -> unit = fn filename => ignore (Lua.call os_remove #[Lua.fromString filename])
val rename : {old : string, new : string} -> unit = fn {old, new} => ignore (Lua.call os_rename #[Lua.fromString old, Lua.fromString new])
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
                          "" :: xs => { isAbs = true, vol = "", arcs = xs }
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
                              | go (revAcc, []) = raise Path
                        in go ([], arcs)
                        end
fun joinDirFile { dir, file } = let val { isAbs, vol, arcs } = fromString dir
                                in toString { isAbs = isAbs, vol = vol, arcs = arcs @ [file] }
                                end
val dir = #dir o splitDirFile
val file = #file o splitDirFile
local
    fun go (revArcs, []) = String.concatWith "/" (List.rev revArcs)
      | go (_ :: revArcs, #"." :: #"." :: #"/" :: xs) = go (revArcs, xs)
      | go (revArcs, #"." :: #"/" :: xs) = go (revArcs, xs)
      | go (revArcs, #"/" :: xs) = go (revArcs, xs)
      | go (revArcs, xs) = let val (arc, rest) = takeArc ([], xs)
                           in go (arc :: revArcs, rest)
                           end
    and takeArc (acc, #"/" :: xs) = (String.implode (List.rev acc), xs)
      | takeArc (acc, x :: xs) = takeArc (x :: acc, xs)
      | takeArc (acc, xs as []) = (String.implode (List.rev acc), xs)
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
fun mkRelative { path, relativeTo } = if isRelative path then
                                          path
                                      else if isRelative relativeTo then
                                          raise Path
                                      else
                                          let val abs = mkCanonical relativeTo
                                          in if path = abs then
                                                 currentArc
                                             else
                                                 let fun stripCommonPrefix (xs, ys) = case (Substring.getc xs, Substring.getc ys) of
                                                                                          (SOME (x, xs'), SOME (y, ys')) => if x = y then
                                                                                                                                stripCommonPrefix (xs', ys')
                                                                                                                            else
                                                                                                                                (xs, ys)
                                                                                        | (_, _) => (xs, ys)
                                                     val (path', abs') = stripCommonPrefix (Substring.full path, Substring.full abs)
                                                     val abs'' = String.fields (fn c => c = #"/") (Substring.string abs')
                                                 in
                                                     case abs'' of
                                                         [""] => Substring.string path'
                                                       | xs => String.concatWith "/" (List.map (fn _ => "..") xs) ^ "/" ^ path
                                                 end
                                          end
end
structure Process = struct
type status = int
val success : status = 0
val failure : status = 1
val isSuccess : status -> bool = fn 0 => true | _ => false
val system : string -> status = fn command => let val result = Lua.call os_execute #[Lua.fromString command]
                                              in failure (* TODO *)
                                              end
(* val atExit : (unit -> unit) -> unit *)
val exit : status -> 'a = fn status => let val result = Lua.call os_exit #[Lua.fromInt status, Lua.fromBool true]
                                       in raise Fail "os.exit not available"
                                       end
val terminate : status -> 'a = fn status => let val result = Lua.call os_exit #[Lua.fromInt status, Lua.fromBool false]
                                            in raise Fail "os.exit not available"
                                            end
val getEnv : string -> string option = fn name => let val result = Lua.call os_getenv #[Lua.fromString name]
                                                  in if Lua.isNil (Vector.sub (result, 0)) then
                                                         NONE
                                                     else
                                                         SOME (Lua.unsafeFromValue (Vector.sub (result, 0)))
                                                  end
(* val sleep : Time.time -> unit : LuaSocket's socket.sleep or luaposix's posix.time.nanosleep or use native API (nanosleep or Sleep) via FFI or system command via os.execute or busy loop *)
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
