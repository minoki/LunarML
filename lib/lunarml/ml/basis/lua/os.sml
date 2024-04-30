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
                            eqtype iodesc
                            val hash : iodesc -> word
                            val compare : iodesc * iodesc -> order
                        end
              structure Path : OS_PATH
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
          end where type IO.iodesc = IODesc.iodesc = struct
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
structure IO = IODesc
structure Path = struct
exception Path
exception InvalidArc
structure UnixPath = UnixPath (exception Path = Path
                               exception InvalidArc = InvalidArc
                              )
structure WindowsPath = WindowsPath (exception Path = Path
                                     exception InvalidArc = InvalidArc
                                    )
val parentArc = ".."
val currentArc = "."
val { fromString, toString, validVolume, getVolume, getParent, splitDirFile, joinDirFile, dir, file, splitBaseExt, joinBaseExt, base, ext, mkCanonical, isCanonical, mkAbsolute, mkRelative, isAbsolute, isRelative, isRoot, concat, fromUnixPath, toUnixPath }
    = LunarML.assumeDiscardable (fn () =>
                                    let val isWindows = String.sub (Lua.unsafeFromValue (Lua.field (Lua.Lib.package, "config")), 0) = #"\\"
                                    in if isWindows then
                                           { fromString = WindowsPath.fromString, toString = WindowsPath.toString, validVolume = WindowsPath.validVolume, getVolume = WindowsPath.getVolume, getParent = WindowsPath.getParent, splitDirFile = WindowsPath.splitDirFile, joinDirFile = WindowsPath.joinDirFile, dir = WindowsPath.dir, file = WindowsPath.file, splitBaseExt = WindowsPath.splitBaseExt, joinBaseExt = WindowsPath.joinBaseExt, base = WindowsPath.base, ext = WindowsPath.ext, mkCanonical = WindowsPath.mkCanonical, isCanonical = WindowsPath.isCanonical, mkAbsolute = WindowsPath.mkAbsolute, mkRelative = WindowsPath.mkRelative, isAbsolute = WindowsPath.isAbsolute, isRelative = WindowsPath.isRelative, isRoot = WindowsPath.isRoot, concat = WindowsPath.concat, fromUnixPath = WindowsPath.fromUnixPath, toUnixPath = WindowsPath.toUnixPath }
                                       else
                                           { fromString = UnixPath.fromString, toString = UnixPath.toString, validVolume = UnixPath.validVolume, getVolume = UnixPath.getVolume, getParent = UnixPath.getParent, splitDirFile = UnixPath.splitDirFile, joinDirFile = UnixPath.joinDirFile, dir = UnixPath.dir, file = UnixPath.file, splitBaseExt = UnixPath.splitBaseExt, joinBaseExt = UnixPath.joinBaseExt, base = UnixPath.base, ext = UnixPath.ext, mkCanonical = UnixPath.mkCanonical, isCanonical = UnixPath.isCanonical, mkAbsolute = UnixPath.mkAbsolute, mkRelative = UnixPath.mkRelative, isAbsolute = UnixPath.isAbsolute, isRelative = UnixPath.isRelative, isRoot = UnixPath.isRoot, concat = UnixPath.concat, fromUnixPath = UnixPath.fromUnixPath, toUnixPath = UnixPath.toUnixPath }
                                    end
                                ) ()
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
