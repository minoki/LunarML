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
          end = struct
type syserror = string
exception SysErr of string * syserror option
local
    structure fs = struct
    _esImport [pure] { mkdirSync, rmdirSync, statSync, lstatSync, readlinkSync, rmSync, renameSync } from "node:fs";
    end
    structure process = struct
    _esImport [pure] { chdir, cwd, exit, env, platform } from "node:process";
    end
    structure child_process = struct
    _esImport [pure] { execSync } from "node:child_process";
    end
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
fun chDir (dir : string) = ( JavaScript.call process.chdir #[JavaScript.fromWideString (JavaScript.decodeUtf8 dir)]
                           ; () (* TODO: raise SysErr *)
                           )
fun getDir () : string = JavaScript.encodeUtf8 (JavaScript.unsafeFromValue (JavaScript.call process.cwd #[]))
fun mkDir (path : string) = let val options = JavaScript.newObject ()
                            in JavaScript.set (options, JavaScript.fromWideString "recursive", JavaScript.fromBool true)
                             ; JavaScript.call fs.mkdirSync #[JavaScript.fromWideString (JavaScript.decodeUtf8 path), options]
                             ; ()
                            end
fun rmDir (path : string) = ( JavaScript.call fs.rmdirSync #[JavaScript.fromWideString (JavaScript.decodeUtf8 path)]
                            ; ()
                            )
fun isDir (path : string) = let val stat = JavaScript.call fs.statSync #[JavaScript.fromWideString (JavaScript.decodeUtf8 path)]
                            in JavaScript.unsafeFromValue (JavaScript.method (stat, "isDirectory") #[])
                            end
fun isLink (path : string) = let val stat = JavaScript.call fs.lstatSync #[JavaScript.fromWideString (JavaScript.decodeUtf8 path)]
                             in JavaScript.unsafeFromValue (JavaScript.method (stat, "isSymbolicLink") #[])
                             end
fun readLink (path : string) : string = JavaScript.encodeUtf8 (JavaScript.unsafeFromValue (JavaScript.call fs.readlinkSync #[JavaScript.fromWideString (JavaScript.decodeUtf8 path)]))
(* fullPath, realPath, modTime, fileSize, setTime *)
fun remove (path : string) = ( JavaScript.call fs.rmSync #[JavaScript.fromWideString (JavaScript.decodeUtf8 path)]
                             ; ()
                             )
fun rename { old : string, new : string } = ( JavaScript.call fs.renameSync #[JavaScript.fromWideString (JavaScript.decodeUtf8 old), JavaScript.fromWideString (JavaScript.decodeUtf8 new)]
                                            ; ()
                                            )
end (* structure FileSys *)
structure IO = struct end
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
                                    let val isWindows = (JavaScript.unsafeFromValue process.platform : WideString.string) = "win32"
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
fun system (command : string) = ( JavaScript.call child_process.execSync #[JavaScript.fromWideString (JavaScript.decodeUtf8 command)]
                                ; success (* TODO: catch failures *)
                                )
(* val atExit : (unit -> unit) -> unit *)
fun exit (status : status) : 'a = ( JavaScript.call process.exit #[JavaScript.fromInt status]
                                  ; _primCall "unreachable" ()
                                  )
fun terminate (status : status) : 'a = ( JavaScript.call process.exit #[JavaScript.fromInt status]
                                       ; _primCall "unreachable" ()
                                       )
fun getEnv (name : string) : string option = let val value = JavaScript.field (process.env, JavaScript.decodeUtf8 name)
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
