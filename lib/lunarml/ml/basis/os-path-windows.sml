functor WindowsPath (exception Path
                     exception InvalidArc
                    ) : OS_PATH = struct
exception Path = Path
exception InvalidArc = InvalidArc
(*
 * The arc separator: #"/" and #"\\".
 * Canonical paths are entirely lowercase.
 * Example volume names are "", "A:", "C:", "\\\\server\\share", and "//server/share".
 *)
val parentArc = ".."
val currentArc = "."
fun hasDOSVolume path = String.size path >= 2 andalso String.sub (path, 1) = #":" andalso Char.isAlpha (String.sub (path, 0))
fun isSlash #"/" = true
  | isSlash #"\\" = true
  | isSlash _ = false
(*: val testUNCPath : string -> int option *)
fun testUNCPath path = if String.size path >= 5 andalso isSlash (String.sub (path, 0)) andalso isSlash (String.sub (path, 1)) andalso not (isSlash (String.sub (path, 2))) then
                           let fun goShare i = if String.size path <= i then
                                                   SOME i
                                               else if isSlash (String.sub (path, i)) then
                                                   SOME i
                                               else
                                                   goShare (i + 1)
                               fun goServer i = if String.size path <= i then
                                                    NONE
                                                else if isSlash (String.sub (path, i)) then
                                                    let val ip1 = i + 1
                                                    in if String.size path > ip1 andalso not (isSlash (String.sub (path, ip1))) then
                                                           goShare (ip1 + 1)
                                                       else
                                                           NONE
                                                    end
                                                else
                                                    goServer (i + 1)
                           in goServer 3
                           end
                       else
                           NONE
datatype volume_type = DOS_VOLUME | UNC_PATH of int (* position *) | NO_VOLUME
fun testVolume path = if hasDOSVolume path then
                          DOS_VOLUME
                      else
                          case testUNCPath path of
                              SOME i => UNC_PATH i
                            | NONE => NO_VOLUME
fun stripVolume path = if hasDOSVolume path then
                           (String.substring (path, 0, 2), String.extract (path, 2, NONE))
                       else
                           case testUNCPath path of
                               SOME i => (String.substring (path, 0, i), String.extract (path, i, NONE))
                             | NONE => ("", path)
fun isAbsolute path = case testUNCPath path of
                          SOME _ => true
                        | NONE => let val rest = if hasDOSVolume path then
                                                     String.extract (path, 2, NONE)
                                                 else
                                                     path
                                  in if String.size rest >= 1 then
                                         let val c = String.sub (rest, 0)
                                         in c = #"/" orelse c = #"\\"
                                         end
                                     else
                                         false
                                  end
fun isRelative path = not (isAbsolute path)
fun isRoot path = let val (_, rest) = stripVolume path
                  in rest = "/" orelse rest = "\\"
                  end
fun getVolume path = #1 (stripVolume path)
fun validVolume { isAbs = false, vol = "" } = true
  | validVolume { isAbs, vol } = (String.size vol = 2 andalso Char.isAlpha (String.sub (vol, 0)) andalso String.sub (vol, 1) = #":")
                                 orelse (isAbs andalso case testUNCPath vol of SOME i => i = String.size vol | NONE => false)
(*: val fromString : string -> { isAbs : bool, vol : string, arcs : string list } *)
fun fromString path = let val (isUNC, vol, rest) = case testVolume path of
                                                       DOS_VOLUME => (false, String.substring (path, 0, 2), String.extract (path, 2, NONE))
                                                     | UNC_PATH i => (true, String.substring (path, 0, i), String.extract (path, i, NONE))
                                                     | NO_VOLUME => (false, "", path)
                      in case String.fields (fn c => c = #"/" orelse c = #"\\") rest of
                             [""] => { isAbs = isUNC, vol = vol, arcs = [] }
                           | "" :: xs => { isAbs = true, vol = vol, arcs = xs }
                           | xs => { isAbs = isUNC, vol = vol, arcs = xs }
                      end
local fun isValidArc arc = CharVector.all (fn c => c <> #"/" andalso c <> #"\\") arc
in
fun toString { isAbs, vol, arcs } = if not (validVolume { isAbs = isAbs, vol = vol }) then
                                        raise Path (* invalid volume *)
                                    else if not isAbs andalso String.size vol >= 3 then
                                        raise Path (* UNC path cannot be relative *)
                                    else
                                        case (isAbs, arcs) of
                                            (false, "" :: _) => raise Path
                                          | _ => if List.all isValidArc arcs then
                                                     if isAbs then
                                                         vol ^ "\\" ^ String.concatWith "\\" arcs
                                                     else
                                                         vol ^ String.concatWith "\\" arcs
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
                                                             val r = Substring.string r
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
fun mkCanonical path = let val { isAbs, vol, arcs } = fromString path
                           fun go (revAcc, "" :: xs) = go (revAcc, xs)
                             | go (_ :: revAcc, ".." :: xs) = go (revAcc, xs)
                             | go (revAcc, "." :: xs) = go (revAcc, xs)
                             | go (revAcc, x :: xs) = go (x :: revAcc, xs)
                             | go (revAcc, []) = String.map Char.toLower (toString { isAbs = isAbs, vol = vol, arcs = List.rev revAcc })
                       in go ([], arcs)
                       end
fun isCanonical path = path = mkCanonical path
local fun concatArcs ([], arcs2) = arcs2
        | concatArcs ([""], arcs2) = arcs2
        | concatArcs (x :: xs, arcs2) = x :: concatArcs (xs, arcs2)
in
fun concat (path, t) = case (fromString path, fromString t) of
                           (_, { isAbs = true, ... }) => raise Path
                         | ({ isAbs, vol = v1, arcs = arcs1 }, { vol = v2, arcs = arcs2, ... }) =>
                           if v2 = "" orelse v1 = v2 then
                               toString { isAbs = isAbs, vol = v1, arcs = concatArcs (arcs1, arcs2) }
                           else
                               raise Path
end
fun mkAbsolute { path, relativeTo } = if isAbsolute path then
                                          path
                                      else
                                          mkCanonical (concat (relativeTo, path))
fun mkRelative { path, relativeTo } = case (fromString path, fromString relativeTo) of
                                          ({ isAbs = false, ... }, _) =>
                                          let val (_, rest) = stripVolume path
                                          in rest (* path is relative *)
                                          end
                                        | (_, { isAbs = false, ... }) => raise Path (* relativeTo is relative *)
                                        | ({ isAbs = true, vol = pVol, arcs = pArcs }, { isAbs = true, vol = rVol, arcs = _ }) =>
                                          if CharVector.map Char.toUpper pVol <> CharVector.map Char.toUpper rVol then
                                              raise Path
                                          else
                                              let val abs = mkCanonical relativeTo
                                              in if path = abs then
                                                     currentArc
                                                 else
                                                     let val rArcs = #arcs (fromString abs)
                                                         fun stripCommonPrefix (xs as (x :: xs'), ys as (y :: ys'))
                                                             = if x = y then
                                                                   stripCommonPrefix (xs', ys')
                                                               else
                                                                   (xs, ys)
                                                           | stripCommonPrefix (xs, ys) = (xs, ys)
                                                         val (path', abs') = stripCommonPrefix (pArcs, rArcs)
                                                         val arcs = List.map (fn _ => "..") abs' @ (case path' of [""] => [] | _ => path')
                                                     in case arcs of
                                                            [] => currentArc
                                                          | _ :: _ => toString { isAbs = false, vol = "", arcs = arcs }
                                                     end
                                              end
fun getParent path = if isRoot path then
                         path
                     else
                         let val (vol, rest) = stripVolume path
                             val (part, lastArc) = Substring.splitr (fn c => c <> #"/" andalso c <> #"\\") (Substring.full rest)
                         in case Substring.string lastArc of
                                "." => path ^ "."
                              | ".." => path ^ "\\.."
                              | "" => path ^ ".."
                              | _ => if Substring.isEmpty part then
                                         vol ^ "."
                                     else
                                         vol ^ Substring.string part
                         end
fun fromUnixPath path = let val arcs = String.fields (fn c => c = #"/") path
                        in List.app (fn arc => if CharVector.exists (fn c => c = #"\\") arc then raise InvalidArc else ()) arcs
                         ; String.concatWith "\\" arcs
                        end
fun toUnixPath path = let val (vol, rest) = stripVolume path
                          val () = if vol <> "" then
                                       raise Path
                                   else
                                       ()
                          val arcs = String.fields (fn c => c = #"/" orelse c = #"\\") rest
                      in String.concatWith "/" arcs
                      end
end;
