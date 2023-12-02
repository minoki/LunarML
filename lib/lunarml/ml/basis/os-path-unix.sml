functor UnixPath (exception Path
                  exception InvalidArc
                 ) : OS_PATH = struct
exception Path = Path
exception InvalidArc = InvalidArc
val parentArc = ".."
val currentArc = "."
fun isAbsolute path = if String.isPrefix "/" path then
                          true
                      else
                          false
fun isRelative path = not (isAbsolute path)
fun isRoot path = path = "/"
fun validVolume { isAbs, vol } = vol = ""
fun getVolume (path : string) = ""
fun fromString path = case String.fields (fn c => c = #"/") path of
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
fun isCanonical path = path = mkCanonical path
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
fun getParent (path as "/") = path
  | getParent path = let val (part, lastArc) = Substring.splitr (fn c => c <> #"/") (Substring.full path)
                     in case Substring.string lastArc of
                            "." => path ^ "."
                          | ".." => path ^ "/.."
                          | "" => path ^ ".."
                          | _ => if Substring.isEmpty part then
                                     "."
                                 else
                                     Substring.string (Substring.trimr 1 part)
                     end
fun fromUnixPath (s : string) = s
fun toUnixPath (s : string) = s
end;
