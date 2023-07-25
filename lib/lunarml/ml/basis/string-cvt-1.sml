structure StringCvt :> STRING_CVT where type radix = StringCvt.radix
                                  where type realfmt = StringCvt.realfmt = struct
open StringCvt
fun padLeft c i s = if String.size s >= i orelse i <= 0 then
                        s
                    else
                        let val c = String.str c
                            fun loop (j, acc) = if j <= 0 then
                                                    String.concat acc
                                                else
                                                    loop (j - 1, c :: acc)
                        in loop (i - String.size s, [s])
                        end
fun padRight c i s = if String.size s >= i orelse i <= 0 then
                         s
                     else
                         let val c = String.str c
                             fun loop (j, acc) = if j <= 0 then
                                                     String.concat (s :: acc)
                                                 else
                                                     loop (j - 1, c :: acc)
                         in loop (i - String.size s, [])
                         end
fun splitl f rdr src = let fun loop (acc, src) = case rdr src of
                                                     NONE => (String.implodeRev acc, src)
                                                   | SOME (x, src') => loop (x :: acc, src')
                       in loop ([], src)
                       end
fun takel f rdr s = #1 (splitl f rdr s)
fun dropl f rdr s = #2 (splitl f rdr s)
fun skipWS rdr = dropl Char.isSpace rdr
type cs = string * int (* the underlying string, the starting index *)
fun scanString scan s = case scan (fn (s, i) => if i < String.size s then
                                                    SOME (String.sub (s, i), (s, i + 1))
                                                else
                                                    NONE
                                  ) (s, 0) of
                            SOME (x, _) => SOME x
                          | NONE => NONE
end;
