signature STRING_STREAM =
sig
  include
    TOKEN_STREAM
    where type token = char
    where type pos = {file: string, line: int, column: int} (* both 1-based *)
  val fromString: {file: string, content: string}
                  -> stream (* (file, content) *)
end
