structure StringStream: STRING_STREAM =
struct
  type token = char
  type pos = {file: string, line: int, column: int}
  type stream = Substring.substring * pos
  fun fromString {file, content} =
    (Substring.full content, {file = file, line = 1, column = 1})
  fun next (content, {file, line, column}) =
    case Substring.getc content of
      NONE => NONE
    | SOME (c, content) =>
        let
          val newpos =
            if c = #"\n" then {file = file, line = line + 1, column = 1}
            else {file = file, line = line, column = column + 1}
        in
          SOME (c, newpos, (content, newpos))
        end
  fun initialPos file = {file = file, line = 1, column = 1}
end
