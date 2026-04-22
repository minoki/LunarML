structure export = struct
  (* String.size: returns length of string *)
  fun helloLen () : int = String.size "Hello, World!"

  (* String.^: concatenation, returns length of result *)
  fun concatLen () : int = String.size ("abc" ^ "!")

  (* String.=: equality, 1 if equal, 0 if not *)
  fun strEqSame () : int = if "abc" = "abc" then 1 else 0
  fun strEqDiff () : int = if "abc" = "abd" then 1 else 0
  fun strEqLenDiff () : int = if "abc" = "abcd" then 1 else 0
  fun strEqEmpty () : int = if "" = "" then 1 else 0

  (* String.<: lexicographic less-than *)
  fun strLtLess () : int = if String.< ("abc", "abd") then 1 else 0
  fun strLtEqual () : int = if String.< ("abc", "abc") then 1 else 0
  fun strLtPrefix () : int = if String.< ("ab", "abc") then 1 else 0
  fun strLtLonger () : int = if String.< ("abc", "ab") then 1 else 0

  (* String.str: char to single-char string, returns its length (always 1) *)
  fun strFromChar () : int = String.size (String.str #"A")

  (* String.concat: concatenate list of strings, return length *)
  fun concatList () : int =
    String.size (String.concat ["Hello", ", ", "World", "!"])

  (* String.implode: list of chars to string, return length *)
  fun implodeTest () : int =
    String.size (String.implode [#"H", #"i", #"!"])

  (* Concatenation of empty strings *)
  fun concatEmpty () : int = String.size ("" ^ "")
  fun concatWithEmpty () : int = String.size ("abc" ^ "")

  (* Multi-step concatenation *)
  fun multiConcat () : int =
    String.size (String.concat ["a", "bb", "ccc", "dddd"])
end
