_equality word = fn (x, y) => _primCall "Word.=" (x, y);
structure Word = struct
type word = word
fun ~ x = _primCall "Word.~" (x)
fun x + y = _primCall "Word.+" (x, y)
fun x - y = _primCall "Word.-" (x, y)
fun x * y = _primCall "Word.*" (x, y)
fun x div y = if y = 0w0 then
                  raise Div
              else
                  _primCall "Word.div.unchecked" (x, y)
fun x mod y = if y = 0w0 then
                  raise Div
              else
                  _primCall "Word.mod.unchecked" (x, y)
fun x < y = _primCall "Word.<" (x, y)
fun x <= y = _primCall "Word.<=" (x, y)
fun x > y = _primCall "Word.>" (x, y)
fun x >= y = _primCall "Word.>=" (x, y)
end
local
fun fromWord (x : word) = x
in
_overload "Word" [word] { + = Word.+
                        , - = Word.-
                        , * = Word.*
                        , div = Word.div
                        , mod = Word.mod
                        , ~ = Word.~
                        , < = Word.<
                        , <= = Word.<=
                        , > = Word.>
                        , >= = Word.>=
                        , fromWord = fromWord
                        , wordSize = 32
                        };
end;

_equality _Prim.Word64.word = fn (x, y) => _primCall "Word64.=" (x, y);
structure Word64 = struct
type word = _Prim.Word64.word
fun ~ x = _primCall "Word64.~" (x)
fun x + y = _primCall "Word64.+" (x, y)
fun x - y = _primCall "Word64.-" (x, y)
fun x * y = _primCall "Word64.*" (x, y)
fun x div y = if y = 0w0 then
                  raise Div
              else
                  _primCall "Word64.div.unchecked" (x, y)
fun x mod y = if y = 0w0 then
                  raise Div
              else
                  _primCall "Word64.mod.unchecked" (x, y)
fun x < y = _primCall "Word64.<" (x, y)
fun x <= y = _primCall "Word64.<=" (x, y)
fun x > y = _primCall "Word64.>" (x, y)
fun x >= y = _primCall "Word64.>=" (x, y)
end
local
fun fromWord (x : word) = _primCall "Word.toWord64" (x)
in
_overload "Word" [Word64.word]
  { + = Word64.+
  , - = Word64.-
  , * = Word64.*
  , div = Word64.div
  , mod = Word64.mod
  , ~ = Word64.~
  , < = Word64.<
  , <= = Word64.<=
  , > = Word64.>
  , >= = Word64.>=
  , fromWord = fromWord
  , wordSize = 64
  };
end;
