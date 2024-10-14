structure JavaScript = struct
  fun function f = _primCall "JavaScript.function" (f)
  fun encodeUtf8 s = _primCall "JavaScript.encodeUtf8" (s)
  fun decodeUtf8 s = _primCall "JavaScript.decodeUtf8" (s)
end;
