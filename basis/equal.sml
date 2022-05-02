val op = = fn (x, y) => _primCall "=" (x, y);
fun x <> y = _primCall "Bool.not" (_primCall "=" (x, y));
