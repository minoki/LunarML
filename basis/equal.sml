val op = = fn (x, y) => _primCall "=" (x, y);
fun x <> y = _primCall "Bool.not" (_primCall "=" (x, y));
_equality 'a ref = fn (x, y) => _primCall "Ref.=" (x, y);
_equality 'a array = fn (x, y) => _primCall "Array.=" (x, y);
_equality ''a list = fn (nil, nil) => true
                      | (x :: xs, y :: ys) => x = y andalso xs = ys
                      | _ => false;
