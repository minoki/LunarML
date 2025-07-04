val _Prim.ref = fn x => _primCall "Ref.ref" (x)
val _Prim.:: = fn (x, xs) => _primCall "List.::" (x, xs)
val _Prim.unit.equal = _primCall "mkFn2" (fn ((), ()) => true);
