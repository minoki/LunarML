local
structure DelimCont = LunarML.DelimCont
structure JavaScript_Promise :> sig
    structure Promise : sig
        type 'a promise
        datatype 'a result = FULFILLED of 'a | REJECTED of exn
        val new : ({ resolve : 'a -> unit, reject : exn -> unit } -> unit) -> 'a promise
        val newNested : ({ resolve : 'a -> unit, resolveTo : 'a promise -> unit, reject : exn -> unit } -> unit) -> 'a promise
        val reject : exn -> 'a promise
        val resolve : 'a -> 'a promise
        val withResolvers : unit -> { promise : 'a promise, resolve : 'a -> unit, reject : exn -> unit }
        val withResolversNested : unit -> { promise : 'a promise, resolve : 'a -> unit, resolveTo : 'a promise -> unit, reject : exn -> unit }
        val andThen : ('a -> 'b promise) -> 'a promise -> 'b promise
        val andThen_ : ('a -> unit) -> 'a promise -> unit
        val andThenWithCatch : ('a -> 'b promise) * (exn -> 'b promise) -> 'a promise -> 'b promise
        val andThenWithCatch_ : ('a -> unit) * (exn -> unit) -> 'a promise -> unit
        val map : ('a -> 'b) -> 'a promise -> 'b promise
        val catch : (exn -> 'a) -> 'a promise -> 'a promise
        val finally : (unit -> unit) -> 'a promise -> 'a promise
        val all2 : 'a promise * 'b promise -> ('a * 'b) promise
        val all3 : 'a promise * 'b promise * 'c promise -> ('a * 'b * 'c) promise
        val all4 : 'a promise * 'b promise * 'c promise * 'd promise -> ('a * 'b * 'c * 'd) promise
        val all5 : 'a promise * 'b promise * 'c promise * 'd promise * 'e promise -> ('a * 'b * 'c * 'd * 'e) promise
        val allSettled2 : 'a promise * 'b promise -> ('a result * 'b result) promise
        val allSettled3 : 'a promise * 'b promise * 'c promise -> ('a result * 'b result * 'c result) promise
        val allSettled4 : 'a promise * 'b promise * 'c promise * 'd promise -> ('a result * 'b result * 'c result * 'd result) promise
        val allSettled5 : 'a promise * 'b promise * 'c promise * 'd promise * 'e promise -> ('a result * 'b result * 'c result * 'd result * 'e result) promise
        structure List : sig
                        val all : ('a promise) list -> ('a list) promise
                        val allSettled : ('a promise) list -> (('a result) list) promise
                        val any : ('a promise) list -> 'a promise
                        val race : ('a promise) list -> 'a promise
                    end
        structure Vector : sig
                        val all : ('a promise) vector -> ('a vector) promise
                        val allSettled : ('a promise) vector -> (('a result) vector) promise
                        val any : ('a promise) vector -> 'a promise
                        val race : ('a promise) vector -> 'a promise
                    end
    end
    val async : ('a -> 'b) -> 'a -> 'b Promise.promise
    val await : 'a Promise.promise -> 'a
end = struct
open JavaScript
structure Promise = struct
type 'a promise = value
datatype 'a result = FULFILLED of 'a | REJECTED of exn
fun wrapThenable (x: 'a): value = _primCall "JavaScript.call" (_Prim.JavaScript.wrapThenable, #[_primCall "Unsafe.cast" (x)])
fun unwrapThenable (x: value): 'a = _primCall "Unsafe.cast" (_primCall "JavaScript.call" (_Prim.JavaScript.unwrapThenable, #[x]))
fun newNested (callback : { resolve : 'a -> unit, resolveTo : 'a promise -> unit, reject : exn -> unit } -> unit): 'a promise
  = let fun callback' args
          = let val resolve = Unsafe.Vector.sub (args, 0)
                val reject = Unsafe.Vector.sub (args, 1)
            in callback { resolve = fn x => ignore (call resolve #[wrapThenable x])
                        , resolveTo = fn x => ignore (call resolve #[unsafeToValue x])
                        , reject = fn e => ignore (call reject #[unsafeToValue e])
                        }
            end
    in unsafeFromValue (new Lib.Promise #[JavaScript.callback callback'])
    end
fun new (callback : { resolve : 'a -> unit, reject : exn -> unit } -> unit): 'a promise
  = newNested (fn { resolve, reject, resolveTo = _ } => callback { resolve = resolve, reject = reject })
fun reject (e: exn): 'a promise = unsafeFromValue (call Lib.Promise.reject #[unsafeToValue e])
fun resolve (x: 'a): 'a promise = unsafeFromValue (call Lib.Promise.resolve #[wrapThenable x])
fun withResolversNested (): { promise : 'a promise, resolve : 'a -> unit, resolveTo : 'a promise -> unit, reject : exn -> unit }
  = let val result = call Lib.Promise.withResolvers #[]
        val resolve = LunarML.assumeDiscardable field (result, "resolve")
        val reject = LunarML.assumeDiscardable field (result, "reject")
    in { promise = unsafeFromValue (field (result, "promise"))
       , resolve = fn x => (call resolve #[wrapThenable x]; ())
       , resolveTo = fn p => (call resolve #[unsafeToValue p]; ())
       , reject = fn e => (call reject #[unsafeToValue e]; ())
       }
    end
fun withResolvers (): { promise : 'a promise, resolve : 'a -> unit, reject : exn -> unit }
  = let val { promise, resolve, resolveTo = _, reject } = withResolversNested ()
    in { promise = promise, resolve = resolve, reject = reject }
    end
fun andThen (f : 'a -> 'b promise) (p : 'a promise) : 'b promise
  = let fun callback args
          = let val arg = unwrapThenable (Unsafe.Vector.sub (args, 0))
            in newNested (fn { resolveTo, ... } => resolveTo (f arg))
            end
    in method (unsafeToValue p, "then") #[function callback]
    end
fun andThenWithCatch (f : 'a -> 'b promise, h : exn -> 'b promise) (p : 'a promise) : 'b promise
  = let fun callback args
          = let val arg = unwrapThenable (Unsafe.Vector.sub (args, 0))
            in newNested (fn { resolveTo, ... } => resolveTo (f arg))
            end
        fun onError args
          = let val arg = unsafeFromValue (Unsafe.Vector.sub (args, 0))
            in newNested (fn { resolveTo, ... } => resolveTo (h arg))
            end
    in method (unsafeToValue p, "then") #[function callback, function onError]
    end
fun andThen_ (f : 'a -> unit) (p : 'a promise) : unit
  = let fun cb args
          = let val arg = unwrapThenable (Unsafe.Vector.sub (args, 0))
            in f arg
            end
    in ignore (method (unsafeToValue p, "then") #[callback cb])
    end
fun andThenWithCatch_ (f : 'a -> unit, h : exn -> unit) (p : 'a promise) : unit
  = let fun cb args
          = let val arg = unwrapThenable (Unsafe.Vector.sub (args, 0))
            in f arg
            end
        fun onError args
          = let val arg = unsafeFromValue (Unsafe.Vector.sub (args, 0))
            in h arg
            end
    in ignore (method (unsafeToValue p, "then") #[callback cb, callback onError])
    end
fun map (f : 'a -> 'b) (p : 'a promise) : 'b promise
  = let fun callback args
          = let val arg = unwrapThenable (Unsafe.Vector.sub (args, 0))
            in newNested (fn { resolve, ... } => resolve (f arg))
            end
    in method (unsafeToValue p, "then") #[function callback]
    end
fun catch (h : exn -> 'a) (p : 'a promise) : 'a promise
  = let fun callback args
          = let val arg = unsafeFromValue (Unsafe.Vector.sub (args, 0))
            in newNested (fn { resolve, ... } => resolve (wrapThenable (h arg)))
            end
    in method (unsafeToValue p, "catch") #[function callback]
    end
fun finally (h : unit -> unit) (p : 'a promise) : 'a promise
  = let fun callback args
          = let val arg = unsafeFromValue (Unsafe.Vector.sub (args, 0))
            in h arg
             ; undefined
            end
    in method (unsafeToValue p, "finally") #[function callback]
    end
fun all2 (a : 'a promise, b : 'b promise) : ('a * 'b) promise
  = unsafeFromValue (call Lib.Promise.all #[unsafeToValue a, unsafeToValue b])
fun all3 (a : 'a promise, b : 'b promise, c : 'c promise) : ('a * 'b * 'c) promise
  = unsafeFromValue (call Lib.Promise.all #[unsafeToValue a, unsafeToValue b, unsafeToValue c])
fun all4 (a : 'a promise, b : 'b promise, c : 'c promise, d : 'd promise) : ('a * 'b * 'c * 'd) promise
  = unsafeFromValue (call Lib.Promise.all #[unsafeToValue a, unsafeToValue b, unsafeToValue c, unsafeToValue d])
fun all5 (a : 'a promise, b : 'b promise, c : 'c promise, d : 'd promise, e : 'e promise) : ('a * 'b * 'c * 'd * 'e) promise
  = unsafeFromValue (call Lib.Promise.all #[unsafeToValue a, unsafeToValue b, unsafeToValue c, unsafeToValue d, unsafeToValue e])
fun settledResult (x : value) : 'a result
  = if unsafeFromValue (field (x, "status")) = ("fulfilled" : String16.string) then
        FULFILLED (unwrapThenable (field (x, "value")))
    else
        REJECTED (unsafeFromValue (field (x, "reason")))
fun allSettled2 (a : 'a promise, b : 'b promise) : ('a * 'b) promise
  = let val p: (value * value) promise = unsafeFromValue (call Lib.Promise.allSettled #[unsafeToValue a, unsafeToValue b])
    in map (fn (a', b') => (settledResult a', settledResult b')) p
    end
fun allSettled3 (a : 'a promise, b : 'b promise, c : 'c promise) : ('a * 'b * 'c) promise
  = let val p: (value * value * value) promise = unsafeFromValue (call Lib.Promise.allSettled #[unsafeToValue a, unsafeToValue b, unsafeToValue c])
    in map (fn (a', b', c') => (settledResult a', settledResult b', settledResult c')) p
    end
fun allSettled4 (a : 'a promise, b : 'b promise, c : 'c promise, d : 'd promise) : ('a * 'b * 'c * 'd) promise
  = let val p: (value * value * value * value) promise = unsafeFromValue (call Lib.Promise.allSettled #[unsafeToValue a, unsafeToValue b, unsafeToValue c, unsafeToValue d])
    in map (fn (a', b', c', d') => (settledResult a', settledResult b', settledResult c', settledResult d')) p
    end
fun allSettled5 (a : 'a promise, b : 'b promise, c : 'c promise, d : 'd promise, e : 'e promise) : ('a * 'b * 'c * 'd * 'e) promise
  = let val p: (value * value * value * value * value) promise = unsafeFromValue (call Lib.Promise.allSettled #[unsafeToValue a, unsafeToValue b, unsafeToValue c, unsafeToValue d, unsafeToValue e])
    in map (fn (a', b', c', d', e') => (settledResult a', settledResult b', settledResult c', settledResult d', settledResult e')) p
    end
local
val Vector_fromList = Vector.fromList
val Vector_foldr = Vector.foldr
in
structure Vector = struct
fun all (ps : ('a promise) vector) : ('a vector) promise
  = let val p: (value vector) promise = unsafeFromValue (call Lib.Promise.all (Unsafe.cast ps))
    in map (Vector.map unwrapThenable) p
    end
fun allSettled (ps : ('a promise) vector) : (('a result) vector) promise
  = let val p: (value vector) promise = unsafeFromValue (call Lib.Promise.all (Unsafe.cast ps))
    in map (Vector.map settledResult) p
    end
fun any (ps : ('a promise) vector) : 'a promise
  = unsafeFromValue (call Lib.Promise.any (Unsafe.cast ps))
fun race (ps : ('a promise) vector) : 'a promise
  = unsafeFromValue (call Lib.Promise.race (Unsafe.cast ps))
end
structure List = struct
fun all (ps : ('a promise) list) : ('a list) promise
  = let val ps = Vector_fromList (List.map unsafeToValue ps)
        val p: (value vector) promise = unsafeFromValue (call Lib.Promise.all ps)
    in map (Vector_foldr (fn (x, xs) => unwrapThenable x :: xs) []) p
    end
fun allSettled (ps : ('a promise) list) : (('a result) list) promise
  = let val ps = Vector_fromList (List.map unsafeToValue ps)
        val p: (value vector) promise = unsafeFromValue (call Lib.Promise.allSettled (Unsafe.cast ps))
    in map (Vector_foldr (fn (x, xs) => settledResult x :: xs) []) p
    end
fun any (ps : ('a promise) list) : 'a promise
  = unsafeFromValue (call Lib.Promise.any (Vector_fromList (List.map unsafeToValue ps)))
fun race (ps : ('a promise) list) : 'a promise
  = unsafeFromValue (call Lib.Promise.race (Vector_fromList (List.map unsafeToValue ps)))
end
end
end (* structure Promise *)
fun async (f : 'a -> 'b) : 'a -> 'b Promise.promise = fn x =>
  Promise.new (fn { resolve, reject = _ } => resolve (f x))
fun await (p : 'a Promise.promise) : 'a =
  DelimCont.withSubCont
    (DelimCont.topLevel,
      fn sk =>
        let fun fulfilled x = DelimCont.pushSubCont (sk, fn () => x)
            fun rejected e = DelimCont.pushSubCont (sk, fn () => raise e)
        in Promise.andThenWithCatch_ (fulfilled, rejected) p
        end
    )
end
in
structure JavaScript = struct
open JavaScript
open JavaScript_Promise
end
end;
