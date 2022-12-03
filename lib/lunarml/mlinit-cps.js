"use strict";
function _id(k, h, x) { return [false, k, [x]]; }
function _ref(k, h, x) { return [false, k, [{ tag: "ref", payload: x }]]; }
const _nil = { tag: "nil" };
function _cons(k, h, a) { return [false, k, [{ tag: "::", payload: a }]]; }
function _list(a) {
    var x = _nil;
    for (var i = a.length - 1; i >= 0; --i) {
        x = { tag: "::", payload: [a[i], x] };
    }
    return x;
}
function _Match_tag() {}
_Match_tag.prototype.name = "Match";
const _Match = new _Match_tag();
function _Bind_tag() {}
_Bind_tag.prototype.name = "Bind";
const _Bind = new _Bind_tag();
function _Div_tag() {}
_Div_tag.prototype.name = "Div";
const _Div = new _Div_tag();
function _Overflow_tag() {}
_Overflow_tag.prototype.name = "Overflow";
const _Overflow = new _Overflow_tag();
function _Size_tag() {}
_Size_tag.prototype.name = "Size";
const _Size = new _Size_tag();
function _Subscript_tag() {}
_Subscript_tag.prototype.name = "Subscript";
const _Subscript = new _Subscript_tag();
function _Fail_tag(payload) { this.payload = payload; }
_Fail_tag.prototype.name = "Fail";
function _Fail(k, h, payload) { return [false, k, [new _Fail_tag(payload)]]; }
function _String_EQUAL(s, t) {
    if (s === t) { return true; }
    var n = s.length;
    if (n !== t.length) { return false; }
    for (var i = 0; i < n; ++i) {
        if (s[i] !== t[i]) {
            return false;
        }
    }
    return true;
}
const MIN_INT32 = -0x80000000;
const MAX_INT32 = 0x7fffffff;
function _Int_abs(k, h, x) {
    if (x < 0) {
        if (x === MIN_INT32) {
            return [false, h, [_Overflow]];
        }
        return [false, k, [-x]];
    } else {
        return [false, k, [x]];
    }
}
function _Int_negate(k, h, x) {
    if (x === MIN_INT32) {
        return [false, h, [_Overflow]];
    }
    return [false, k, [(-x)|0]];
}
function __Int_add(x, y) {
    var z = x + y;
    if (z < MIN_INT32 || z > MAX_INT32) {
        throw _Overflow;
    }
    return z;
}
function __Int_sub(x, y) {
    var z = x - y;
    if (z < MIN_INT32 || z > MAX_INT32) {
        throw _Overflow;
    }
    return z;
}
function __Int_mul(x, y) {
    var z = x * y;
    if (z < MIN_INT32 || z > MAX_INT32) {
        throw _Overflow;
    }
    return z|0;
}
function __Int_div(x, y) {
    if (y === 0) {
        throw _Div;
    } else if (x === MIN_INT32 && y === -1) {
        throw _Overflow;
    }
    return Math.floor(x / y)|0;
}
function __Int_mod(x, y) {
    if (y === 0) {
        throw _Div;
    }
    return x - Math.floor(x / y) * y;
}
function __Int_quot(x, y) {
    if (y === 0) {
        throw _Div;
    } else if (x === MIN_INT32 && y === -1) {
        throw _Overflow;
    }
    return (x / y)|0;
}
function __Int_rem(x, y) {
    if (y === 0) {
        throw _Div;
    }
    return (x % y)|0;
}
function __Word_div(x, y) {
    if (y === 0) {
        throw _Div;
    }
    return (x / y)>>>0;
}
function __Word_mod(x, y) {
    if (y === 0) {
        throw _Div;
    }
    return (x % y)>>>0;
}
const Math_abs = Math.abs;
function _Real_abs(k, h, x) {
    return [false, k, [Math_abs(x)]];
}
const Math_imul = Math.imul;
function _call(k1, h, f) {
    return [false, k1, [function(k2, h, args) {
        return [false, k2, [f.apply(undefined, args)]];
    }]];
}
function _new(k1, h, f) {
    return [false, k1, [function(k2, h, args) {
        return [false, k2, [Reflect.construct(f, args)]];
    }]];
}
function _method(k1, h, a) {
    var obj = a[0];
    var f = obj[a[1]];
    return [false, k1, [function(k2, h, args) {
        return [false, k2, [f.apply(obj, args)]];
    }]];
}
function _encodeUtf8(k, h, s) {
    var encoder = new TextEncoder();
    return [false, k, [encoder.encode(s)]];
}
function _decodeUtf8(k, h, s) {
    var decoder = new TextDecoder();
    return [false, k, [decoder.decode(s)]];
}
function _exnName(k, h, e) { return _encodeUtf8(k, h, e.name); }
function _String_LT(a, b) {
    var i = 0;
    var m = a.length, n = b.length;
    while (i < m && i < n) {
        var x = a[i], y = b[i];
        if (x < y) {
            return true;
        } else if (x > y) {
            return false;
        }
        ++i;
    }
    return m < n;
}
function _String_append(a, b) {
    if (a.length === 0) { return b; }
    if (b.length === 0) { return a; }
    var c = new Uint8Array(a.length + b.length);
    c.set(a);
    c.set(b, a.length);
    return c;
}
function _String_concat(k, h, xs) {
    var n = 0;
    var xs0 = xs;
    while (xs0.tag === "::") {
        var s = xs0.payload[0];
        n += s.length;
        xs0 = xs0.payload[1];
    }
    var a = new Uint8Array(n);
    var m = 0;
    while (xs.tag === "::") {
        var s = xs.payload[0];
        a.set(s, m);
        m += s.length;
        xs = xs.payload[1];
    }
    return [false, k, [a]];
}
function _String_concatWith(sep, xs) {
    var n = 0;
    var xs0 = xs;
    while (xs0.tag === "::") {
        var s = xs0.payload[0];
        n += s.length;
        xs0 = xs0.payload[1];
        if (xs0.tag === "::") {
            n += sep.length;
        }
    }
    var a = new Uint8Array(n);
    var m = 0;
    while (xs.tag === "::") {
        var s = xs.payload[0];
        a.set(s, m);
        m += s.length;
        xs = xs.payload[1];
        if (xs.tag === "::") {
            a.set(sep, m);
            m += sep.length;
        }
    }
    return a;
}
function _String_implode(k, h, xs) {
    var n = 0;
    var xs0 = xs;
    while (xs0.tag === "::") {
        ++n;
        xs0 = xs0.payload[1];
    }
    var a = new Uint8Array(n);
    var i = 0;
    while (xs.tag === "::") {
        a[i] = xs.payload[0];
        xs = xs.payload[1];
        ++i;
    }
    return [false, k, [a]];
}
function _Array_array(k, h, t) {
    var n = t[0], init = t[1];
    if (n < 0) {
        throw _Size;
    }
    var a = new Array(n);
    a.fill(init);
    return [false, k, [a]];
}
function _VectorOrArray_fromList(k, h, xs) {
    var a = [];
    while (xs.tag === "::") {
        a.push(xs.payload[0]);
        xs = xs.payload[1];
    }
    return [false, k, [a]];
}
function _Vector_unsafeFromListRevN(n, xs) {
    var a = new Array(n);
    var i = n - 1;
    while (xs.tag === "::") {
        a[i] = xs.payload[0];
        xs = xs.payload[1];
        --i;
    }
    // i should be -1 here
    return a;
}
function _Vector_concat(k, h, xs) {
    var n = 0;
    var xs0 = xs;
    while (xs0.tag === "::") {
        var v = xs0.payload[0];
        n += v.length;
        xs0 = xs0.payload[1];
    }
    var a = new Array(n);
    var m = 0;
    while (xs.tag === "::") {
        var v = xs.payload[0];
        for (var i = 0; i < v.length; ++i) {
            a[m++] = v[i];
        }
        xs = xs.payload[1];
    }
    return [false, k, [a]];
}
function _PromptTag() {
}
function _newPromptTag() {
    return new _PromptTag();
}
var _topLevel = new _PromptTag();
const _emptySeq = { tag: "nil" };
function _consPrompt(p, xs) {
    return { tag: "prompt", p: p, tail: xs };
}
function _consCont(k, exh, xs) {
    return { tag: "cont", k: k, exh: exh, tail: xs }
}
function _appendSeq(xs, ys) {
    if (xs.tag === "nil") {
        return ys;
    } else if (xs.tag === "cont") {
        return _consCont(xs.k, xs.exh, _appendSeq(xs.tail, ys));
    } else {
        return _consPrompt(xs.p, _appendSeq(xs.tail, ys));
    }
}
function _splitSeq(p, xs) {
    if (xs.tag === "nil") {
        throw new Error("Prompt was not found on the stack");
    } else {
        if (xs.tag === "prompt" && xs.p === p) {
            return [_emptySeq, xs.tail];
        } else {
            var [ys, zs] = _splitSeq(p, xs.tail);
            if (xs.tag === "cont") {
                return [_consCont(xs.k, xs.exh, ys), zs];
            } else {
                return [_consPrompt(xs.p, ys), zs];
            }
        }
    }
}
var _metaCont = _emptySeq;
function _underflow(v) {
    while (_metaCont.tag !== "nil") {
        if (_metaCont.tag === "cont") {
            const k = _metaCont.k;
            _metaCont = _metaCont.tail;
            return [false, k, [v]];
        }
        _metaCont = _metaCont.tail;
    }
    return [true, v];
}
function _throw(e) {
    while (_metaCont.tag !== "nil") {
        if (_metaCont.tag === "cont") {
            const exh = _metaCont.exh;
            _metaCont = _metaCont.tail;
            return [false, exh, [e]];
        }
        _metaCont = _metaCont.tail;
    }
    throw e;
}
function _pushPrompt(p, f, k, exh) {
    /* f : unit -> 'a */
    const metaCont_old = _consCont(k, exh, _metaCont);
    _metaCont = _consPrompt(p, metaCont_old);
    return [false, f, [_underflow, _throw, undefined]];
}
function _withSubCont(p, f, k, exh) {
    /* f : ('a,'b) subcont -> 'b */
    var [aboveP, belowP] = _splitSeq(p, _metaCont);
    _metaCont = belowP;
    return [false, f, [_underflow, _throw, _consCont(k, exh, aboveP)]];
}
function _pushSubCont(subcont, f, k, exh) {
    /* f : unit -> 'a */
    _metaCont = _appendSeq(subcont, _consCont(k, exh, _metaCont));
    return [false, f, [_underflow, _throw, undefined]];
}
function _run(f) {
    var metaCont_old = _metaCont;
    _metaCont = _consPrompt(_topLevel, _emptySeq);
    var r = f(result => [true, result], e => { _metaCont = metaCont_old; throw e; });
    while (!r[0]) {
        r = r[1].apply(undefined, r[2]);
    }
    _metaCont = metaCont_old;
    return r[1];
}
function _function(k, h, f) {
    return [false, k, [function() {
        var metaCont_old = _metaCont;
        _metaCont = _emptySeq;
        var r = f(result => [true, result], e => { _metaCont = metaCont_old; throw e; }, arguments);
        while (!r[0]) {
            r = r[1].apply(undefined, r[2]);
        }
        _metaCont = metaCont_old;
        return r[1];
    }]];
}
