"use strict";
function _id(x) { return x; }
function _ref(x) { return { tag: "ref", payload: x }; }
const _nil = { tag: "nil" };
function _cons(a) { return { tag: "::", payload: a }; }
function _list(a) {
    let x = _nil;
    for (let i = a.length - 1; i >= 0; --i) {
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
function _Fail(payload) { return new _Fail_tag(payload); }
function _String_EQUAL(s, t) {
    if (s === t) { return true; }
    const n = s.length;
    if (n !== t.length) { return false; }
    for (let i = 0; i < n; ++i) {
        if (s[i] !== t[i]) {
            return false;
        }
    }
    return true;
}
const MIN_INT32 = -0x80000000;
const MAX_INT32 = 0x7fffffff;
function _Int_abs(x) {
    if (x < 0) {
        if (x === MIN_INT32) {
            throw _Overflow;
        }
        return -x;
    } else {
        return x;
    }
}
function _Int_negate(x) {
    if (x === MIN_INT32) {
        throw _Overflow;
    }
    return (-x)|0;
}
function __Int_add(x, y) {
    const z = x + y;
    if (z < MIN_INT32 || z > MAX_INT32) {
        throw _Overflow;
    }
    return z;
}
function __Int_sub(x, y) {
    const z = x - y;
    if (z < MIN_INT32 || z > MAX_INT32) {
        throw _Overflow;
    }
    return z;
}
function __Int_mul(x, y) {
    const z = x * y;
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
const Math_imul = Math.imul;
function _new(f) {
    return function(args) {
        return Reflect.construct(f, args);
    };
}
function _method(a) {
    const obj = a[0];
    const f = obj[a[1]];
    return function(args) {
        return f.apply(obj, args);
    };
}
function _encodeUtf8(s) {
    const encoder = new TextEncoder();
    return encoder.encode(s);
}
function _decodeUtf8(s) {
    const decoder = new TextDecoder();
    return decoder.decode(s);
}
function _exnName(e) { return _encodeUtf8(e.name); }
function _String_LT(a, b) {
    let i = 0;
    const m = a.length, n = b.length;
    while (i < m && i < n) {
        const x = a[i], y = b[i];
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
    const c = new Uint8Array(a.length + b.length);
    c.set(a);
    c.set(b, a.length);
    return c;
}
function _String_concat(xs) {
    let n = 0;
    let xs0 = xs;
    while (xs0.tag === "::") {
        const s = xs0.payload[0];
        n += s.length;
        xs0 = xs0.payload[1];
    }
    const a = new Uint8Array(n);
    let m = 0;
    while (xs.tag === "::") {
        const s = xs.payload[0];
        a.set(s, m);
        m += s.length;
        xs = xs.payload[1];
    }
    return a;
}
function _String_concatWith(sep, xs) {
    let n = 0;
    let xs0 = xs;
    while (xs0.tag === "::") {
        const s = xs0.payload[0];
        n += s.length;
        xs0 = xs0.payload[1];
        if (xs0.tag === "::") {
            n += sep.length;
        }
    }
    const a = new Uint8Array(n);
    let m = 0;
    while (xs.tag === "::") {
        const s = xs.payload[0];
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
function _String_implode(xs) {
    let n = 0;
    let xs0 = xs;
    while (xs0.tag === "::") {
        ++n;
        xs0 = xs0.payload[1];
    }
    const a = new Uint8Array(n);
    let i = 0;
    while (xs.tag === "::") {
        a[i] = xs.payload[0];
        xs = xs.payload[1];
        ++i;
    }
    return a;
}
function _String_translate(f, s) {
    const m = s.length;
    const a = new Array(m);
    let n = 0;
    for (let i = 0; i < m; ++i) {
        const t = f(s[i]);
        a[i] = t;
        n += t.length;
    }
    const r = new Uint8Array(n);
    let l = 0;
    for (let i = 0; i < m; ++i) {
        r.set(a[i], l);
        l += a[i].length;
    }
    return r;
}
function _Array_array(t) {
    const n = t[0], init = t[1];
    if (n < 0) {
        throw _Size;
    }
    const a = new Array(n);
    a.fill(init);
    return a;
}
function _VectorOrArray_fromList(xs) {
    const a = [];
    while (xs.tag === "::") {
        a.push(xs.payload[0]);
        xs = xs.payload[1];
    }
    return a;
}
function _Vector_unsafeFromListRevN(n, xs) {
    const a = new Array(n);
    let i = n - 1;
    while (xs.tag === "::") {
        a[i] = xs.payload[0];
        xs = xs.payload[1];
        --i;
    }
    // i should be -1 here
    return a;
}
function _VectorOrArray_tabulate(t) {
    const n = t[0], f = t[1];
    if (n < 0) {
        throw _Size;
    }
    const a = new Array(n);
    for (let i = 0; i < n; ++i) {
        a[i] = f(i);
    }
    return a;
}
function _Vector_concat(xs) {
    let n = 0;
    let xs0 = xs;
    while (xs0.tag === "::") {
        const v = xs0.payload[0];
        n += v.length;
        xs0 = xs0.payload[1];
    }
    const a = new Array(n);
    let m = 0;
    while (xs.tag === "::") {
        const v = xs.payload[0];
        for (let i = 0; i < v.length; ++i) {
            a[m++] = v[i];
        }
        xs = xs.payload[1];
    }
    return a;
}
/*
type Trampoline<Result> = [true, Result] | exists X. [false, MLFunction<Result, X>, X]
interface MLFunction<Result, Arg> {
    (arg: Arg): Result;
    _MLTAIL_?: (arg: Arg) => Trampoline<Result>;
}
 */
function _wrap(f) {
    const F = function(x) {
        let result = f(x); // [true, Result] | exists X. [false, MLFunction<Result, X>, X]
        while (!result[0]) {
            const g = result[1];
            if ("_MLTAIL_" in g) {
                result = g._MLTAIL_(result[2]);
            } else {
                return g(result[2]);
            }
        }
        return result[1];
    }
    F._MLTAIL_ = f;
    return F;
}
function _function(f) {
    return function() {
        return f(arguments);
    };
}
