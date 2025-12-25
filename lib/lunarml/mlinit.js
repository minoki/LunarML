"use strict";
//BEGIN _list
function _list(a) {
    let x = null;
    for (let i = a.length - 1; i >= 0; --i) {
        x = [a[i], x];
    }
    return x;
}
//END
//BEGIN _Match_tag
function _Match_tag() {}
_Match_tag.prototype.__isMLExn = true;
_Match_tag.prototype.name = "Match";
//END
//BEGIN _Match: _Match_tag
const _Match = new _Match_tag();
//END
//BEGIN _isMatch: _Match_tag
function _isMatch(e) { return e instanceof _Match_tag; }
//END
//BEGIN _Bind_tag
function _Bind_tag() {}
_Bind_tag.prototype.__isMLExn = true;
_Bind_tag.prototype.name = "Bind";
//END
//BEGIN _Bind: _Bind_tag
const _Bind = new _Bind_tag();
//END
//BEGIN _isBind: _Bind_tag
function _isBind(e) { return e instanceof _Bind_tag; }
//END
//BEGIN _Div_tag
function _Div_tag() {}
_Div_tag.prototype.__isMLExn = true;
_Div_tag.prototype.name = "Div";
//END
//BEGIN _Div: _Div_tag
const _Div = new _Div_tag();
//END
//BEGIN _isDiv: _Div_tag
function _isDiv(e) { return e instanceof _Div_tag; }
//END
//BEGIN _Overflow_tag
function _Overflow_tag() {}
_Overflow_tag.prototype.__isMLExn = true;
_Overflow_tag.prototype.name = "Overflow";
//END
//BEGIN _Overflow: _Overflow_tag
const _Overflow = new _Overflow_tag();
//END
//BEGIN _isOverflow: _Overflow_tag
function _isOverflow(e) { return e instanceof _Overflow_tag; }
//END
//BEGIN _Size_tag
function _Size_tag() {}
_Size_tag.prototype.__isMLExn = true;
_Size_tag.prototype.name = "Size";
//END
//BEGIN _Size: _Size_tag
const _Size = new _Size_tag();
//END
//BEGIN _isSize: _Size_tag
function _isSize(e) { return e instanceof _Size_tag; }
//END
//BEGIN _Subscript_tag
function _Subscript_tag() {}
_Subscript_tag.prototype.__isMLExn = true;
_Subscript_tag.prototype.name = "Subscript";
//END
//BEGIN _Subscript: _Subscript_tag
const _Subscript = new _Subscript_tag();
//END
//BEGIN _isSubscript: _Subscript_tag
function _isSubscript(e) { return e instanceof _Subscript_tag; }
//END
//BEGIN _Fail_tag
function _Fail_tag(payload) { this.payload = payload; }
_Fail_tag.prototype.__isMLExn = true;
_Fail_tag.prototype.name = "Fail";
//END
//BEGIN _Fail: _Fail_tag
function _Fail(payload) { return new _Fail_tag(payload); }
//END
//BEGIN _isFail: _Fail_tag
function _isFail(e) { return e instanceof _Fail_tag; }
//END
//BEGIN _Fail_payload
function _Fail_payload(e) { return e.payload; }
//END
//BEGIN _id
function _id(x) { return x; }
//END
//BEGIN _isError
function _isError(e) { return !("__isMLExn" in e); }
//END
//BEGIN _mkFn2
function _mkFn2(f) {
  return (a, b) => f([a, b]);
}
//END
//BEGIN _mkFn3
function _mkFn3(f) {
  return (a, b, c) => f([a, b, c]);
}
//END
//BEGIN _String_EQUAL
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
//END
//BEGIN MIN_INT32
const MIN_INT32 = -0x80000000;
//END
//BEGIN MAX_INT32
const MAX_INT32 = 0x7fffffff;
//END
//BEGIN _Int32_abs: MIN_INT32 _Overflow
function _Int32_abs(x) {
    if (x < 0) {
        if (x === MIN_INT32) {
            throw _Overflow;
        }
        return -x;
    } else {
        return x;
    }
}
//END
//BEGIN _Int32_negate: MIN_INT32 _Overflow
function _Int32_negate(x) {
    if (x === MIN_INT32) {
        throw _Overflow;
    }
    return (-x)|0;
}
//END
//BEGIN _Int32_add: MIN_INT32 MAX_INT32 _Overflow
function _Int32_add(x, y) {
    const z = x + y;
    if (z < MIN_INT32 || z > MAX_INT32) {
        throw _Overflow;
    }
    return z;
}
//END
//BEGIN _Int32_sub: MIN_INT32 MAX_INT32 _Overflow
function _Int32_sub(x, y) {
    const z = x - y;
    if (z < MIN_INT32 || z > MAX_INT32) {
        throw _Overflow;
    }
    return z;
}
//END
//BEGIN _Int32_mul: MIN_INT32 MAX_INT32 _Overflow
function _Int32_mul(x, y) {
    const z = x * y;
    if (z < MIN_INT32 || z > MAX_INT32) {
        throw _Overflow;
    }
    return z|0;
}
//END
//BEGIN _Int32_div: _Div MIN_INT32 _Overflow
function _Int32_div(x, y) {
    if (y === 0) {
        throw _Div;
    } else if (x === MIN_INT32 && y === -1) {
        throw _Overflow;
    }
    return Math.floor(x / y)|0;
}
//END
//BEGIN _Int32_mod: _Div
function _Int32_mod(x, y) {
    if (y === 0) {
        throw _Div;
    }
    return x - Math.floor(x / y) * y;
}
//END
//BEGIN _Int32_quot: MIN_INT32 _Overflow
function _Int32_quot(x, y) {
    if (y === 0) {
        throw _Div;
    } else if (x === MIN_INT32 && y === -1) {
        throw _Overflow;
    }
    return (x / y)|0;
}
//END
//BEGIN MIN_INT54
const MIN_INT54 = -0x20000000000000;
//END
//BEGIN MAX_INT54
const MAX_INT54 = 0x1fffffffffffff;
//END
//BEGIN _Int54_abs: MIN_INT54 _Overflow
function _Int54_abs(x) {
    if (x < 0) {
        if (x === MIN_INT54) {
            throw _Overflow;
        }
        return -x;
    } else {
        return x;
    }
}
//END
//BEGIN _Int54_negate: MIN_INT54 _Overflow
function _Int54_negate(x) {
    if (x === MIN_INT54) {
        throw _Overflow;
    }
    return 0 - x; // Avoid -0
}
//END
//BEGIN _Int54_add: MIN_INT54 MAX_INT54 _Overflow
function _Int54_add(x, y) {
    const z = x + y;
    if ((MIN_INT54 < z && z <= MAX_INT54) || (z === MIN_INT54 && (x & 1) === (y & 1))) {
        return z;
    } else {
        throw _Overflow;
    }
}
//END
//BEGIN _Int54_sub: MIN_INT54 MAX_INT54 _Overflow
function _Int54_sub(x, y) {
    const z = x - y;
    if ((MIN_INT54 < z && z <= MAX_INT54) || (z === MIN_INT54 && (x & 1) === (y & 1))) {
        return z;
    } else {
        throw _Overflow;
    }
}
//END
//BEGIN _Int54_mul: MIN_INT54 MAX_INT54 _Overflow
function _Int54_mul(x, y) {
    const z = 0 + x * y;
    if ((MIN_INT54 < z && z <= MAX_INT54) || (z === MIN_INT54 && ((x & 1) === 0 || (y & 1) === 0))) {
        return z;
    } else {
        throw _Overflow;
    }
}
//END
//BEGIN _Int54_div: _Div MIN_INT54 _Overflow
function _Int54_div(x, y) {
    if (y === 0) {
        throw _Div;
    } else if (x === MIN_INT54 && y === -1) {
        throw _Overflow;
    } else {
        return 0 + Math.floor(x / y);
    }
}
//END
//BEGIN _Int54_mod: _Div
function _Int54_mod(x, y) {
    if (y === 0) {
        throw _Div;
    } else {
        const r = 0 + x % y;
        if (r === 0 || x * y >= 0) {
            return r;
        } else {
            return r + y;
        }
    }
}
//END
//BEGIN _Int54_quot: _Div MIN_INT54 _Overflow
function _Int54_quot(x, y) {
    if (y === 0) {
        throw _Div;
    } else if (x === MIN_INT54 && y === -1) {
        throw _Overflow;
    } else {
        return 0 + Math.trunc(x / y);
    }
}
//END
//BEGIN _Int54_rem: _Div
function _Int54_rem(x, y) {
    if (y === 0) {
        throw _Div;
    } else {
        return 0 + x % y;
    }
}
//END
//BEGIN Math_abs
const Math_abs = Math.abs;
//END
//BEGIN Math_imul
const Math_imul = Math.imul;
//END
//BEGIN _encodeUtf8
function _encodeUtf8(s) {
    const encoder = new TextEncoder();
    return encoder.encode(s);
}
//END
//BEGIN _decodeUtf8
function _decodeUtf8(s) {
    const decoder = new TextDecoder();
    return decoder.decode(s);
}
//END
//BEGIN _exnName: _encodeUtf8
function _exnName(e) { return _encodeUtf8(e.name); }
//END
//BEGIN _String_LT
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
//END
//BEGIN _String_append
function _String_append(a, b) {
    if (a.length === 0) { return b; }
    if (b.length === 0) { return a; }
    const c = new Uint8Array(a.length + b.length);
    c.set(a);
    c.set(b, a.length);
    return c;
}
//END
//BEGIN _String_concat
function _String_concat(xs) {
    let n = 0;
    let xs0 = xs;
    while (xs0 !== null) {
        const s = xs0[0];
        n += s.length;
        xs0 = xs0[1];
    }
    const a = new Uint8Array(n);
    let m = 0;
    while (xs !== null) {
        const s = xs[0];
        a.set(s, m);
        m += s.length;
        xs = xs[1];
    }
    return a;
}
//END
//BEGIN _String_concatWith
function _String_concatWith(sep, xs) {
    let n = 0;
    let xs0 = xs;
    while (xs0 !== null) {
        const s = xs0[0];
        n += s.length;
        xs0 = xs0[1];
        if (xs0 !== null) {
            n += sep.length;
        }
    }
    const a = new Uint8Array(n);
    let m = 0;
    while (xs !== null) {
        const s = xs[0];
        a.set(s, m);
        m += s.length;
        xs = xs[1];
        if (xs !== null) {
            a.set(sep, m);
            m += sep.length;
        }
    }
    return a;
}
//END
//BEGIN _String_implode
function _String_implode(xs) {
    let n = 0;
    let xs0 = xs;
    while (xs0 !== null) {
        ++n;
        xs0 = xs0[1];
    }
    const a = new Uint8Array(n);
    let i = 0;
    while (xs !== null) {
        a[i] = xs[0];
        xs = xs[1];
        ++i;
    }
    return a;
}
//END
//BEGIN _String_translate
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
//END
//BEGIN _String32_append
function _String32_append(a, b) {
    if (a.length === 0) { return b; }
    if (b.length === 0) { return a; }
    const c = new Int32Array(a.length + b.length);
    c.set(a);
    c.set(b, a.length);
    return c;
}
//END
//BEGIN _String32_concat
function _String32_concat(xs) {
    let n = 0;
    let xs0 = xs;
    while (xs0 !== null) {
        const s = xs0[0];
        n += s.length;
        xs0 = xs0[1];
    }
    const a = new Int32Array(n);
    let m = 0;
    while (xs !== null) {
        const s = xs[0];
        a.set(s, m);
        m += s.length;
        xs = xs[1];
    }
    return a;
}
//END
//BEGIN _String32_concatWith
function _String32_concatWith(sep, xs) {
    let n = 0;
    let xs0 = xs;
    while (xs0 !== null) {
        const s = xs0[0];
        n += s.length;
        xs0 = xs0[1];
        if (xs0 !== null) {
            n += sep.length;
        }
    }
    const a = new Int32Array(n);
    let m = 0;
    while (xs !== null) {
        const s = xs[0];
        a.set(s, m);
        m += s.length;
        xs = xs[1];
        if (xs !== null) {
            a.set(sep, m);
            m += sep.length;
        }
    }
    return a;
}
//END
//BEGIN _String32_implode
function _String32_implode(xs) {
    let n = 0;
    let xs0 = xs;
    while (xs0 !== null) {
        ++n;
        xs0 = xs0[1];
    }
    const a = new Int32Array(n);
    let i = 0;
    while (xs !== null) {
        a[i] = xs[0];
        xs = xs[1];
        ++i;
    }
    return a;
}
//END
//BEGIN _String32_translate
function _String32_translate(f, s) {
    const m = s.length;
    const a = new Array(m);
    let n = 0;
    for (let i = 0; i < m; ++i) {
        const t = f(s[i]);
        a[i] = t;
        n += t.length;
    }
    const r = new Int32Array(n);
    let l = 0;
    for (let i = 0; i < m; ++i) {
        r.set(a[i], l);
        l += a[i].length;
    }
    return r;
}
//END
//BEGIN _UTF16_LT
function _UTF16_LT(a, b) {
    let i = 0;
    const m = a.length, n = b.length;
    while (i < m && i < n) {
        const x = a.codePointAt(i), y = b.codePointAt(i);
        if (x !== y) {
            return x < y;
        }
        i += 1 + (x >= 0x10000);
    }
    return m < n;
}
//END
//BEGIN _UTF16_size
function _UTF16_size(a) {
    const n = a.length;
    let l = 0;
    for (let i = 0; i < n; ) {
        const x = a.codePointAt(i);
        i += 1 + (x >= 0x10000);
        l += 1;
    }
    return l;
}
//END
//BEGIN _UTF16_isWellFormed
const _UTF16_isWellFormed =
    typeof "".isWellFormed === "function"
    ? (s => s.isWellFormed())
    : (s => s.search(/[\uD800-\uDFFF]/u) === -1);
//END
//BEGIN _UTF16_toWellFormed
const _UTF16_toWellFormed =
    typeof "".toWellFormed === "function"
    ? (s => s.toWellFormed())
    : (s => s.replace(/[\uD800-\uDFFF]/gu, "\uFFFD"));
//END
//BEGIN _Array_array: _Size
function _Array_array(n, init) {
    if (n < 0 || n > 0xffffffff) {
        throw _Size;
    }
    const a = new Array(n);
    a.fill(init);
    return a;
}
//END
//BEGIN _VectorOrArray_fromList
function _VectorOrArray_fromList(xs) {
    const a = [];
    while (xs !== null) {
        a.push(xs[0]);
        xs = xs[1];
    }
    return a;
}
//END
//BEGIN _Vector_unsafeFromListRevN: _Size
function _Vector_unsafeFromListRevN(n, xs) {
    if (n > 0xffffffff) {
        throw _Size;
    }
    const a = new Array(n);
    let i = n - 1;
    while (xs !== null) {
        a[i] = xs[0];
        xs = xs[1];
        --i;
    }
    // i should be -1 here
    return a;
}
//END
//BEGIN _VectorOrArray_tabulate: _Size
function _VectorOrArray_tabulate(n, f) {
    if (n < 0 || n > 0xffffffff) {
        throw _Size;
    }
    const a = new Array(n);
    for (let i = 0; i < n; ++i) {
        a[i] = f(i);
    }
    return a;
}
//END
//BEGIN _Vector_concat: _Size
function _Vector_concat(xs) {
    let n = 0;
    let xs0 = xs;
    while (xs0 !== null) {
        const v = xs0[0];
        n += v.length;
        xs0 = xs0[1];
    }
    if (n > 0xffffffff) {
        throw _Size;
    }
    const a = new Array(n);
    let m = 0;
    while (xs !== null) {
        const v = xs[0];
        for (let i = 0; i < v.length; ++i) {
            a[m++] = v[i];
        }
        xs = xs[1];
    }
    return a;
}
//END
//BEGIN _ThenableWrapper
function _ThenableWrapper(wrapped) {
    this.payload = wrapped;
}
//END
//BEGIN _wrapThenable: _ThenableWrapper
function _wrapThenable(x) {
    if (x != null && (typeof x === "object" || typeof x === "function")) {
        try {
            if (typeof x.then === "function") {
                return new _ThenableWrapper(x);
            }
        } catch {
        }
    }
    return x;
}
//END
//BEGIN _unwrapThenable: _ThenableWrapper
function _unwrapThenable(x) {
    return x instanceof _ThenableWrapper ? x.payload : x;
}
//END
/*
type Trampoline<Result> = [true, Result] | exists Args. [false, MLFunction<Result, Args>, Args]
interface MLFunction<Result, Args> {
    (...args: Args): Result;
    _MLTAIL_?: (...args: Args) => Trampoline<Result>;
}
*/
//BEGIN _wrap
function _wrap(f) {
    const F = function() {
        let result = f.apply(undefined, arguments); // [true, Result] | exists X. [false, MLFunction<Result, X>, X]
        while (!result[0]) {
            const g = result[1];
            if ("_MLTAIL_" in g) {
                result = g._MLTAIL_.apply(undefined, result[2]);
            } else {
                return g.apply(undefined, result[2]);
            }
        }
        return result[1];
    }
    F._MLTAIL_ = f;
    return F;
}
//END
//BEGIN _function
function _function(f) {
    return function() {
        return f(arguments);
    };
}
//END
