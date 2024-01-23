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
_Match_tag.prototype.name = "Match";
//END
//BEGIN _Match: _Match_tag
const _Match = new _Match_tag();
//END
//BEGIN _Bind_tag
function _Bind_tag() {}
_Bind_tag.prototype.name = "Bind";
//END
//BEGIN _Bind: _Bind_tag
const _Bind = new _Bind_tag();
//END
//BEGIN _Div_tag
function _Div_tag() {}
_Div_tag.prototype.name = "Div";
//END
//BEGIN _Div: _Div_tag
const _Div = new _Div_tag();
//END
//BEGIN _Overflow_tag
function _Overflow_tag() {}
_Overflow_tag.prototype.name = "Overflow";
//END
//BEGIN _Overflow: _Overflow_tag
const _Overflow = new _Overflow_tag();
//END
//BEGIN _Size_tag
function _Size_tag() {}
_Size_tag.prototype.name = "Size";
//END
//BEGIN _Size: _Size_tag
const _Size = new _Size_tag();
//END
//BEGIN _Subscript_tag
function _Subscript_tag() {}
_Subscript_tag.prototype.name = "Subscript";
//END
//BEGIN _Subscript: _Subscript_tag
const _Subscript = new _Subscript_tag();
//END
//BEGIN _Fail_tag
function _Fail_tag(payload) { this.payload = payload; }
_Fail_tag.prototype.name = "Fail";
//END
//BEGIN _Fail: _Fail_tag
function _Fail(k, payload) { return [false, k, [new _Fail_tag(payload)]]; }
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
//BEGIN _Int32_quot: _Div MIN_INT32 _Overflow
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
//BEGIN _Real_abs: Math_abs
function _Real_abs(k, x) {
    return [false, k, [Math_abs(x)]];
}
//END
//BEGIN Math_imul
const Math_imul = Math.imul;
//END
//BEGIN _encodeUtf8
function _encodeUtf8(k, s) {
    const encoder = new TextEncoder();
    return [false, k, [encoder.encode(s)]];
}
//END
//BEGIN _decodeUtf8
function _decodeUtf8(k, s) {
    const decoder = new TextDecoder();
    return [false, k, [decoder.decode(s)]];
}
//END
//BEGIN _exnName: _encodeUtf8
function _exnName(k, e) { return _encodeUtf8(k, e.name); }
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
function _String_concat(k, xs) {
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
    return [false, k, [a]];
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
function _String_implode(k, xs) {
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
    return [false, k, [a]];
}
//END
//BEGIN _Array_array: _Size
function _Array_array(k, t) {
    const n = t[0], init = t[1];
    if (n < 0 || n > 0xffffffff) {
        throw _Size;
    }
    const a = new Array(n);
    a.fill(init);
    return [false, k, [a]];
}
//END
//BEGIN _VectorOrArray_fromList
function _VectorOrArray_fromList(k, xs) {
    const a = [];
    while (xs !== null) {
        a.push(xs[0]);
        xs = xs[1];
    }
    return [false, k, [a]];
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
//BEGIN _Vector_concat: _Size
function _Vector_concat(k, xs) {
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
    return [false, k, [a]];
}
//END
//BEGIN _PromptTag
function _PromptTag() {
}
//END
//BEGIN _topLevel: _PromptTag
const _topLevel = new _PromptTag();
//END
//BEGIN _emptySeq
const _emptySeq = { tag: "nil" };
//END
//BEGIN _consPrompt
function _consPrompt(p, xs) {
    return { tag: "prompt", p: p, tail: xs };
}
//END
//BEGIN _consCont
function _consCont(k, exh, xs) {
    return { tag: "cont", k: k, exh: exh, tail: xs }
}
//END
//BEGIN _appendSeq: _consCont _consPrompt
function _appendSeq(xs, ys) {
    let a = [];
    while (xs.tag !== "nil") {
        a.push(xs);
        xs = xs.tail;
    }
    let zs = ys;
    for (let i = a.length - 1; i >= 0; --i) {
        let x = a[i];
        if (x.tag === "cont") {
            zs = _consCont(x.k, x.exh, zs);
        } else {
            zs = _consPrompt(x.p, zs);
        }
    }
    return zs;
}
//END
//BEGIN _splitSeq: _emptySeq
function _splitSeq(p, xs) {
    let a = [];
    for (;;) {
        if (xs.tag === "nil") {
            throw new Error("Prompt was not found on the stack");
        } else if (xs.tag === "prompt" && xs.p === p) {
            let ys = _emptySeq;
            for (let i = a.length - 1; i >= 0; --i) {
                let x = a[i];
                if (x.tag === "cont") {
                    ys = _consCont(x.k, x.exh, ys);
                } else {
                    ys = _consPrompt(x.p, ys);
                }
            }
            return [ys, xs.tail];
        } else {
            a.push(xs);
            xs = xs.tail;
        }
    }
}
//END
//BEGIN _metaCont: _emptySeq
let _metaCont = _emptySeq;
//END
//BEGIN _exh
let _exh = undefined;
//END
//BEGIN _underflow: _metaCont _exh
function _underflow(v) {
    while (_metaCont.tag !== "nil") {
        if (_metaCont.tag === "cont") {
            const k = _metaCont.k;
            _exh = _metaCont.exh;
            _metaCont = _metaCont.tail;
            return [false, k, [v]];
        }
        _metaCont = _metaCont.tail;
    }
    return [true, v];
}
//END
//BEGIN _throw: _metaCont
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
//END
//BEGIN _pushPrompt: _metaCont _consPrompt _consCont _exh _throw _underflow
function _pushPrompt(k, p, f) {
    /* f : unit -> 'a */
    _metaCont = _consPrompt(p, _consCont(k, _exh, _metaCont));
    _exh = _throw;
    return [false, f, [_underflow, undefined]];
}
//END
//BEGIN _withSubCont: _splitSeq _metaCont _exh _throw _underflow _consCont
function _withSubCont(k, p, f) {
    /* f : ('a,'b) subcont -> 'b */
    const [aboveP, belowP] = _splitSeq(p, _metaCont);
    const exh_old = _exh;
    _metaCont = belowP;
    _exh = _throw;
    return [false, f, [_underflow, _consCont(k, exh_old, aboveP)]];
}
//END
//BEGIN _pushSubCont: _metaCont _appendSeq _consCont _exh _throw _underflow
function _pushSubCont(k, subcont, f) {
    /* f : unit -> 'a */
    _metaCont = _appendSeq(subcont, _consCont(k, _exh, _metaCont));
    _exh = _throw;
    return [false, f, [_underflow, undefined]];
}
//END
//BEGIN _run: _metaCont _exh _consPrompt _topLevel _emptySeq
function _run(f, topLevel) {
    const metaCont_old = _metaCont;
    const exh_old = _exh;
    _metaCont = topLevel ? _consPrompt(_topLevel, _emptySeq) : _emptySeq;
    _exh = undefined;
    let r;
    try {
        r = f(result => [true, result]);
    } catch (e) {
        if (typeof _exh === "undefined") {
            _metaCont = metaCont_old;
            _exh = exh_old;
            throw e;
        } else {
            r = [false, _exh, [e]];
        }
    }
    while (!r[0]) {
        try {
            while (!r[0]) {
                r = r[1].apply(undefined, r[2]);
            }
        } catch (e) {
            if (typeof _exh === "undefined") {
                _metaCont = metaCont_old;
                _exh = exh_old;
                throw e;
            } else {
                r = [false, _exh, [e]];
            }
        }
    }
    _metaCont = metaCont_old;
    _exh = exh_old;
    return r[1];
}
//END
//BEGIN _function: _metaCont _exh _emptySeq
function _function(k, f) {
    return [false, k, [function() {
        const metaCont_old = _metaCont;
        const exh_old = _exh;
        _metaCont = _emptySeq;
        _exh = undefined;
        let r;
        try {
            r = f(result => [true, result], arguments);
        } catch (e) {
            if (typeof _exh === "undefined") {
                _metaCont = metaCont_old;
                _exh = exh_old;
                throw e;
            } else {
                r = [false, _exh, [e]];
            }
        }
        while (!r[0]) {
            try {
                while (!r[0]) {
                    r = r[1].apply(undefined, r[2]);
                }
            } catch (e) {
                if (typeof _exh === "undefined") {
                    _metaCont = metaCont_old;
                    _exh = exh_old;
                    throw e;
                } else {
                    r = [false, _exh, [e]];
                }
            }
        }
        _metaCont = metaCont_old;
        _exh = exh_old;
        return r[1];
    }]];
}
//END
