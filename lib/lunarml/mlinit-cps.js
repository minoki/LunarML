"use strict";
function _list(a) {
    let x = null;
    for (let i = a.length - 1; i >= 0; --i) {
        x = [a[i], x];
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
function _Fail(k, payload) { return [false, k, [new _Fail_tag(payload)]]; }
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
function _Int32_abs(k, x) {
    if (x < 0) {
        if (x === MIN_INT32) {
            throw _Overflow;
        }
        return [false, k, [-x]];
    } else {
        return [false, k, [x]];
    }
}
function _Int32_negate(k, x) {
    if (x === MIN_INT32) {
        throw _Overflow;
    }
    return [false, k, [(-x)|0]];
}
function _Int32_add(x, y) {
    const z = x + y;
    if (z < MIN_INT32 || z > MAX_INT32) {
        throw _Overflow;
    }
    return z;
}
function _Int32_sub(x, y) {
    const z = x - y;
    if (z < MIN_INT32 || z > MAX_INT32) {
        throw _Overflow;
    }
    return z;
}
function _Int32_mul(x, y) {
    const z = x * y;
    if (z < MIN_INT32 || z > MAX_INT32) {
        throw _Overflow;
    }
    return z|0;
}
function _Int32_div(x, y) {
    if (y === 0) {
        throw _Div;
    } else if (x === MIN_INT32 && y === -1) {
        throw _Overflow;
    }
    return Math.floor(x / y)|0;
}
function _Int32_mod(x, y) {
    if (y === 0) {
        throw _Div;
    }
    return x - Math.floor(x / y) * y;
}
function _Int32_quot(x, y) {
    if (y === 0) {
        throw _Div;
    } else if (x === MIN_INT32 && y === -1) {
        throw _Overflow;
    }
    return (x / y)|0;
}
function _Int32_rem(x, y) {
    if (y === 0) {
        throw _Div;
    }
    return (x % y)|0;
}
function _Word32_div(x, y) {
    if (y === 0) {
        throw _Div;
    }
    return (x / y)>>>0;
}
function _Word32_mod(x, y) {
    if (y === 0) {
        throw _Div;
    }
    return (x % y)>>>0;
}
const Math_abs = Math.abs;
function _Real_abs(k, x) {
    return [false, k, [Math_abs(x)]];
}
const Math_imul = Math.imul;
function _encodeUtf8(k, s) {
    const encoder = new TextEncoder();
    return [false, k, [encoder.encode(s)]];
}
function _decodeUtf8(k, s) {
    const decoder = new TextDecoder();
    return [false, k, [decoder.decode(s)]];
}
function _exnName(k, e) { return _encodeUtf8(k, e.name); }
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
function _Array_array(k, t) {
    const n = t[0], init = t[1];
    if (n < 0) {
        throw _Size;
    }
    const a = new Array(n);
    a.fill(init);
    return [false, k, [a]];
}
function _VectorOrArray_fromList(k, xs) {
    const a = [];
    while (xs !== null) {
        a.push(xs[0]);
        xs = xs[1];
    }
    return [false, k, [a]];
}
function _Vector_unsafeFromListRevN(n, xs) {
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
function _Vector_concat(k, xs) {
    let n = 0;
    let xs0 = xs;
    while (xs0 !== null) {
        const v = xs0[0];
        n += v.length;
        xs0 = xs0[1];
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
function _PromptTag() {
}
function _newPromptTag() {
    return new _PromptTag();
}
const _topLevel = new _PromptTag();
const _emptySeq = { tag: "nil" };
function _consPrompt(p, xs) {
    return { tag: "prompt", p: p, tail: xs };
}
function _consCont(k, exh, xs) {
    return { tag: "cont", k: k, exh: exh, tail: xs }
}
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
let _metaCont = _emptySeq;
let _exh = undefined;
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
function _pushPrompt(k, p, f) {
    /* f : unit -> 'a */
    _metaCont = _consPrompt(p, _consCont(k, _exh, _metaCont));
    _exh = _throw;
    return [false, f, [_underflow, undefined]];
}
function _withSubCont(k, p, f) {
    /* f : ('a,'b) subcont -> 'b */
    const [aboveP, belowP] = _splitSeq(p, _metaCont);
    const exh_old = _exh;
    _metaCont = belowP;
    _exh = _throw;
    return [false, f, [_underflow, _consCont(k, exh_old, aboveP)]];
}
function _pushSubCont(k, subcont, f) {
    /* f : unit -> 'a */
    _metaCont = _appendSeq(subcont, _consCont(k, _exh, _metaCont));
    _exh = _throw;
    return [false, f, [_underflow, undefined]];
}
function _run(f) {
    const metaCont_old = _metaCont;
    const exh_old = _exh;
    _metaCont = _consPrompt(_topLevel, _emptySeq);
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
            if (typeof _exh === "undefiend") {
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
                if (typeof _exh === "undefiend") {
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
