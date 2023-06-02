# Language extensions

## Vector expressions and patterns

Prior art:

* [SML/NJ](https://www.smlnj.org/doc/features.html)
* [MLton](http://mlton.org/SuccessorML)
* [Moscow ML](https://mosml.org/mosmlref.pdf)

`#[exp_0, exp_1, …, exp_{n-1}]` is equivalent to `Vector.fromList [exp_0, exp_1, ..., exp_{n-1}]` (with built-in value of `Vector.fromList`) except that the vector expression is non-expansive if every `exp_i` is non-expansive.

`#[pat_0, pat_1, …, pat_{n-1}]` matches a vector `v` if `Vector.length v = n` and for each `i`, `pat_i` matches `Vector.sub (v, i)` (with built-in values of `Vector.length` and `Vector.sub`).

`#[pat_0, pat_1, …, pat_{n-1}, ...]` (the last `...` is verbatim) matches a vector `v` if `Vector.length v >= n` and for each `i`, `pat_i` matches `Vector.sub (v, i)` (with built-in values of `Vector.length` and `Vector.sub`).

## Hexadecimal floating-point constants

Examples: `0x1p~1022`, `0x1.ffff_ffff_ffff_f`

The syntax of hexadecimal floating-point constants is:

```
<hexadecimal-integer-constant> ::= '~'? '0' 'w'? 'x' <hexadecimal-digit-sequence>
<hexadecimal-floating-point-constant> ::= '~'? '0x' <hexadecimal-digit-sequence> (<binary-exponent-part> | '.' <hexadecimal-digit-sequence> <binary-exponent-part>?)
<hexadecimal-digit-sequence> ::= <hexadecimal-digit> ('_'* <hexadecimal-digit>)*
<binary-exponent-part> ::= [pP] '~'? <digit> ('_'* <digit>)?
```

In short: the (binary) exponent part is optional and use tilde (`~`) for the negation symbol.

The hexadecimal floating-point constants must be exact; `0x1.0000_0000_0000_01p0` and `0x1p1024` are examples of invalid constants (assuming the type is `Real64.real`).

## UTF-encoded escape sequence in text constants

The `\u{}` escape sequence allows you to embed a Unicode scalar value in a text constant.
The compiler encodes the character in UTF-8/16/32, depending on the type.

The scalar value is expressed in hexadecimal format; `\u{3B1}` or `\u{3b1}` for U+03B1 GREEK SMALL LETTER ALPHA.
Underscores are not allowed between hexadecimal digits.

When `\u{}` is used in a character constant, the character must be encoded as a single code unit in the corresponding UTF.

Examples:

```sml
("\u{3042}" : string) = "\227\129\130"
("\u{80}" : string) = "\194\128"
("\u{1F600}" : string) = "\240\159\152\128"
if WideChar.maxOrd = 255 then
    ("\u{1F600}" : WideString.string) = "\240\159\152\128"
else if WideChar.maxOrd = 65535 then
    ("\u{1F600}" : WideString.string) = "\uD83D\uDE00"
else if WideChar.maxOrd = 1114111 then
    ("\u{1F600}" : WideString.string) = "\U0001F600"
```

Examples of invalid constants:

```sml
#"\u{80}" : char (* equivalent to #"\194\128" *)
if WideChar.maxOrd = 65535 then
    #"\u{10000}" : WideChar.char (* equivalent to #"\uD800\uDC00" *)
```

## Infixing dot

To use this extension, `allowInfixingDot` annotation in MLB file is needed:

```
ann "allowInfixingDot true" in
...
end
```

`infexp_1 .longvid. infexp_2` is equivalent to `op longvid (infexp_1, infexp_2)`.

`pat_1 .longvid. pat_2` is equivalent to `op longvid (pat_1, pat_2)`.

Associativity of `.longvid.` is currently `infix 0`, but I plan to allow overriding.

Examples:

```sml
0wxdead .Word.andb. 0wxbeef; (* equivalent to Word.andb (0wxdead, 0wxbeef) *)
fun a .foo. b = print (a ^ ", " ^ b ^ "\n"); (* equivalent to fun foo (a, b) = ... *)
```

## Other extensions planned

* [ ] Packaged modules (like in Alice ML or HaMLet S)
* [ ] Unicode identifiers
