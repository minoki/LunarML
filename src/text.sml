(*
 * Copyright (c) 2023 ARATA Mizuki
 * This file is part of LunarML.
 *)
structure StringElement :>
sig
  datatype char =
    CODEUNIT of int (* \ddd, \uXXXX *)
  | UNICODE_SCALAR of int (* \u{} *)
  val charToString: char -> string
  val isAsciiString: char vector -> bool
  val toAsciiString: char vector -> string option
  val fromAsciiString: string -> char vector
  val encodeAscii: string -> int vector
  val encode7bit: char vector -> string
  val encode8bit: char vector -> string
  val encode16bit: char vector -> int vector
  val encode32bit: char vector -> int vector
  val encodeUString: char vector -> int vector
end =
struct
  datatype char =
    CODEUNIT of int (* \ddd, \uXXXX *)
  | UNICODE_SCALAR of int (* \u{} *)
  fun charToString (CODEUNIT x) =
        if x < 0 then
          raise Fail "charToString: negative ordinal"
        else if x <= 127 then
          let
            val c = chr x
          in
            case c of
              #"\\" => "\\\\"
            | #"\"" => "\\\""
            | #"\a" => "\\a"
            | #"\b" => "\\b"
            | #"\t" => "\\t"
            | #"\n" => "\\n"
            | #"\v" => "\\v"
            | #"\f" => "\\f"
            | #"\r" => "\\r"
            | #" " => " "
            | _ =>
                if Char.isPrint c then String.str c
                else if x < 10 then "\\00" ^ Int.toString x
                else if x < 100 then "\\0" ^ Int.toString x
                else "\\" ^ Int.toString x
          end
        else if x <= 0xFFF then
          "\\u0" ^ Int.fmt StringCvt.HEX x
        else if x <= 0xFFFF then
          "\\u" ^ Int.fmt StringCvt.HEX x
        else if x <= 0xFFFFF then
          "\\U000" ^ Int.fmt StringCvt.HEX x
        else if x <= 0xFFFFFF then
          "\\U00" ^ Int.fmt StringCvt.HEX x
        else if x <= 0xFFFFFF then
          "\\U0" ^ Int.fmt StringCvt.HEX x
        else if x <= 0x7FFFFFF then
          "\\U" ^ Int.fmt StringCvt.HEX x
        else
          raise Fail "charToString: ordinal too large"
    | charToString (UNICODE_SCALAR x) =
        "\\u{" ^ Int.fmt StringCvt.HEX x ^ "}"
  fun isAsciiString (s: char vector) : bool =
    Vector.all
      (fn CODEUNIT x => 0 <= x andalso x < 128
        | UNICODE_SCALAR x => 0 <= x andalso x < 128) s
  fun toAsciiString (s: char vector) : string option =
    let
      val v =
        Vector.map
          (fn CODEUNIT x =>
             if 0 <= x andalso x < 128 then Char.chr x else raise Chr
            | UNICODE_SCALAR x =>
             if 0 <= x andalso x < 128 then Char.chr x else raise Chr) s
    in
      SOME (CharVector.tabulate (Vector.length v, fn i => Vector.sub (v, i)))
    end
    handle Chr => NONE
  fun fromAsciiString (s: string) : char vector =
    Vector.tabulate (String.size s, fn i =>
      CODEUNIT (Char.ord (String.sub (s, i))))
  fun encodeAscii (s: string) : int vector =
    Vector.tabulate (String.size s, fn i => Char.ord (String.sub (s, i)))
  fun encode7bit (s: char vector) : string (* may raise Chr *) =
    String.implode
      (Vector.foldr
         (fn (CODEUNIT x, xs) =>
            if 0 <= x andalso x <= 127 then Char.chr x :: xs else raise Chr
           | (UNICODE_SCALAR x, xs) =>
            if x < 128 then Char.chr x :: xs else raise Chr) [] s)
  fun encode8bit (s: char vector) : string (* may raise Chr *) =
    String.implode
      (Vector.foldr
         (fn (CODEUNIT x, xs) =>
            if 0 <= x andalso x <= 255 then Char.chr x :: xs else raise Chr
           | (UNICODE_SCALAR x, xs) =>
            if x < 128 then
              Char.chr x :: xs
            else if x < 0x800 then
              let
                val x = Word.fromInt x
                val u0 = Word.orb (0wxc0, Word.>> (x, 0w6))
                val u1 = Word.orb (0wx80, Word.andb (x, 0wx3f))
              in
                Char.chr (Word.toInt u0) :: Char.chr (Word.toInt u1) :: xs
              end
            else if x < 0x10000 then
              let
                val x = Word.fromInt x
                val u0 = Word.orb (0wxe0, Word.>> (x, 0w12))
                val u1 = Word.orb (0wx80, Word.andb (Word.>> (x, 0w6), 0wx3f))
                val u2 = Word.orb (0wx80, Word.andb (x, 0wx3f))
              in
                Char.chr (Word.toInt u0) :: Char.chr (Word.toInt u1)
                :: Char.chr (Word.toInt u2) :: xs
              end
            else
              let
                val x = Word.fromInt x
                val u0 = Word.orb (0wxf0, Word.>> (x, 0w18))
                val u1 = Word.orb (0wx80, Word.andb (Word.>> (x, 0w12), 0wx3f))
                val u2 = Word.orb (0wx80, Word.andb (Word.>> (x, 0w6), 0wx3f))
                val u3 = Word.orb (0wx80, Word.andb (x, 0wx3f))
              in
                Char.chr (Word.toInt u0) :: Char.chr (Word.toInt u1)
                :: Char.chr (Word.toInt u2) :: Char.chr (Word.toInt u3) :: xs
              end) [] s)
  fun encode16bit (s: char vector) : int vector (* may raise Chr *) =
    Vector.fromList
      (Vector.foldr
         (fn (CODEUNIT x, xs) =>
            if 0 <= x andalso x <= 0xffff then x :: xs else raise Chr
           | (UNICODE_SCALAR x, xs) =>
            if x <= 0xffff then
              x :: xs
            else
              let
                val y = Word.fromInt x - 0wx10000
                val lead = Word.orb (0wxD800, Word.>> (y, 0w10))
                val trail = Word.orb (0wxDC00, Word.andb (y, 0wx3ff))
              in
                Word.toInt lead :: Word.toInt trail :: xs
              end) [] s)
  fun encode32bit (s: char vector) : int vector (* may raise Chr *) =
    Vector.fromList
      (Vector.foldr
         (fn (CODEUNIT x, xs) =>
            if 0 <= x andalso x <= 0x10ffff then x :: xs else raise Chr
           | (UNICODE_SCALAR x, xs) => x :: xs) [] s)
  fun encodeUString (s: char vector) : int vector (* may raise Chr *) =
    Vector.fromList
      (Vector.foldr
         (fn (CODEUNIT x, xs) =>
            if
              (0 <= x andalso x <= 0xd7ff)
              orelse (0xe000 <= x andalso x <= 0x10ffff)
            then x :: xs
            else raise Chr
           | (UNICODE_SCALAR x, xs) => x :: xs) [] s)
end;
