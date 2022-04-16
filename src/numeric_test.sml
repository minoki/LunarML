fun intToString n = if n < 0 then
                        "-" ^ Int.toString (~n)
                    else
                        Int.toString n;
fun notationToString (Numeric.DecimalNotation { sign, intPart, fracPart, exponent })
    = let val s = if sign then "-" else ""
          val ip = IntInf.toString intPart
          val fp = Vector.foldr (fn (x, acc) => Int.toString x ^ acc) "" fracPart
      in s ^ ip ^ (if Vector.length fracPart = 0 then "" else "." ^ fp) ^ (if exponent = 0 then "" else "e" ^ intToString exponent)
      end
  | notationToString (Numeric.HexadecimalNotation { sign, intPart, fracPart, exponent })
    = let val s = if sign then "-" else ""
          val ip = IntInf.fmt StringCvt.HEX intPart
          val fp = Vector.foldr (fn (x, acc) => Int.fmt StringCvt.HEX x ^ acc) "" fracPart
      in s ^ "0x" ^ ip ^ (if Vector.length fracPart = 0 then "" else "." ^ fp) ^ (if exponent = 0 then "" else "p" ^ intToString exponent)
      end;
fun tryConvert notation = print (notationToString notation ^ " => " ^
                                 (case Numeric.toDecimal { nominal_format = Numeric.binary64, target_format = Numeric.binary64 } notation of
                                      NONE => "NONE\n"
                                    | SOME x => "SOME " ^ notationToString x ^ "\n"
                                 )
                                );
tryConvert (Numeric.HexadecimalNotation { sign = false, intPart = 1, fracPart = vector [], exponent = ~1074 });
tryConvert (Numeric.HexadecimalNotation { sign = false, intPart = 1, fracPart = vector [1], exponent = ~1074 });
tryConvert (Numeric.HexadecimalNotation { sign = false, intPart = 2, fracPart = vector [], exponent = ~1074 });
tryConvert (Numeric.HexadecimalNotation { sign = false, intPart = 3, fracPart = vector [], exponent = ~1074 });
tryConvert (Numeric.HexadecimalNotation { sign = false, intPart = 1, fracPart = vector [], exponent = ~1022 });
tryConvert (Numeric.HexadecimalNotation { sign = false, intPart = 0, fracPart = vector [0xf, 0xf, 0xf, 0xf, 0xf, 0xf, 0xf, 0xf, 0xf, 0xf, 0xf, 0xf, 0x8], exponent = 0 });
tryConvert (Numeric.HexadecimalNotation { sign = true, intPart = 1, fracPart = vector [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1], exponent = 0 });
tryConvert (Numeric.HexadecimalNotation { sign = false, intPart = 0, fracPart = vector [0xf, 0xf, 0xf, 0xf, 0xf, 0xf, 0xf, 0xf, 0xf, 0xf, 0xf, 0xf, 0xe, 0x8], exponent = 1023 });
tryConvert (Numeric.HexadecimalNotation { sign = false, intPart = 1, fracPart = vector [0xf, 0xf, 0xf, 0xf, 0xf, 0xf, 0xf, 0xf, 0xf, 0xf, 0xf, 0xf, 0xe, 0x8], exponent = 1023 });
tryConvert (Numeric.HexadecimalNotation { sign = false, intPart = 1, fracPart = vector [0xf, 0xf, 0xf, 0xf, 0xf, 0xf, 0xf, 0xf, 0xf, 0xf, 0xf, 0xf, 0xe], exponent = 1023 });
tryConvert (Numeric.HexadecimalNotation { sign = false, intPart = 1, fracPart = vector [0xf, 0xf, 0xf, 0xf, 0xf, 0xf, 0xf, 0xf, 0xf, 0xf, 0xf, 0xf, 0xf], exponent = 1023 });
tryConvert (Numeric.HexadecimalNotation { sign = false, intPart = 1, fracPart = vector [], exponent = 1024 });
