infix 4 ==
val op == = Real.==
val NaN = 0.0 / 0.0;
fun sameValue (x, y) = if x == 0.0 andalso y == 0.0 then
                           Real.signBit x = Real.signBit y
                       else
                           x == y orelse (Real.isNan x andalso Real.isNan y);
datatype 'a result = VALUE of 'a
                   | OVERFLOW
                   | DOMAIN
fun resultToString (VALUE x) = LargeInt.toString x
  | resultToString OVERFLOW = "Overflow"
  | resultToString DOMAIN = "DOMAIN"
val maxFiniteAsLargeInt = IntInf.<< (0x1_ffff_ffff_ffff_f, 0w1023 - 0w52);
val () = List.app (fn (s, x, { round, floor, ceil, trunc }) =>
                      ( let val actual = VALUE (Real.toLargeInt IEEEReal.TO_NEAREST x)
                                         handle Overflow => OVERFLOW
                                              | Domain => DOMAIN
                        in if round = actual then
                               print ("toLargeInt TO_NEAREST " ^ s ^ ": OK\n")
                           else
                               print ("toLargeInt TO_NEAREST " ^ s ^ ": mismatch (expected " ^ resultToString round ^ ", but got " ^ resultToString actual ^ ")\n")
                        end
                      ; let val actual = VALUE (Real.toLargeInt IEEEReal.TO_NEGINF x)
                                         handle Overflow => OVERFLOW
                                              | Domain => DOMAIN
                        in if floor = actual then
                               print ("toLargeInt TO_NEGINF " ^ s ^ ": OK\n")
                           else
                               print ("toLargeInt TO_NEGINF " ^ s ^ ": mismatch (expected " ^ resultToString floor ^ ", but got " ^ resultToString actual ^ ")\n")
                        end
                      ; let val actual = VALUE (Real.toLargeInt IEEEReal.TO_POSINF x)
                                         handle Overflow => OVERFLOW
                                              | Domain => DOMAIN
                        in if ceil = actual then
                               print ("toLargeInt TO_POSINF " ^ s ^ ": OK\n")
                           else
                               print ("toLargeInt TO_POSINF " ^ s ^ ": mismatch (expected " ^ resultToString ceil ^ ", but got " ^ resultToString actual ^ ")\n")
                        end
                      ; let val actual = VALUE (Real.toLargeInt IEEEReal.TO_ZERO x)
                                         handle Overflow => OVERFLOW
                                              | Domain => DOMAIN
                        in if trunc = actual then
                               print ("toLargeInt TO_ZERO " ^ s ^ ": OK\n")
                           else
                               print ("toLargeInt TO_ZERO " ^ s ^ ": mismatch (expected " ^ resultToString trunc ^ ", but got " ^ resultToString actual ^ ")\n")
                        end
                      )
                  )
                  [("negInf", Real.negInf, { round = OVERFLOW, floor = OVERFLOW, ceil = OVERFLOW, trunc = OVERFLOW })
                  ,("~maxFinite", ~Real.maxFinite, { round = VALUE (~maxFiniteAsLargeInt), floor = VALUE (~maxFiniteAsLargeInt), ceil = VALUE (~maxFiniteAsLargeInt), trunc = VALUE (~maxFiniteAsLargeInt) })
                  ,("~3.5", ~3.5, { round = VALUE ~4, floor = VALUE ~4, ceil = VALUE ~3, trunc = VALUE ~3 })
                  ,("~3.25", ~3.25, { round = VALUE ~3, floor = VALUE ~4, ceil = VALUE ~3, trunc = VALUE ~3 })
                  ,("~3.0", ~3.0, { round = VALUE ~3, floor = VALUE ~3, ceil = VALUE ~3, trunc = VALUE ~3 })
                  ,("~2.75", ~2.75, { round = VALUE ~3, floor = VALUE ~3, ceil = VALUE ~2, trunc = VALUE ~2 })
                  ,("~2.5", ~2.5, { round = VALUE ~2, floor = VALUE ~3, ceil = VALUE ~2, trunc = VALUE ~2 })
                  ,("~2.25", ~2.25, { round = VALUE ~2, floor = VALUE ~3, ceil = VALUE ~2, trunc = VALUE ~2 })
                  ,("~2.0", ~2.0, { round = VALUE ~2, floor = VALUE ~2, ceil = VALUE ~2, trunc = VALUE ~2 })
                  ,("~1.75", ~1.75, { round = VALUE ~2, floor = VALUE ~2, ceil = VALUE ~1, trunc = VALUE ~1 })
                  ,("~1.5", ~1.5, { round = VALUE ~2, floor = VALUE ~2, ceil = VALUE ~1, trunc = VALUE ~1 })
                  ,("~1.25", ~1.25, { round = VALUE ~1, floor = VALUE ~2, ceil = VALUE ~1, trunc = VALUE ~1 })
                  ,("~1.0", ~1.0, { round = VALUE ~1, floor = VALUE ~1, ceil = VALUE ~1, trunc = VALUE ~1 })
                  ,("~0.75", ~0.75, { round = VALUE ~1, floor = VALUE ~1, ceil = VALUE 0, trunc = VALUE 0 })
                  ,("~0.5", ~0.5, { round = VALUE 0, floor = VALUE ~1, ceil = VALUE 0, trunc = VALUE 0 })
                  ,("~0.25", ~0.25, { round = VALUE 0, floor = VALUE ~1, ceil = VALUE 0, trunc = VALUE 0 })
                  ,("~minNormalPos", ~Real.minNormalPos, { round = VALUE 0, floor = VALUE ~1, ceil = VALUE 0, trunc = VALUE 0 })
                  ,("~minPos", ~Real.minPos, { round = VALUE 0, floor = VALUE ~1, ceil = VALUE 0, trunc = VALUE 0 })
                  ,("~0.0", ~0.0, { round = VALUE 0, floor = VALUE 0, ceil = VALUE 0, trunc = VALUE 0 })
                  ,("0.0", 0.0, { round = VALUE 0, floor = VALUE 0, ceil = VALUE 0, trunc = VALUE 0 })
                  ,("minPos", Real.minPos, { round = VALUE 0, floor = VALUE 0, ceil = VALUE 1, trunc = VALUE 0 })
                  ,("minNormalPos", Real.minNormalPos, { round = VALUE 0, floor = VALUE 0, ceil = VALUE 1, trunc = VALUE 0 })
                  ,("0.25", 0.25, { round = VALUE 0, floor = VALUE 0, ceil = VALUE 1, trunc = VALUE 0 })
                  ,("0.5", 0.5, { round = VALUE 0, floor = VALUE 0, ceil = VALUE 1, trunc = VALUE 0 })
                  ,("0.75", 0.75, { round = VALUE 1, floor = VALUE 0, ceil = VALUE 1, trunc = VALUE 0 })
                  ,("1.0", 1.0, { round = VALUE 1, floor = VALUE 1, ceil = VALUE 1, trunc = VALUE 1 })
                  ,("1.25", 1.25, { round = VALUE 1, floor = VALUE 1, ceil = VALUE 2, trunc = VALUE 1 })
                  ,("1.5", 1.5, { round = VALUE 2, floor = VALUE 1, ceil = VALUE 2, trunc = VALUE 1 })
                  ,("1.75", 1.75, { round = VALUE 2, floor = VALUE 1, ceil = VALUE 2, trunc = VALUE 1 })
                  ,("2.0", 2.0, { round = VALUE 2, floor = VALUE 2, ceil = VALUE 2, trunc = VALUE 2 })
                  ,("2.25", 2.25, { round = VALUE 2, floor = VALUE 2, ceil = VALUE 3, trunc = VALUE 2 })
                  ,("2.5", 2.5, { round = VALUE 2, floor = VALUE 2, ceil = VALUE 3, trunc = VALUE 2 })
                  ,("2.75", 2.75, { round = VALUE 3, floor = VALUE 2, ceil = VALUE 3, trunc = VALUE 2 })
                  ,("3.0", 3.0, { round = VALUE 3, floor = VALUE 3, ceil = VALUE 3, trunc = VALUE 3 })
                  ,("3.25", 3.25, { round = VALUE 3, floor = VALUE 3, ceil = VALUE 4, trunc = VALUE 3 })
                  ,("3.5", 3.5, { round = VALUE 4, floor = VALUE 3, ceil = VALUE 4, trunc = VALUE 3 })
                  ,("maxFinite", Real.maxFinite, { round = VALUE maxFiniteAsLargeInt, floor = VALUE maxFiniteAsLargeInt, ceil = VALUE maxFiniteAsLargeInt, trunc = VALUE maxFiniteAsLargeInt })
                  ,("posInf", Real.posInf, { round = OVERFLOW, floor = OVERFLOW, ceil = OVERFLOW, trunc = OVERFLOW })
                  ,("NaN", NaN, { round = DOMAIN, floor = DOMAIN, ceil = DOMAIN, trunc = DOMAIN })
                  ];
val () = List.app (fn (s, i : IntInf.int, expected) =>
                      let val actual = Real.fromLargeInt i
                      in if sameValue (expected, actual) then
                             print ("fromLargeInt " ^ s ^ ": OK\n")
                         else
                             print ("fromLargeInt " ^ s ^ ": mismatch (expected " ^ Real.fmt (StringCvt.GEN (SOME 17)) expected ^ ", but got " ^ Real.fmt (StringCvt.GEN (SOME 17)) actual ^ ")\n")
                      end
                  )
                  [("0", 0, 0.0)
                  ,("1", 1, 1.0)
                  ,("~1", ~1, ~1.0)
                  ,("~0xDEAD_BEEF", ~0xDEAD_BEEF, ~0xDEAD_BEEFp0)
                  ,("0xCAFE_CAFE_CAFE", 0xCAFE_CAFE_CAFE, 0xCAFE_CAFE_CAFEp0)
                  ,("0xCAFE_CAFE_CAFE_0000", 0xCAFE_CAFE_CAFE_0000, 0xCAFE_CAFE_CAFEp16)
                  ,("0xFFFF_FFFF_FFFF_FC", 0xFFFF_FFFF_FFFF_FC, 0x1p56)
                  ,("0xFFFF_FFFF_FFFF_FC0", 0xFFFF_FFFF_FFFF_FC0, 0x1p60)
                  ,("0xFFFF_FFFF_FFFF_FBFF", 0xFFFF_FFFF_FFFF_FBFF, 0x0.FFFF_FFFF_FFFF_F8p64)
                  ,("0xFFFF_FFFF_FFFF_FC00", 0xFFFF_FFFF_FFFF_FC00, 0x1p64)
                  ,("0xFFFF_FFFF_FFFF_FC01", 0xFFFF_FFFF_FFFF_FC01, 0x1p64)
                  ,("0xFFFF_FFFF_FFFF_FBFF_F", 0xFFFF_FFFF_FFFF_FBFF_F, 0x0.FFFF_FFFF_FFFF_F8p68)
                  ,("0xFFFF_FFFF_FFFF_FC00_0", 0xFFFF_FFFF_FFFF_FC00_0, 0x1p68)
                  ,("0xFFFF_FFFF_FFFF_FC00_1", 0xFFFF_FFFF_FFFF_FC00_1, 0x1p68)
                  ,("~0xFFFF_FFFF_FFFF_FC", ~0xFFFF_FFFF_FFFF_FC, ~0x1p56)
                  ,("~0xFFFF_FFFF_FFFF_FC0", ~0xFFFF_FFFF_FFFF_FC0, ~0x1p60)
                  ,("~0xFFFF_FFFF_FFFF_FC00", ~0xFFFF_FFFF_FFFF_FC00, ~0x1p64)
                  ,("0xFFFF_FFFF_FFFF_F4", 0xFFFF_FFFF_FFFF_F4, 0x0.FFFF_FFFF_FFFF_Fp56)
                  ,("0xFFFF_FFFF_FFFF_F40", 0xFFFF_FFFF_FFFF_F40, 0x0.FFFF_FFFF_FFFF_Fp60)
                  ,("0xFFFF_FFFF_FFFF_F3FF", 0xFFFF_FFFF_FFFF_F3FF, 0x0.FFFF_FFFF_FFFF_Fp64)
                  ,("0xFFFF_FFFF_FFFF_F400", 0xFFFF_FFFF_FFFF_F400, 0x0.FFFF_FFFF_FFFF_Fp64)
                  ,("0xFFFF_FFFF_FFFF_F401", 0xFFFF_FFFF_FFFF_F401, 0x0.FFFF_FFFF_FFFF_F8p64)
                  ,("0xFFFF_FFFF_FFFF_F3FF_F", 0xFFFF_FFFF_FFFF_F3FF_F, 0x0.FFFF_FFFF_FFFF_Fp68)
                  ,("0xFFFF_FFFF_FFFF_F400_0", 0xFFFF_FFFF_FFFF_F400_0, 0x0.FFFF_FFFF_FFFF_Fp68)
                  ,("0xFFFF_FFFF_FFFF_F400_1", 0xFFFF_FFFF_FFFF_F400_1, 0x0.FFFF_FFFF_FFFF_F8p68)
                  ,("~0xFFFF_FFFF_FFFF_F4", ~0xFFFF_FFFF_FFFF_F4, ~0x0.FFFF_FFFF_FFFF_Fp56)
                  ,("~0xFFFF_FFFF_FFFF_F40", ~0xFFFF_FFFF_FFFF_F40, ~0x0.FFFF_FFFF_FFFF_Fp60)
                  ,("~0xFFFF_FFFF_FFFF_F400", ~0xFFFF_FFFF_FFFF_F400, ~0x0.FFFF_FFFF_FFFF_Fp64)
                  ,("~0xFFFF_FFFF_FFFF_F3FF_FF", ~0xFFFF_FFFF_FFFF_F3FF_FF, ~0x0.FFFF_FFFF_FFFF_Fp72)
                  ,("~0xFFFF_FFFF_FFFF_F400_00", ~0xFFFF_FFFF_FFFF_F400_00, ~0x0.FFFF_FFFF_FFFF_Fp72)
                  ,("~0xFFFF_FFFF_FFFF_F400_01", ~0xFFFF_FFFF_FFFF_F400_01, ~0x0.FFFF_FFFF_FFFF_F8p72)
                  ,("2^100", IntInf.<< (1, 0w100), 0x1p100)
                  ,("maxFinite", maxFiniteAsLargeInt, Real.maxFinite)
                  ,("maxFinite+1", maxFiniteAsLargeInt+1, Real.maxFinite)
                  ,("maxFinite-1", maxFiniteAsLargeInt-1, Real.maxFinite)
                  ];
