functor TestWord (val name : string; structure W : WORD) = struct
fun printOptionWord NONE = print "NONE\n"
  | printOptionWord (SOME w) = print ("SOME 0w" ^ W.fmt StringCvt.DEC w ^ "\n")
val ss = ["", "0wx0", "0wx1", "0wX10", "99", "100", "0w", "0wx", "0xa", "0Xg", "0W3", "0", "10", "100", "1020", "1080", "0wx2", "8", "fF", " \t11"];
val () = List.app (fn s => (print (name ^ ".fromString \"" ^ String.toString s ^ "\" = "); printOptionWord (W.fromString s) handle Overflow => print "Overflow\n")) ss
val () = List.app (fn (radixs, radix) => List.app (fn s => (print (name ^ ".scan " ^ radixs ^ " \"" ^ String.toString s ^ "\" = "); printOptionWord (StringCvt.scanString (W.scan radix) s) handle Overflow => print "Overflow\n")) ss) [("BIN", StringCvt.BIN), ("OCT", StringCvt.OCT), ("DEC", StringCvt.DEC), ("HEX", StringCvt.HEX)]
end;
structure TestWord = TestWord (val name = "Word"; structure W = Word)
structure TestWord8 = TestWord (val name = "Word8"; structure W = Word8)
structure TestWord16 = TestWord (val name = "Word16"; structure W = Word16)
structure TestWord32 = TestWord (val name = "Word32"; structure W = Word32)
structure TestWord64 = TestWord (val name = "Word64"; structure W = Word64);
