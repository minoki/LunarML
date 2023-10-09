(*
 * Copyright (c) 2023 ARATA Mizuki
 * This file is part of LunarML.
 *)
structure TargetInfo :> sig
              datatype datatype_tag_type = STRING8 | STRING16
              type target_info = { defaultInt : Primitives.int_width
                                 , defaultWord : Primitives.word_width
                                 , datatypeTag : datatype_tag_type (* for datatype tag *)
                                 , minInt : IntInf.int option
                                 , maxInt : IntInf.int option
                                 , wordSize : int
                                 }
              val minInt64 : IntInf.int
              val maxInt64 : IntInf.int
              val minInt54 : IntInf.int
              val maxInt54 : IntInf.int
              val minInt32 : IntInf.int
              val maxInt32 : IntInf.int
          end = struct
datatype datatype_tag_type = STRING8 | STRING16
type target_info = { defaultInt : Primitives.int_width
                   , defaultWord : Primitives.word_width
                   , datatypeTag : datatype_tag_type (* for datatype tag *)
                   , minInt : IntInf.int option
                   , maxInt : IntInf.int option
                   , wordSize : int
                   }
val minInt64 : IntInf.int = ~0x8000000000000000
val maxInt64 : IntInf.int = 0x7fffffffffffffff
val minInt54 : IntInf.int = ~0x20000000000000
val maxInt54 : IntInf.int = 0x1fffffffffffff
val minInt32 : IntInf.int = ~0x80000000
val maxInt32 : IntInf.int = 0x7fffffff
end;
