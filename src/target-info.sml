structure TargetInfo = struct
datatype default_int = NATIVE_INT | INT32
datatype default_word = NATIVE_WORD | WORD32
datatype datatype_tag_type = STRING8 | STRING16
type target_info = { defaultInt : default_int
                   , defaultWord : default_word
                   , datatypeTag : datatype_tag_type (* for datatype tag *)
                   , minInt : IntInf.int option
                   , maxInt : IntInf.int option
                   , wordSize : int
                   }
end;
