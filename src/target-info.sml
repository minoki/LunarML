structure TargetInfo = struct
datatype datatype_tag_type = STRING8 | STRING16
type target_info = { defaultInt : Primitives.int_width
                   , defaultWord : Primitives.word_width
                   , datatypeTag : datatype_tag_type (* for datatype tag *)
                   , minInt : IntInf.int option
                   , maxInt : IntInf.int option
                   , wordSize : int
                   }
end;
