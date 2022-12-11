structure TargetInfo = struct
datatype datatype_tag_type = STRING8 | STRING16
type target_info = { datatypeTag : datatype_tag_type (* for datatype tag *)
                   , minInt : IntInf.int option
                   , maxInt : IntInf.int option
                   , wordSize : int
                   }
end;
