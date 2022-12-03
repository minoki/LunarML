structure TargetInfo = struct
datatype char_width = CHAR8 | CHAR16
datatype datatype_tag_type = STRING8 | STRING16
type target_info = { wideChar : char_width
                   , datatypeTag : datatype_tag_type (* for datatype tag *)
                   (* maxInt, minInt, wordSize, realFormat, hasIntInf *)
                   }
end;
