structure TargetInfo = struct
datatype char_width = CHAR8 | CHAR16
datatype native_string_type = NARROW_STRING | WIDE_STRING
type target_info = { wideChar : char_width
                   , nativeString : native_string_type (* for datatype tag *)
                   (* maxInt, minInt, wordSize, realFormat, hasIntInf *)
                   }
end;
