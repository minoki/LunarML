(*
 * Copyright (c) 2024 ARATA Mizuki
 * This file is part of LunarML.
 *)
structure StringMap = RedBlackMapFn (type ord_key = string; val compare = String.compare);
