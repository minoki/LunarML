fun op @ ([], ys) = ys
  | (x :: xs) @ ys = x :: (xs @ ys);
val rec rev = fn nil => nil
  | x :: xs => rev xs @ [x];
val 'a id = fn (x : 'a) => x;
val 'a list_toString = fn (showElem : 'a -> string) => let val rec go = fn [] => ""
					                     | x :: xs => "," ^ showElem x ^ go xs
				                       in fn [] => "[]"
				                        | (x :: xs) => "[" ^ showElem x ^ go xs ^ "]"
				                       end;
val strlist_toString = list_toString id;
print (strlist_toString [] ^ "\n");
print (strlist_toString ["foo"] ^ "\n");
print (strlist_toString ["foo","bar"] ^ "\n");
print (strlist_toString ["foo","bar","baz"] ^ "\n");
val intlist_toString = list_toString Int.toString;
print (intlist_toString [] ^ "\n");
print (intlist_toString [0] ^ "\n");
print (intlist_toString [1,2] ^ "\n");
print (intlist_toString ([1,2] @ [3,4,5]) ^ "\n");
print (intlist_toString (rev [2,3,5,7,11,13,17]) ^ "\n");
