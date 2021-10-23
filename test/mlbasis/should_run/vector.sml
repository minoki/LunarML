val v = Vector.concat [Vector.fromList [1,2,3], vector [4,5,6], vector [], vector [7,8,9]];
Vector.app (fn x => print (Int.toString x ^ "\n")) v;
