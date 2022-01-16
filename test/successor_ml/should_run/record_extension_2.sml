type t = { a : int
         , b : string
         , c : char
         , d : word
         , e : int * int
         , f : int
         , g : string
         }
fun update_a ({ ... = rest, a } : t, f) = { ... = rest, a = f a }
fun update_b ({ ... = rest, b } : t, f) = { ... = rest, b = f b }
fun update_c ({ ... = rest, c } : t, f) = { ... = rest, c = f c }
fun update_d ({ ... = rest, d } : t, f) = { ... = rest, d = f d }
fun update_e ({ ... = rest, e } : t, f) = { ... = rest, e = f e }
fun update_f ({ ... = rest, f } : t, f') = { ... = rest, f = f' f }
fun update_g ({ ... = rest, g } : t, f) = { ... = rest, g = f g }
val x = { a = 0, b = "b", c = #"c", d = 0wxcafe, e = (4, 2), f = 37, g = "Hello world" }
val x = update_a (x, fn a => a + 1)
val x = update_b (x, fn b => b ^ b)
val x = update_c (x, Char.toUpper)
val x = update_d (x, fn d => d * 0w2)
val x = update_e (x, fn (s, t) => (t, s))
val x = update_f (x, fn f => f * f);
val x = update_g (x, fn g => g ^ "!");
val z = { 1 = print (Int.toString (#a x) ^ "\n")
        , 2 = print (#b x ^ "\n")
        , 3 = print (String.str (#c x) ^ "\n")
        , ... = (print (Word.toString (#d x) ^ "\n"); { a = "a", b = "b" })
        , 4 = print ("(" ^ Int.toString (#1 (#e x)) ^ "," ^ Int.toString (#2 (#e x)) ^ ")\n")
        , 5 = print (Int.toString (#f x) ^ "\n")
        , 6 = print (#g x ^ "\n")
        };
print (Bool.toString (z = { 1 = (), 2 = (), 3 = (), 4 = (), 5 = (), 6 = (), a = "a", b = "b" }) ^ "\n");
