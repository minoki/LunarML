type t = { a : int
         , b : string
         , c : char
         , d : word
         , e : int * int
         , f : int
         , g : string
         }
fun update_a ({ a, ... = rest } : t, f) = { a = f a, ... = rest }
fun update_b ({ b, ... = rest } : t, f) = { b = f b, ... = rest }
fun update_c ({ c, ... = rest } : t, f) = { c = f c, ... = rest }
fun update_d ({ d, ... = rest } : t, f) = { d = f d, ... = rest }
fun update_e ({ e, ... = rest } : t, f) = { e = f e, ... = rest }
fun update_f ({ f, ... = rest } : t, f') = { f = f' f, ... = rest }
fun update_g ({ g, ... = rest } : t, f) = { g = f g, ... = rest }
val x = { a = 0, b = "b", c = #"c", d = 0wxcafe, e = (4, 2), f = 37, g = "Hello world" }
val x = update_a (x, fn a => a + 1)
val x = update_b (x, fn b => b ^ b)
val x = update_c (x, Char.toUpper)
val x = update_d (x, fn d => d * 0w2)
val x = update_e (x, fn (s, t) => (t, s))
val x = update_f (x, fn f => f * f);
val x = update_g (x, fn g => g ^ "!");
print (Int.toString (#a x) ^ "\n");
print (#b x ^ "\n");
print (String.str (#c x) ^ "\n");
print (Word.toString (#d x) ^ "\n");
print ("(" ^ Int.toString (#1 (#e x)) ^ "," ^ Int.toString (#2 (#e x)) ^ ")\n");
print (Int.toString (#f x) ^ "\n");
print (#g x ^ "\n");
