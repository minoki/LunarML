type t = { a : int, b : string, c : char, d : word, e : int * int, f : int, g : string }
fun update_a (x : t, f) = { x where a = f (#a x) }
fun update_b (x : t, f) = { x where b = f (#b x) }
fun update_c (x : t, f) = { x where c = f (#c x) }
fun update_d (x : t, f) = { x where d = f (#d x) }
fun update_e (x : t, f) = { x where e = f (#e x) }
fun update_f (x : t, f) = { x where f = f (#f x) }
fun update_g (x : t, f) = { x where g = f (#g x) }
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
