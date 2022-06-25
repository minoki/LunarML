datatype 'a t = T of { value : 'a }
fun f (c : 'a -> order, t : 'a t) = case t of
                                        T { value } => (c value, t)
fun g k (k', _) = Int.compare (k', k)
datatype 'a u = U of { r : (int * 'a) t ref }
fun h (U { r }, k) = case f (g k, !r) of
                         (_, T { value as { 2 = x, ... } }) => #2 value;
