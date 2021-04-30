fun x <> y = not (x = y);

(* General *)
fun x before () = x;
fun ignore _ = ();
fun (f o g) x = f (g x);

(* List *)
fun [] @ ys = ys
  | (x :: xs) @ ys = x :: (xs @ ys);
fun app f [] = ()
  | app f (x :: xs) = (f x; app f xs);
fun foldl f init [] = init
  | foldl f init (x :: xs) = foldl f (f (x, init)) xs;
fun foldr f init [] = init
  | foldr f init (x :: xs) = f (x, foldr f init xs);
fun hd [] = raise Fail "List.Empty" (* TODO: exception declaration *)
  | hd (x :: _) = x;
local
    fun doLength (acc, []) = acc : int
      | doLength (acc, x :: xs) = doLength (acc + 1, xs)
in 
fun length xs = doLength (0, xs)
end;
fun map f [] = []
  | map f (x :: xs) = f x :: map f xs;
fun null [] = true
  | null _ = false;
fun rev [] = []
  | rev (x :: xs) = rev xs @ [x];
fun tl [] = raise Fail "List.Empty"
  | tl (_ :: xs) = xs;

(* Option *)
datatype 'a option = NONE | SOME of 'a;
fun getOpt (NONE, default) = default
  | getOpt (SOME x, _) = x;
fun isSome (SOME _) = true
  | isSome NONE = false;
fun valOf (SOME x) = x
  | valOf NONE = raise Fail "Option.Option"; (* TODO: exception declaration *)
