(* source: Andrzej Filinski. 1994. Representing monads. In Proceedings of the 21st ACM SIGPLAN-SIGACT symposium on Principles of programming languages (POPL '94). Association for Computing Machinery, New York, NY, USA, 446–457. https://doi.org/10.1145/174675.178047 *)

signature MONAD = sig
    type 'a t
    val unit : 'a -> 'a t
    val ext : ('a -> 'b t) -> 'a t -> 'b t
end

signature RMONAD = sig
    structure M : MONAD
    val reflect : 'a M.t -> 'a
    val reify : (unit -> 'a) -> 'a M.t
end

signature UNIVERSAL = sig
    type u
    val to_u : 'a -> u
    val from_u : u -> 'a
end

structure Universal :> UNIVERSAL = struct
type u = JavaScript.value
val to_u = Unsafe.cast
val from_u = Unsafe.cast
end

functor Represent (M : MONAD) : RMONAD = struct
structure M = M
val p : Universal.u M.t LunarML.DelimCont.prompt = LunarML.DelimCont.newPrompt ()
fun reflect m = LunarML.DelimCont.shift (p, fn k => M.ext k m)
fun reify t = M.ext (M.unit o Universal.from_u)
                    (LunarML.DelimCont.pushPrompt (p, fn () => M.unit (Universal.to_u (t ()))))
end;

(*
 * Example: exceptions
 *)

structure ErrorMonad = struct
datatype 'a t = SUC of 'a | ERR of string
val unit = SUC
fun ext f (SUC a) = f a
  | ext f (ERR s) = ERR s
end

structure ErrorRep = Represent (ErrorMonad);

local open ErrorMonad ErrorRep in
fun myraise e = reflect (ERR e)
fun myhandle t h = case reify t of
                       SUC a => a
                     | ERR s => h s
end
fun show t = myhandle (fn () => print ("OK: " ^ Int.toString (t ()) ^ "\n"))
                      (fn s => print ("Error: " ^ s ^ "\n"));
show (fn () => 1 + 2);
show (fn () => 1 + myraise "oops");

(*
 * Example: state
 *)

functor StateMonad (type state) : MONAD = struct
type 'a t = state -> 'a * state
fun unit a = fn s0 => (a, s0)
fun ext f m = fn s0 => let val (a, s1) = m s0
                       in f a s1
                       end
end

structure IntStateRep = Represent (StateMonad (type state = int));

fun tick () = IntStateRep.reflect (fn s => ((), s + 1))
fun fetch () = IntStateRep.reflect (fn s => (s, s))
fun store n = IntStateRep.reflect (fn s => ((), n));
val result = #1 (IntStateRep.reify (fn () => ( store 5
                                             ; tick ()
                                             ; 2 * fetch ()
                                             )
                                   ) 0);
print (Int.toString result ^ "\n");

(*
 * Example: nondeterminism
 *)

structure ListMonad : MONAD = struct
type 'a t = 'a list
fun unit x = [x]
fun ext f [] = []
  | ext f (x :: xs) = f x @ ext f xs
end

structure ListRep = Represent (ListMonad);

local open ListRep in
fun amb (x, y) = reflect (reify (fn () => x) @ reify (fn () => y))
fun fail () = reflect []
end
val result = ListRep.reify (fn () => let val x = amb (3,4) * amb (5,7)
                                     in if x >= 20 then
                                            x
                                        else
                                            fail()
                                     end
                           );
print ("[" ^ String.concatWith "," (List.map Int.toString result) ^ "]\n");
