structure Void :>
sig
  type void
  val absurd: void -> 'a
end =
struct
  datatype void = VOID of void
  fun absurd (VOID v) = absurd v
end;
