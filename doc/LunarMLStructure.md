# LunarML structure

```sml
structure LunarML : sig
  val assumeDiscardable : ('a -> 'b) -> 'a -> 'b
  structure DelimCont
end
```

`assumeDiscardable f x` is equivalent to `f x`, except that the compiler ignores the possible side-effect of the application, and eliminates the call if the result is not used.

See [Delimited Continuations](DelimitedContinuations.md) for `LunarML.DelimCont`.
