# Delimited Continuations

Availability: JS-CPS backend.

Status: Experimental. Does not work well with exceptions.

```sml
structure LunarML : sig
  ...
  structure DelimCont : sig
    type 'a prompt_tag
    type ('a,'b) subcont
    val newPromptTag : unit -> 'a prompt_tag
    val pushPrompt : 'a prompt_tag * (unit -> 'a) -> 'a
    val withSubCont : 'b prompt_tag * (('a,'b) subcont -> 'b) -> 'a
    val pushSubCont : ('a,'b) subcont * (unit -> 'a) -> 'b
    val shift : 'a prompt_tag * (('b -> 'a) -> 'a) -> 'b
    val control : 'a prompt_tag * (('b -> 'a) -> 'a) -> 'b
    val abort : 'a prompt_tag * 'a -> 'b
    val topLevel : unit prompt_tag
  end
  ...
end
```

The interface is based on the following paper:

* Kent Dybvig, Simon Peyton Jones, Amr Sabry. 2005. A Monadic Framework for Delimited Continuations. *Journal of Functional Programming*. doi:10.1017/S0956796807006259 <https://www.microsoft.com/en-us/research/publication/a-monadic-framework-for-delimited-continuations/>

`pushPrompt` is equivalent to `reset`/`prompt` in other formulations.

`topLevel` is a prompt that is implicitly pushed by the runtime. Some runtime functions, including `TextIO.print`, need it to work.

`JavaScript.callback` implicitly pushes `topLevel`, but `JavaScript.function` does not.
