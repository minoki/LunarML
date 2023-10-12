# Successor ML

LunarML implements some of [Successor ML](https://github.com/SMLFamily/Successor-ML) features:

* [x] Monomorphic non-exhaustive bindings
* [x] Simplified recursive value bindings
    * SML '97-compatible ordering for type variables is also supported: `val <tyvarseq> rec <valbind>`
* [x] Abstype as derived form
* [x] Fixed manifest type specifications
* [x] Abolish sequenced type realizations
    * `and type` is allowed by default; You can use `"allowWhereAndType false"` annotation to disable it.
* [x] Line comments
* [x] Extended literal syntax
    * [x] Underscores (e.g. `3.1415_9265`, `0xffff_ffff`)
    * [x] Binary notation (`0b`, `0wb`)
    * [x] Eight hex digits in text (`\Uxxxxxxxx`)
* [x] Record punning
* [x] Record extension
* [x] Record update
* [ ] Conjunctive patterns
* [ ] Disjunctive patterns
* [ ] Nested matches
* [ ] Pattern guards
* [x] Optional bars and semicolons
* [ ] Optional else branch
* [x] Do declarations
* [x] Withtype in signatures
