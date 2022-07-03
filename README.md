
# LunarML   [![Badge License]][License]

*A standard ML compiler that produces **Lua** / **JavaScript**.*

<br>

<div align = center>

---

[![Button Usage]][Usage]   
[![Button Building]][Building] 

---

</div>

<br>
<br>

## Features

-   Most of SML '97 language + Signatures + Functors
-   A subset of the **[SML Basis Library]**
-   An **[Interface to Lua]**
-   ML Basis system like **[MLton]**

<br>

### Successor ML Features

-   [x] Monomorphic non-exhaustive bindings
-   [x] <details><summary>Simplified recursive value bindings</summary>

    SML '97-compatible ordering for <br>
    type variables is also supported: 
    
    ```sml
    val <tyvarseq> rec <valbind>
    ```
    
    </details>


-   [x] Abstype as derived form
-   [x] Fixed manifest type specifications
-   [ ] Abolish sequenced type realizations
-   [ ] Line comments
-   [x] <details><summary>Extended literal syntax</summary>
    -   [x] Underscores 
        - `3.1415_9265`
        - `0xffff_ffff`
    -   [x] Binary notation
        - `0wb`
        - `0b`
    -   [x] Eight hex digits in text
        - `\Uxxxxxxxx`

    </details>
    
        
-   [ ] Record punning
-   [x] Record extension
-   [x] Record update
-   [ ] Conjunctive patterns
-   [ ] Disjunctive patterns
-   [ ] Nested matches
-   [ ] Pattern guards
-   [ ] Optional bars and semicolons
-   [ ] Optional else branch
-   [ ] Do declarations
-   [x] With type in signatures

### Planned Extensions

-   [x] Vector expressions and patterns
-   [ ] <details><summary>Packaged modules</summary>
    - `Alice ML`
    - `HaMLet S`
    
    </details>
    
    
-   [x] <details><summary>Hexadecimal floating-point constants</summary>
    - `0x1.ffff_ffff_ffff_f`
    - `0x1p1024`
    
    ### Syntax

    ```sml
    <hexadecimal-integer-constant> ::= '~'? '0' 'w'? 'x' <hexadecimal-digit-sequence>
    ```

    ```sml
    <hexadecimal-floating-point-constant> ::= '~'? '0x' <hexadecimal-digit-sequence> (<binary-exponent-part> | 
    ```

    ```sml    
    '.' <hexadecimal-digit-sequence> <binary-exponent-part>?)
    ```
    
    ```sml
    <hexadecimal-digit-sequence> ::= <hexadecimal-digit> ('_'* <hexadecimal-digit>)*
    ```
    
    ```sml
    <binary-exponent-part> ::= [pP] '~'? <digit> ('_'* <digit>)?
    ```
    
    ### In Short
    
    The ( binary ) exponent part is optional and <br>
    tilde ( `~` ) is used for the negation symbol.
    
    </details>
    
    
-   [x] <details><summary>Variably-encoded Unicode escape sequence in string literals</summary>
    - `\u{3042}`
    
    ### Details
    
    The `\u{}` escape sequence makes it possible to <br>
    embed unicode scalar values in a string literals.
    
    The compiler encodes the character in <br>
    UTF-8,16,32 depending on the string type.
    
    </details>

<br>


<!----------------------------------------------------------------------------->

[MLton]: http://mlton.org/MLBasis

[SML Basis Library]: doc/BasisLibrary.md
[Interface to Lua]: doc/LuaInterface.md
[Building]: doc/Building.md 'How to compile this project from source.'
[License]: LICENSE
[Usage]: doc/Usage.md 'What options you have available to interact with.'


<!---------------------------------[ Badges ]---------------------------------->

[Badge License]: https://img.shields.io/badge/License-MIT-yellow.svg?style=for-the-badge


<!--------------------------------[ Buttons ]---------------------------------->

[Button Building]: https://img.shields.io/badge/Building-C9284D?style=for-the-badge&logoColor=white&logo=Square
[Button Usage]: https://img.shields.io/badge/Usage-31A8FF?style=for-the-badge&logoColor=white&logo=ReadTheDocs


