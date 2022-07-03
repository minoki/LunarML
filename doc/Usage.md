
# Usage

*How to user the `lunarml` command.*

<br>

## Syntax

```shell
./lunarml [options] input.(sml|mlb)
```

<br>
<br>

## Arguments

### Help

Pass `--help` for more information.

<br>

### Lua

Pass `-mexe` to produce a Lua program.

*This is the default setting.*

<br>

### Lua Module

Pass `-mlib` to compile a Lua module.

<br>

### JavaScript

Pass `--js` to produce a JavaScript program.

<br>

### JavaScript CPS

Pass `--js-cps` to generate a JavaScript in **CPS** mode.

*Supports delimited continuations.*

<br>

### Nested Handles

Pass `--lua-stackless-mode` to allow deeply nested `handle`.

<br>
