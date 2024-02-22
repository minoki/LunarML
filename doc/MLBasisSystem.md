# ML Basis system

LunarML supports ML Basis system compatible with MLton and ML Kit.

* [MLBasis (MLton)](http://mlton.org/MLBasis)
* [ML Basis Files (MLKit)](https://elsman.com/mlkit/mlbasisfiles.html)

## Syntax

Comments are ML-like: between `(*` .. `*)` and can be nested.

```
<basexp> ::= 'bas' <basdec> 'end'
           | 'let' <basdec> 'in' <basexp> 'end'
           | <identifier>
<basdec> ::=   (* empty *)
           | <basdec> ';'? <basdec>
           | 'basis' <identifier> '=' <basexp> ('and' <identifier> '=' <basexp>)*
           | 'open' <identifier>+
           | 'local' <basdec> 'in' <basdec> 'end'
           | 'structure' <identifier> ('=' <identifier>)? ('and' <identifier> ('=' <identifier>)?)*
           | 'signature' <identifier> ('=' <identifier>)? ('and' <identifier> ('=' <identifier>)?)*
           | 'functor' <identifier> ('=' <identifier>)? ('and' <identifier> ('=' <identifier>)?)*
           | 'ann' <string>+ 'in' <basdec> 'end'
           | <path.sml> | <path.sig> | <path.fun>
           | <path.mlb>
```

An MLB file consists of `<basdec>`.

## Available annotations

* `nonexhaustiveBind {warn|error|ignore}`
* `nonexhaustiveMatch {warn|error|ignore}`
* `nonexhaustiveRaise {ignore|warn|error}`
* `redundantBind {warn|error|ignore}`
* `redundantMatch {warn|error|ignore}`
* `redundantRaise {warn|error|ignore}`
* `sequenceNonUnit {ignore|warn|error}`
* `valDescInComments {ignore|warn|error}`
* `allowDoDecls {true|false}`
* `allowExtendedConsts {true|false}`
    * `allowExtendedNumConsts {true|false}`
    * `allowExtendedTextConsts {true|false}`
* `allowLineComments {true|false}`
* `allowOptBar {true|false}`
* `allowOptSemicolon {true|false}`
* `allowRecordPunExps {true|false}`
* `allowSigWithtype {true|false}`
* `allowVectorExpsAndPats {true|false}`
    * `allowVectorExps {true|false}`
    * `allowVectorPats {true|false}`
* `allowRecordExtension {true|false}`
* `allowRecordUpdate {true|false}`
* `allowUtfEscapeSequences {true|false}`
* `allowHexFloatConsts {true|false}`
* `allowValRecTyVars {true|false}`
* `allowValTyVarsRec {true|false}`
* `allowFreeTyVarsInTypeDec {true|false}`
* `allowWhereAndType {true|false}`
* `allowInfixingDot {false|true}`
* `allowSuccessorML {true|false}`
    * This is a combination of the following annotations:
        * `allowDoDecls <value>`
        * `allowExtendedNumConsts <value>`
        * `allowExtendedTextConsts <value>`
        * `allowLineComments <value>`
        * `allowOptBar <value>`
        * `allowOptSemicolon <value>`
        * `allowRecordPunExps <value>`
        * `allowSigWithtype <value>`
        * `allowRecordExtension <value>`
        * `allowRecordUpdate <value>`
        * `allowValRecTyVars <value>`
        * `allowValTyVarsRec <not value>`
        * `allowFreeTyVarsInTypeDec <not value>`
        * `allowWhereAndType <not value>`

The default value for an annotation could be specified by `--default-ann` option.

## Avaliable libraries

* `$(SML_LIB)/basis/basis.mlb`: The [Basis Library](BasisLibrary.md)
* `$(SML_LIB)/basis/unsafe.mlb`: The [Unsafe](UnsafeStructure.md) structure
* `$(SML_LIB)/basis/lunarml.mlb`: The [LunarML](LunarMLStructure.md) structure
* `$(SML_LIB)/basis/lua.mlb`: The [Lua](LuaInterface.md) structure
* `$(SML_LIB)/basis/javascript.mlb`: The [JavaScript](JavaScriptInterface.md) structure
* `$(SML_LIB)/basis/pipeline.mlb`: The pipeline operator `|>`. See [OtherLibraries](OtherLibraries.md).

The following libraries are available if `make -C thirdparty install` is run:

* `$(SML_LIB)/mlyacc-lib/mlyacc-lib.mlb`
* `$(SML_LIB)/smlnj-lib/Util/smlnj-lib.mlb`

## Path variables

You can use `--mlb-path-map=<file>` option and `--mlb-path-var=<var>=<path>` option to configure MLB path variables.

Each line of path map file consists of two space-separated tokens: variable name and path.
