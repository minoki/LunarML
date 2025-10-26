type pos = int
type svalue = Tokens.svalue
type ('a,'b) token = ('a,'b) Tokens.token
type lexresult = (svalue,pos) Tokens.token
type arg = string
fun error x = raise Fail x
fun eof s = Tokens.EOF (0,0)

%%
%header (functor AlbireoLexFun (structure Tokens: Albireo_TOKENS));
%arg (filename);

digit = [0-9];
ws = [\ \t\n];
id_start = [a-zA-Z_];
id_continue = [a-zA-Z0-9_];

%%

{ws}+ => (continue ());
"+" => (Tokens.PLUS (0,0));
"-" => (Tokens.MINUS (0,0));
"*" => (Tokens.TIMES (0,0));
"/" => (Tokens.DIV (0,0));
"(" => (Tokens.LPAREN (0,0));
")" => (Tokens.RPAREN (0,0));
"{" => (Tokens.LBRACE (0,0));
"}" => (Tokens.RBRACE (0,0));
"=" => (Tokens.EQ (0,0));
";" => (Tokens.SEMICOLON (0,0));
":" => (Tokens.COLON (0,0));
"->" => (Tokens.ARROW (0,0));
"," => (Tokens.COMMA (0,0));
{digit}+ => (Tokens.INT ((CharVector.foldl (fn (d, acc) => ord d - ord #"0" + 10 * acc) 0 yytext, NONE),0,0));
{digit}+"i32" => (Tokens.INT ((CharVector.foldl (fn (d, acc) => ord d - ord #"0" + 10 * acc) 0 yytext, SOME AlbireoSyntax.IT_32),0,0));
return => (Tokens.RETURN (0,0));
func => (Tokens.FUNC (0,0));
i32 => (Tokens.I32 (0,0));
{id_start}{id_continue}* => (Tokens.IDENT (yytext,0,0));

. => (error ("bad character: " ^ yytext));
