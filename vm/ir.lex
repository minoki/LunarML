(* -*- mode: sml-lex -*- *)

type pos = int
type svalue = Tokens.svalue
type ('a,'b) token = ('a,'b) Tokens.token
type lexresult = (svalue,pos) Tokens.token

fun error x = print x
fun eof () = Tokens.EOF (0, 0)
fun stringValue s = let fun go (t, acc)
                            = case Substring.getc t of
                                  NONE => String.implode (List.rev acc)
                                | SOME (#"\\", t) => (case Substring.getc t of
                                                          NONE => raise Fail "invalid string"
                                                        | SOME (#"a", t) => go (t, #"\a" :: acc)
                                                        | SOME (#"b", t) => go (t, #"\b" :: acc)
                                                        | SOME (#"t", t) => go (t, #"\t" :: acc)
                                                        | SOME (#"n", t) => go (t, #"\n" :: acc)
                                                        | SOME (#"r", t) => go (t, #"\r" :: acc)
                                                        | SOME (#"\"", t) => go (t, #"\"" :: acc)
                                                        | SOME (#"|", t) => go (t, #"|" :: acc)
                                                        | SOME (c, t) => go (t, c :: acc)
                                                     )
                                | SOME (c, t) => go (t, c :: acc)
                    in go (Substring.substring (s, 1, String.size s - 2), [])
                    end

fun hexDigitToInt x = if #"0" <= x andalso x <= #"9" then
                          ord x - ord #"0"
                      else if #"a" <= x andalso x <= #"f" then
                          ord x - ord #"a" + 10
                      else
                          ord x - ord #"A" + 10
%%
%header (functor IRLexFun (structure Tokens: IR_TOKENS));

digit = [0-9];
hexDigit = [0-9a-fA-F];
ws = [\ \t\n];
identifierPrefixChar = [a-zA-Z\!\$\%\&\*\+\-\/\:\<\=\>\?\@\^\_\~];
identifierChar = [a-zA-Z0-9\!\$\%\&\*\+\-\.\/\:\<\=\>\?\@\^\_\~];
stringChar = [^\"\\]|\\[abtnr\"\\\|];

%%

{ws}+ => (lex ());
";"[^\n]* => (lex ());
"(" => (Tokens.LPAREN (0, 0));
")" => (Tokens.RPAREN (0, 0));
"_" => (Tokens.UNDERSCORE (0, 0));
"abort" => (Tokens.ABORT (0, 0));
"construct-data-with-payload" => (Tokens.CONSTRUCT_DATA_WITH_PAYLOAD (0, 0));
"construct-data-without-payload" => (Tokens.CONSTRUCT_DATA_WITHOUT_PAYLOAD (0, 0));
"false" => (Tokens.FALSE (0, 0));
"get-data-payload" => (Tokens.GET_DATA_PAYLOAD (0, 0));
"get-data-tag" => (Tokens.GET_DATA_TAG (0, 0));
"handle" => (Tokens.HANDLE (0, 0));
"if" => (Tokens.IF (0, 0));
"lambda" => (Tokens.LAMBDA (0, 0));
"let" => (Tokens.LET (0, 0));
"nil" => (Tokens.NIL (0, 0));
"prim0" => (Tokens.PRIM0 (0, 0));
"prim1" => (Tokens.PRIM1 (0, 0));
"prim2" => (Tokens.PRIM2 (0, 0));
"prim3" => (Tokens.PRIM3 (0, 0));
"push-prompt" => (Tokens.PUSH_PROMPT (0, 0));
"push-subcont" => (Tokens.PUSH_SUBCONT (0, 0));
"raise" => (Tokens.RAISE (0, 0));
"rec" => (Tokens.REC (0, 0));
"true" => (Tokens.TRUE (0, 0));
"tuple" => (Tokens.TUPLE (0, 0));
"unit" => (Tokens.UNIT (0, 0));
"with-subcont" => (Tokens.WITH_SUBCONT (0, 0));
"\""{stringChar}*"\"" => (Tokens.STRING (stringValue yytext, 0, 0));
"%"{identifierPrefixChar}{identifierChar}* => (Tokens.VID (String.extract (yytext, 1, NONE), 0, 0));
"0w"{digit}+ => (Tokens.WORD (CharVector.foldl (fn (a,r) => Word64.fromInt (ord a - ord #"0") + 0w10 * r) 0w0 (String.extract (yytext, 2, NONE)), 0, 0));
"0wx"{hexDigit}+ => (Tokens.WORD (CharVector.foldl (fn (a,r) => Word64.fromInt (hexDigitToInt a) + 0w16 * r) 0w0 (String.extract (yytext, 3, NONE)), 0, 0));
{digit}+ => (Tokens.INT (CharVector.foldl (fn (a,r) => Int64.fromInt (ord a - ord #"0") + 10 * r) 0 yytext, 0, 0));
"~"{digit}+ => (Tokens.INT (CharVector.foldl (fn (a,r) => Int64.fromInt (ord #"0" - ord a) + 10 * r) 0 yytext, 0, 0));
{identifierPrefixChar}{identifierChar}* => (Tokens.ID (yytext, 0, 0));
. => (error ("toylang: ignoring bad character " ^ yytext); lex ());
