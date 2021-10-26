signature CHAR_PARSER = sig
    eqtype string
    eqtype char
    type 'a parser
    val oneOf : char list -> char parser
    val noneOf : char list -> char parser
    val spaces : unit parser
    val space : char parser
    val newline : char parser
    val crlf : char parser
    val endOfLine : char parser
    val tab : char parser
    val upper : char parser
    val lower : char parser
    val alphaNum : char parser
    val letter : char parser
    val digit : char parser
    val hexDigit : char parser
    val octDigit : char parser
    val digitValue : int parser
    val hexDigitValue : int parser
    val octDigitValue : int parser
    val char : char -> char parser
    val anyChar : char parser
    val satisfy : (char -> bool) -> char parser
    val string : string -> string parser
end
