let
    fun A B C = B ^ C;
in
    print (A "Hello " "world!\n")
end;
let
    infix B
    fun A B C = A ^ C;
in
    print (op B ("Hello ", "world!\n"))
end;
let
    infix B
    fun (A B C) = A ^ C;
in
    print (op B ("Hello ", "world!\n"))
end;
let
    infix B
    fun (A B C) D E = A ^ C ^ D ^ E;
in
    print (op B ("Hello", " ") "world!" "\n")
end;
let
    infix D
    fun (A :: C) D E = A ^ E
      | nil D E = E;
in
    print (op D (["Hello "], "world!\n"))
end;
