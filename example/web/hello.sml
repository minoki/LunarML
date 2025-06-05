val console = JavaScript.global "console";
JavaScript.method (console, "log") #[JavaScript.fromString16 "Hello from console!"];
val document = JavaScript.global "document";
fun onLoad () = 
    let val root = JavaScript.method (document, "getElementById") #[JavaScript.fromString16 "root"]
    in JavaScript.setField (root, "innerText", JavaScript.fromString16 "Hello with innerText!")
    end;
JavaScript.method (document, "addEventListener") #[JavaScript.fromString16 "DOMContentLoaded", JavaScript.function (fn _ => (onLoad (); JavaScript.undefined))];
