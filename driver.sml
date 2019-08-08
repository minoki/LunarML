structure Driver = struct
fun print_error (s,p1,p2) = print s
fun parse str = #2 (Fixity.doDecs(InitialEnv.initialFixity, #1 (DamepoMLParser.parse(0, DamepoMLParser.makeLexer(DamepoMLLex.makeInputFromString str), print_error, ()))))

fun compile source =
    let val ast1 = parse source
        val ctx = Typing.newContext()
        val (_, ast2) = ToTypedSyntax.toUDecs(ctx, ToTypedSyntax.emptyEnv, PostParsing.scopeTyVarsInDecs(Syntax.TyVarSet.empty, ast1))
        val (env, tvc, decs) = Typing.typeCheckProgram(ctx, InitialEnv.initialEnv, ast2)
    in Typing.applyDefaultTypes(tvc, decs)
    end
end (* structure Driver *)
