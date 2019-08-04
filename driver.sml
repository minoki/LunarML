structure Driver = struct
fun compile source =
    let val ast1 = SimpleParser.parse source
        val ctx = Typing.newContext()
        val (_, ast2) = ToTypedSyntax.toUDecs(ctx, ToTypedSyntax.emptyEnv, PostParsing.scopeTyVarsInDecs(Syntax.TyVarSet.empty, ast1))
        val (env, tvc, decs) = Typing.typeCheckProgram(ctx, Typing.initialEnv, ast2)
    in Typing.applyDefaultTypes(tvc, decs)
    end
end (* structure Driver *)
