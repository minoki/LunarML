structure Driver = struct
fun print_error (s,p1,p2) = print s
fun parse str = #2 (Fixity.doDecs(InitialEnv.initialFixity, #1 (DamepoMLParser.parse(0, DamepoMLParser.makeLexer(DamepoMLLex.makeInputFromString str), print_error, ()))))

fun compile source =
    let val ast1 = parse source
        val ctx = Typing.newContext()
        val (_, ast2) = ToTypedSyntax.toUDecs(ctx, Syntax.TyVarMap.empty, InitialEnv.initialEnv_ToTypedSyntax, PostParsing.scopeTyVarsInDecs(Syntax.TyVarSet.empty, ast1))
        val (env, tvc, (topdecs, decs)) = Typing.typeCheckProgram(ctx, InitialEnv.initialEnv, ast2)
        val decs' = Typing.applyDefaultTypes(tvc, decs)
        val fctx = { nextVId = #nextVId ctx }
        val fdecs = ToFSyntax.toFDecs(fctx, ToFSyntax.emptyEnv, decs')
        val fdecs' = List.map (#doDec (FTransform.desugarPatternMatches fctx) FTransform.emptyEnv) fdecs
    in (topdecs, ast1, ast2, decs', fdecs, fdecs')
           (* TODO:
            * - Desugar equality
            * - Desugar pattern matches
            * - Some normal form
            *)
    end
end (* structure Driver *)
