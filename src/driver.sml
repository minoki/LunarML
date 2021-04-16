structure Driver = struct
fun parse(name, str) = let fun print_error (s,p1 as (l1,c1),p2 as (l2,c2)) = if p1 = p2 then
                                                                                 print (name ^ ":" ^ Int.toString l1 ^ ":" ^ Int.toString c1 ^ ": " ^ s ^ "\n")
                                                                             else
                                                                                 print (name ^ ":" ^ Int.toString l1 ^ ":" ^ Int.toString c1 ^ "-" ^ Int.toString l2 ^ ":" ^ Int.toString c2 ^ ": " ^ s ^ "\n")
                       in #2 (Fixity.doDecs(InitialEnv.initialFixity, #1 (DamepoMLParser.parse((* lookahead *) 0, DamepoMLParser.makeLexer(DamepoMLLex.makeInputFromString str), print_error, ()))))
                       end

fun compile(name, source) =
    let val ast1 = parse(name, source)
        val ctx = Typing.newContext()
        val (_, ast2) = ToTypedSyntax.toUDecs(ctx, Syntax.TyVarMap.empty, InitialEnv.initialEnv_ToTypedSyntax, PostParsing.scopeTyVarsInDecs(Syntax.TyVarSet.empty, ast1))
        val (env, tvc, (topdecs, decs)) = Typing.typeCheckProgram(ctx, InitialEnv.initialEnv, ast2)
        val decs' = Typing.applyDefaultTypes(tvc, decs)
        val fctx = { nextVId = #nextVId ctx }
        val fdecs = ToFSyntax.toFDecs(fctx, ToFSyntax.emptyEnv, decs')
        val fdecs' = List.map (#doDec (FTransform.desugarPatternMatches fctx) FTransform.initialEnv) fdecs
        val lua = CodeGenLua.doDecs { nextLuaId = ref 0 } CodeGenLua.MkEnv fdecs'
    in (topdecs, ast1, ast2, decs', fdecs, fdecs', lua)
    end
end (* structure Driver *)
