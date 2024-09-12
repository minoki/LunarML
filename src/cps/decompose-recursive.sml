(*
 * Copyright (c) 2024 ARATA Mizuki
 * This file is part of LunarML.
 *)
structure CpsDecomposeRecursive:
sig
  val goCExp: CpsSimplify.Context * CSyntax.CExp -> CSyntax.CExp
end =
struct
  local structure C = CSyntax
  in
    fun goDec ctx (dec, acc) =
      case dec of
        C.ValDec
          {exp = C.Abs {contParam, params, body, attr}, results as [SOME _]} =>
          C.ValDec
            { exp = C.Abs
                { contParam = contParam
                , params = params
                , body = goCExp (ctx, body)
                , attr = attr
                }
            , results = results
            } :: acc
      | C.ValDec {exp = _, results = _} => dec :: acc
      | C.RecDec defs =>
          let
            val defs =
              List.map
                (fn {name, contParam, params, body, attr} =>
                   { name = name
                   , contParam = contParam
                   , params = params
                   , body = goCExp (ctx, body)
                   , attr = attr
                   }) defs
            val defined =
              List.foldl
                (fn ({name, ...}, set) => TypedSyntax.VIdSet.add (set, name))
                TypedSyntax.VIdSet.empty defs
            val map =
              List.foldl
                (fn (def as {name, body, ...}, map) =>
                   TypedSyntax.VIdMap.insert
                     ( map
                     , name
                     , { def = def
                       , dests = TypedSyntax.VIdSet.intersection
                           ( C.freeVarsInExp
                               ( TypedSyntax.VIdSet.empty
                               , body
                               , TypedSyntax.VIdSet.empty
                               )
                           , defined
                           )
                       }
                     )) TypedSyntax.VIdMap.empty defs
            val sccs = TypedSyntax.VIdSCC.components (#dests, map)
            val () =
              case sccs of
                [_] => ()
              | _ => #simplificationOccurred ctx := true
          in
            List.foldl
              (fn (scc, decs) =>
                 let
                   val dec =
                     case TypedSyntax.VIdSet.listItems scc of
                       [vid] =>
                         let
                           val
                             { def as {name, contParam, params, body, attr}
                             , dests
                             } = TypedSyntax.VIdMap.lookup (map, vid)
                         in
                           if TypedSyntax.VIdSet.member (dests, vid) then
                             C.RecDec [def]
                           else
                             ( #simplificationOccurred ctx := true
                             ; C.ValDec
                                 { exp = C.Abs
                                     { contParam = contParam
                                     , params = params
                                     , body = body
                                     , attr = attr
                                     }
                                 , results = [SOME name]
                                 }
                             )
                         end
                     | scc =>
                         C.RecDec
                           (List.map
                              (fn vid =>
                                 #def (TypedSyntax.VIdMap.lookup (map, vid)))
                              scc)
                 in
                   dec :: decs
                 end) acc sccs
          end
      | C.ContDec {name, params, body, attr} =>
          C.ContDec
            { name = name
            , params = params
            , body = goCExp (ctx, body)
            , attr = attr
            } :: acc
      | C.RecContDec defs =>
          let
            val defs =
              List.map
                (fn (name, params, body) => (name, params, goCExp (ctx, body)))
                defs
          in
            C.RecContDec defs :: acc
          end
      | C.ESImportDec _ => dec :: acc
    and goCExp (ctx: CpsSimplify.Context, exp) =
      case exp of
        C.Let {decs, cont} =>
          C.Let
            { decs = List.rev (List.foldl (goDec ctx) [] decs)
            , cont = goCExp (ctx, cont)
            }
      | C.App _ => exp
      | C.AppCont _ => exp
      | C.If {cond, thenCont, elseCont} =>
          C.If
            { cond = cond
            , thenCont = goCExp (ctx, thenCont)
            , elseCont = goCExp (ctx, elseCont)
            }
      | C.Handle {body, handler = (e, h), successfulExitIn, successfulExitOut} =>
          C.Handle
            { body = goCExp (ctx, body)
            , handler = (e, goCExp (ctx, h))
            , successfulExitIn = successfulExitIn
            , successfulExitOut = successfulExitOut
            }
      | C.Unreachable => exp
  end (* local *)
end; (* structure CpsDecomposeRecursive *)
