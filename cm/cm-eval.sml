structure CMEval = struct
structure StringSet = CMParser.StringSet
structure StringMap = RedBlackMapFn (open String
                                     type ord_key = string
                                    )
val predefined = List.foldl StringMap.insert' StringMap.empty
                            [("NEW_CM", 1)
                            ,("SMLNJ_VERSION", 110)
                            ,("SMLNJ_MINOR_VERSION", 99)
                            ] (* OPSYS_{UNIX,WIN32,MACOS,OS2,BEOS}, ARCH_{SPARC,ALPHA,MIPS,X86,HPPA,RS6000,PPC}, {BIG,LITTLE}_ENDIAN, SIZE_{32,64} *)

fun readFile filename = let val ins = TextIO.openIn filename (* may raise Io *)
                            val content = TextIO.inputAll ins
                            val () = TextIO.closeIn ins
                        in content
                        end;

type state = { definedIn : string CMSyntax.MLSymbolMap.map (* key: mlsymbol, value: filename *)
             , uses : CMSyntax.MLSymbolSet.set StringMap.map (* key: filename, value: set of mlsymbols *)
             , allFilesRev : string list
             }
val initialState : state = { definedIn = CMSyntax.MLSymbolMap.empty, uses = StringMap.empty, allFilesRev = [] }

fun evalArith (state : state) (CMSyntax.ANumber x) = x
  | evalArith state (CMSyntax.ACMId x) = (case StringMap.find (predefined, x) of
                                              SOME v => v
                                            | NONE => (TextIO.output (TextIO.stdErr, "undefined identifier " ^ x ^ "\n"); 0)
                                         )
  | evalArith state (CMSyntax.ANegate x) = ~ (evalArith state x)
  | evalArith state (CMSyntax.ABinary (CMSyntax.MUL, a, b)) = evalArith state a * evalArith state b
  | evalArith state (CMSyntax.ABinary (CMSyntax.DIV, a, b)) = evalArith state a div evalArith state b
  | evalArith state (CMSyntax.ABinary (CMSyntax.MOD, a, b)) = evalArith state a mod evalArith state b
  | evalArith state (CMSyntax.ABinary (CMSyntax.PLUS, a, b)) = evalArith state a + evalArith state b
  | evalArith state (CMSyntax.ABinary (CMSyntax.MINUS, a, b)) = evalArith state a - evalArith state b
fun evalCondition (state : state) (CMSyntax.PPDefinedCM cmid)
    = if StringMap.inDomain (predefined, cmid) then
          (TextIO.output (TextIO.stdErr, "CM identifier " ^ cmid ^ ": defined\n"); true)
      else
          (TextIO.output (TextIO.stdErr, "CM identifier " ^ cmid ^ ": not defined\n"); false)
  | evalCondition state (CMSyntax.PPDefinedML mlsymbol)
    = if CMSyntax.MLSymbolMap.inDomain (#definedIn state, mlsymbol) then
          (TextIO.output (TextIO.stdErr, CMSyntax.MLSymbol.toString mlsymbol ^ ": defined\n"); true)
      else
          (TextIO.output (TextIO.stdErr, CMSyntax.MLSymbol.toString mlsymbol ^ ": not defined\n"); false)
  | evalCondition state (CMSyntax.PPCompare (CMSyntax.LT, a, b))
    = evalArith state a < evalArith state b
  | evalCondition state (CMSyntax.PPCompare (CMSyntax.LE, a, b))
    = evalArith state a <= evalArith state b
  | evalCondition state (CMSyntax.PPCompare (CMSyntax.GT, a, b))
    = evalArith state a > evalArith state b
  | evalCondition state (CMSyntax.PPCompare (CMSyntax.GE, a, b))
    = evalArith state a >= evalArith state b
  | evalCondition state (CMSyntax.PPCompare (CMSyntax.A_EQ, a, b))
    = evalArith state a = evalArith state b
  | evalCondition state (CMSyntax.PPCompare (CMSyntax.A_NE, a, b))
    = evalArith state a <> evalArith state b
  | evalCondition state (CMSyntax.PPNot a)
    = not (evalCondition state a)
  | evalCondition state (CMSyntax.PPBoolBinary (CMSyntax.B_EQ, a, b))
    = evalCondition state a = evalCondition state b
  | evalCondition state (CMSyntax.PPBoolBinary (CMSyntax.B_NE, a, b))
    = evalCondition state a <> evalCondition state b
  | evalCondition state (CMSyntax.PPBoolBinary (CMSyntax.ANDALSO, a, b))
    = evalCondition state a andalso evalCondition state b
  | evalCondition state (CMSyntax.PPBoolBinary (CMSyntax.ORELSE, a, b))
    = evalCondition state a orelse evalCondition state b

fun process ({ definedIn, uses, allFilesRev } : state, CMSyntax.PPJust (CMSyntax.Member { pathname = "$/basis.cm" }))
    = let val pathname = "$(SML_LIB)/basis/basis.mlb"
      in { definedIn = List.foldl CMSyntax.MLSymbolMap.insert' definedIn
                                  [((CMSyntax.SIGNATURE, "MONO_ARRAY"), pathname)
                                  ,((CMSyntax.STRUCTURE, "Array"), pathname)
                                  ,((CMSyntax.STRUCTURE, "ArraySlice"), pathname)
                                  ,((CMSyntax.STRUCTURE, "Bool"), pathname)
                                  ,((CMSyntax.STRUCTURE, "Byte"), pathname)
                                  ,((CMSyntax.STRUCTURE, "Char"), pathname)
                                  ,((CMSyntax.STRUCTURE, "CharVector"), pathname)
                                  ,((CMSyntax.STRUCTURE, "General"), pathname)
                                  ,((CMSyntax.STRUCTURE, "Int"), pathname)
                                  ,((CMSyntax.STRUCTURE, "Int32"), pathname)
                                  ,((CMSyntax.STRUCTURE, "LargeInt"), pathname)
                                  ,((CMSyntax.STRUCTURE, "LargeReal"), pathname)
                                  ,((CMSyntax.STRUCTURE, "LargeWord"), pathname)
                                  ,((CMSyntax.STRUCTURE, "List"), pathname)
                                  ,((CMSyntax.STRUCTURE, "Math"), pathname)
                                  ,((CMSyntax.STRUCTURE, "OS"), pathname)
                                  ,((CMSyntax.STRUCTURE, "Option"), pathname)
                                  ,((CMSyntax.STRUCTURE, "PackWord32Big"), pathname)
                                  ,((CMSyntax.STRUCTURE, "Real"), pathname)
                                  ,((CMSyntax.STRUCTURE, "String"), pathname)
                                  ,((CMSyntax.STRUCTURE, "StringCvt"), pathname)
                                  ,((CMSyntax.STRUCTURE, "Substring"), pathname)
                                  ,((CMSyntax.STRUCTURE, "TextIO"), pathname)
                                  ,((CMSyntax.STRUCTURE, "Time"), pathname)
                                  ,((CMSyntax.STRUCTURE, "Unsafe"), pathname)
                                  ,((CMSyntax.STRUCTURE, "Vector"), pathname)
                                  ,((CMSyntax.STRUCTURE, "Word"), pathname)
                                  ,((CMSyntax.STRUCTURE, "Word64"), pathname)
                                  ,((CMSyntax.STRUCTURE, "Word8"), pathname)
                                  ,((CMSyntax.STRUCTURE, "Word8Array"), pathname)
                                  ,((CMSyntax.STRUCTURE, "Word8ArraySlice"), pathname)
                                  ,((CMSyntax.STRUCTURE, "Word8Vector"), pathname)
                                  ,((CMSyntax.STRUCTURE, "Word8VectorSlice"), pathname)
                                  ]
         , uses = StringMap.insert (uses, pathname, CMSyntax.MLSymbolSet.empty)
         , allFilesRev = pathname :: allFilesRev
         }
      end
  | process ({ definedIn, uses, allFilesRev } : state, CMSyntax.PPJust (CMSyntax.Member { pathname = "$/smlnj-lib.cm" }))
    = let val pathname = "$(SML_LIB)/smlnj-lib/smlnj-lib.mlb"
      in { definedIn = List.foldl CMSyntax.MLSymbolMap.insert' definedIn
                                  []
         , uses = StringMap.insert (uses, pathname, CMSyntax.MLSymbolSet.empty)
         , allFilesRev = pathname :: allFilesRev
         }
      end
  | process ({ definedIn, uses, allFilesRev } : state, CMSyntax.PPJust (CMSyntax.Member { pathname = "$/ml-yacc-lib.cm" }))
    = let val pathname = "$(SML_LIB)/mlyacc-lib/mlyacc-lib.mlb"
      in { definedIn = List.foldl CMSyntax.MLSymbolMap.insert' definedIn
                                  []
         , uses = StringMap.insert (uses, pathname, CMSyntax.MLSymbolSet.empty)
         , allFilesRev = pathname :: allFilesRev
         }
      end
  | process (state as { definedIn, uses, allFilesRev }, CMSyntax.PPJust (CMSyntax.Member { pathname }))
    = let val content = readFile pathname
          val lines = Vector.fromList (String.fields (fn x => x = #"\n") content)
      in case CMAnalyzer.parse ({ languageOptions = LanguageOptions.default }, pathname, lines, content) of
             NONE => state
           | SOME program => let val { definedStructures, definedSignatures, definedFunctors, usedStructures, usedSignatures, usedFunctors } = CMAnalyzer.goProgram program
                                 val definedIn = Syntax.StrIdSet.foldl (fn (Syntax.MkStrId strid, acc) =>
                                                                           case CMSyntax.MLSymbolMap.find (acc, (CMSyntax.STRUCTURE, strid)) of
                                                                               SOME original => (TextIO.output (TextIO.stdErr, "Error: multiple definition of structure " ^ strid ^ ": originally in " ^ original ^ ", now in " ^ pathname ^ "\n"); acc)
                                                                             | NONE => CMSyntax.MLSymbolMap.insert (acc, (CMSyntax.STRUCTURE, strid), pathname)
                                                                       ) definedIn definedStructures
                                 val definedIn = Syntax.SigIdSet.foldl (fn (Syntax.MkSigId sigid, acc) =>
                                                                           case CMSyntax.MLSymbolMap.find (acc, (CMSyntax.SIGNATURE, sigid)) of
                                                                               SOME original => (TextIO.output (TextIO.stdErr, "Error: multiple definition of signature " ^ sigid ^ ": originally in " ^ original ^ ", now in " ^ pathname ^ "\n"); acc)
                                                                             | NONE => CMSyntax.MLSymbolMap.insert (acc, (CMSyntax.SIGNATURE, sigid), pathname)
                                                                       ) definedIn definedSignatures
                                 val definedIn = Syntax.FunIdSet.foldl (fn (Syntax.MkFunId funid, acc) =>
                                                                           case CMSyntax.MLSymbolMap.find (acc, (CMSyntax.FUNCTOR, funid)) of
                                                                               SOME original => (TextIO.output (TextIO.stdErr, "Error: multiple definition of functor " ^ funid ^ ": originally in " ^ original ^ ", now in " ^ pathname ^ "\n"); acc)
                                                                             | NONE => CMSyntax.MLSymbolMap.insert (acc, (CMSyntax.FUNCTOR, funid), pathname)
                                                                       ) definedIn definedFunctors
                                 val set = Syntax.StrIdSet.foldl (fn (Syntax.MkStrId strid, acc) => CMSyntax.MLSymbolSet.add (acc, (CMSyntax.STRUCTURE, strid))) CMSyntax.MLSymbolSet.empty usedStructures
                                 val set = Syntax.SigIdSet.foldl (fn (Syntax.MkSigId sigid, acc) => CMSyntax.MLSymbolSet.add (acc, (CMSyntax.SIGNATURE, sigid))) set usedSignatures
                                 val set = Syntax.FunIdSet.foldl (fn (Syntax.MkFunId funid, acc) => CMSyntax.MLSymbolSet.add (acc, (CMSyntax.FUNCTOR, funid))) set usedFunctors
                             in { definedIn = definedIn, uses = StringMap.insert (uses, pathname, set), allFilesRev = pathname :: allFilesRev }
                             end
      end
  | process (state, CMSyntax.PPConditional (cond, then', else')) = processList (state, if evalCondition state cond then then' else else')
  | process (state, CMSyntax.PPError e) = (TextIO.output (TextIO.stdErr, "#error " ^ e ^ "\n"); state)
and processList (state, xs) = List.foldl (fn (pp, state) => process (state, pp)) state xs

fun graph ({ definedIn, uses, allFilesRev } : state) : StringSet.set StringMap.map (* key: user, value: set of used *)
    = StringMap.mapi (fn (user, syms) =>
                         CMSyntax.MLSymbolSet.foldl (fn (sym, acc) =>
                                                        case CMSyntax.MLSymbolMap.find (definedIn, sym) of
                                                            NONE => (TextIO.output (TextIO.stdErr, "Warning: " ^ CMSyntax.MLSymbol.toString sym ^ " not found\n"); acc)
                                                          | SOME definition => StringSet.add (acc, definition)
                                                    ) StringSet.empty syms
                     ) uses

fun sort (state : state, graph : StringSet.set StringMap.map) : string list
    = let val allFiles = List.rev (#allFilesRev state)
          val color = StringMap.mapi (fn (pathname, _) => ref (String.isSuffix ".mlb" pathname)) graph (* false: not visited yet, true: already visited *)
          val out = ref (List.filter (String.isSuffix ".mlb") (#allFilesRev state)) : string list ref
          fun dfs u = let val () = StringMap.lookup (color, u) := true
                          val neighbor = StringMap.lookup (graph, u)
                      in List.app (fn v => if StringSet.member (neighbor, v) then
                                               if not (!(StringMap.lookup (color, v))) then
                                                   dfs v
                                               else
                                                   ()
                                           else
                                               ()) allFiles
                       ; out := u :: !out
                      end
      in List.app (fn s => if not (!(StringMap.lookup (color, s))) then
                               dfs s
                           else
                               ()) allFiles
       ; !out
      end

fun processExport (state : state, CMSyntax.PPJust (CMSyntax.MLSymbol sym)) = [sym]
  | processExport (state, CMSyntax.PPConditional (cond, then', else')) = processExports (state, if evalCondition state cond then then' else else')
  | processExport (state, CMSyntax.PPError e) = (TextIO.output (TextIO.stdErr, "#error " ^ e ^ "\n"); [])
and processExports (state, xs) = List.concat (List.map (fn pp => processExport (state, pp)) xs)
end;
