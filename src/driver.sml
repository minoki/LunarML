(*
 * Copyright (c) 2023 ARATA Mizuki
 * This file is part of LunarML.
 *)
structure Driver :> sig
              type Context = { nextTyVar : int ref
                             , nextVId : int ref
                             , targetInfo : TargetInfo.target_info
                             , errorCounter : Message.counter
                             }
              val newContext : TargetInfo.target_info * Message.counter -> Context
              type Env = { fixity : Fixity.Env
                         , typingEnv : Typing.Env
                         , tynameset : TypedSyntax.TyNameSet.set
                         , toFEnv : ToFSyntax.Env
                         }
              val compile : Context * LanguageOptions.options * Env * string * string -> Env * FSyntax.Dec list
          end = struct

fun rep (c, n) = CharVector.tabulate (n, fn _ => c)
fun untab s = String.map (fn #"\t" => #" " | c => c) s
fun printSpan (name, lines, { start = p1, end_ = p2 } : SourcePos.span) =
    if #file p1 = name andalso #file p2 = name then
        if #line p1 = #line p2 then
            let val l = #line p1 - 1
                val c1 = #column p1 - 1
                val c2 = #column p2 - 1
            in if 0 <= l andalso l < Vector.length lines then
                   let val text = Vector.sub (lines, l)
                       val start = Int.max (0, c1 - 30)
                       val e = Int.min (String.size text, c2 + 30)
                   in TextIO.output (TextIO.stdErr, untab (String.substring (text, start, e - start)) ^ "\n" ^ rep(#" ", c1 - start) ^ "^" ^ rep(#"~", c2 - c1) ^ "\n")
                   end
               else
                   () (* not available *)
            end
        else
            ( let val l1 = #line p1 - 1
                  val c1 = #column p1 - 1
              in if 0 <= l1 andalso l1 < Vector.length lines then
                     let val text = Vector.sub (lines, l1)
                         val start = Int.max (0, c1 - 30)
                         val e = Int.min(String.size text, c1 + 30)
                     in TextIO.output (TextIO.stdErr, untab (String.substring (text, start, e - start)) ^ "\n" ^ rep(#" ", c1 - start) ^ "^" ^ rep(#"~", e - c1 - 1) ^ "\n")
                     end
                 else
                     () (* not available *)
              end
            ; let val l2 = #line p2 - 1
                  val c2 = #column p2 - 1
              in if 0 <= l2 andalso l2 < Vector.length lines then
                     let val text = Vector.sub (lines, l2)
                         val start = Int.max (0, c2 - 30)
                         val e = Int.min(String.size text, c2 + 30)
                     in TextIO.output (TextIO.stdErr, untab (String.substring (text, start, e - start)) ^ "\n" ^ rep(#"~", c2 - start + 1) ^ "\n")
                     end
                 else
                     () (* not available *)
              end
            )
    else
        () (* what to do? *)

type Context = { nextTyVar : int ref
               , nextVId : int ref
               , targetInfo : TargetInfo.target_info
               , errorCounter : Message.counter
               }
fun newContext (targetInfo : TargetInfo.target_info, errorCounter : Message.counter) : Context
    = let val nextTyVar = Typing.newTyVarCounter ()
          val nextVId = Typing.newVIdCounter ()
      in { nextTyVar = nextTyVar
         , nextVId = nextVId
         , targetInfo = targetInfo
         , errorCounter = errorCounter
         }
      end

type Env = { fixity : Fixity.Env
           , typingEnv : Typing.Env
           , tynameset : TypedSyntax.TyNameSet.set
           , toFEnv : ToFSyntax.Env
           }

fun compile ({ nextTyVar, nextVId, targetInfo, errorCounter } : Context, langopt : LanguageOptions.options, origEnv as { fixity, typingEnv, tynameset, toFEnv } : Env, name, source)
    = let val lines = Vector.fromList (String.fields (fn x => x = #"\n") source)
          fun printMessage { spans, domain, message, type_ }
              = let val t = case type_ of
                                Message.WARNING => "warning: "
                              | Message.ERROR => "error: "
                in case spans of
                       [] => TextIO.output (TextIO.stdErr, t ^ message ^ "\n")
                     | { start = p1 as { file = f1, line = l1, column = c1 }, end_ = p2 as { file = f2, line = l2, column = c2 }} :: _ =>
                       ( if f1 = f2 then
                             if p1 = p2 then
                                 TextIO.output (TextIO.stdErr, f1 ^ ":" ^ Int.toString l1 ^ ":" ^ Int.toString c1 ^ ": " ^ t ^ message ^ "\n")
                             else
                                 TextIO.output (TextIO.stdErr, f1 ^ ":" ^ Int.toString l1 ^ ":" ^ Int.toString c1 ^ "-" ^ Int.toString l2 ^ ":" ^ Int.toString c2 ^ ": " ^ t ^ message ^ "\n")
                         else
                             TextIO.output (TextIO.stdErr, f1 ^ ":" ^ Int.toString l1 ^ ":" ^ Int.toString c1 ^ "-" ^ f2 ^ ":" ^ Int.toString l2 ^ ":" ^ Int.toString c2 ^ ": " ^ t ^ message ^ "\n")
                       ; List.app (fn s => printSpan (name, lines, s)) spans
                       )
                end
          val messageHandler = Message.newHandler (errorCounter, printMessage)
          val lexer = LunarMLParser.makeLexer (LunarMLLex.makeInputFromString source) (name, langopt, messageHandler)
          val (decs, _) = let fun onError (message, p1, p2) = Message.error (messageHandler, [{ start = p1, end_ = p2 }], "syntax", message)
                          in LunarMLParser.parse ((* lookahead *) 0, lexer, onError, name)
                          end handle LunarMLParser.ParseError => raise Message.Abort
          val (fixity', decs) = Fixity.doProgram ({ nextVId = nextVId, messageHandler = messageHandler }, fixity, decs)
          val () = CheckSyntacticRestrictions.checkProgram { messageHandler = messageHandler, languageOptions = langopt } decs
          val decs = PostParsing.scopeTyVarsInProgram decs
          val typingContext = { nextTyVar = nextTyVar, nextVId = nextVId, messageHandler = messageHandler, matchContext = [], languageOptions = langopt }
          val (typingEnv', decs) = Typing.typeCheckProgram (typingContext, typingEnv, decs)
          val tynameset = Typing.checkTyScopeOfProgram (typingContext, tynameset, decs)
          val (toFEnv, fdecs) = ToFSyntax.programToFDecs ({ nextVId = nextVId, nextTyVar = nextTyVar, targetInfo = targetInfo, messageHandler = messageHandler }, toFEnv, List.concat decs)
          val () = let val patternMatchContext = { options = langopt, messageHandler = messageHandler }
                   in List.app (fn dec => CheckPatternMatch.goDec (patternMatchContext, dec)) fdecs
                   end
          val modifiedEnv = { fixity = fixity'
                            , typingEnv = typingEnv'
                            , tynameset = tynameset
                            , toFEnv = toFEnv
                            }
      in (modifiedEnv, fdecs)
      end
end; (* structure Driver *)
