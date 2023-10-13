(*
 * Copyright (c) 2023 ARATA Mizuki
 * This file is part of LunarML.
 *)
structure LanguageOptions :> sig
              datatype ignore_warn_error = IGNORE | WARN | ERROR
              type options = { allowDoDecls : bool
                             , allowExtendedNumConsts : bool
                             , allowExtendedTextConsts : bool
                             , allowLineComments : bool
                             , allowOptBar : bool
                             , allowOptSemicolon : bool
                             , allowRecordPunExps : bool
                             , allowSigWithtype : bool
                             , allowVectorExps : bool
                             , allowVectorPats : bool
                             , allowRecordExtension : bool
                             , allowRecordUpdate : bool
                             , allowUtfEscapeSequences : bool
                             , allowHexFloatConsts : bool
                             , allowValRecTyVars : bool
                             , allowValTyVarsRec : bool
                             , allowFreeTyVarsInTypeDec : bool
                             , allowWhereAndType : bool
                             , allowPrim : bool
                             , allowBindEqual : bool
                             , allowOverload : bool
                             , allowInfixingDot : bool
                             , nonexhaustiveBind : ignore_warn_error
                             , nonexhaustiveMatch : ignore_warn_error
                             , nonexhaustiveRaise : ignore_warn_error
                             , redundantBind : ignore_warn_error
                             , redundantMatch : ignore_warn_error
                             , redundantRaise : ignore_warn_error
                             , sequenceNonUnit : ignore_warn_error
                             , valDescInComments : ignore_warn_error
                             }
              val default : options
              val setByName : string -> (bool -> options -> options) option
              val setAllowExtendedConsts : bool -> options -> options
              val setAllowVectorExpsAndPats : bool -> options -> options
              val setAllowSuccessorML : bool -> options -> options
              structure set : sig
                            val allowDoDecls : bool -> options -> options
                            val allowExtendedNumConsts : bool -> options -> options
                            val allowExtendedTextConsts : bool -> options -> options
                            val allowLineComments : bool -> options -> options
                            val allowOptBar : bool -> options -> options
                            val allowOptSemicolon : bool -> options -> options
                            val allowRecordPunExps : bool -> options -> options
                            val allowSigWithtype : bool -> options -> options
                            val allowVectorExps : bool -> options -> options
                            val allowVectorPats : bool -> options -> options
                            val allowRecordExtension : bool -> options -> options
                            val allowRecordUpdate : bool -> options -> options
                            val allowUtfEscapeSequences : bool -> options -> options
                            val allowHexFloatConsts : bool -> options -> options
                            val allowValRecTyVars : bool -> options -> options
                            val allowValTyVarsRec : bool -> options -> options
                            val allowFreeTyVarsInTypeDec : bool -> options -> options
                            val allowWhereAndType : bool -> options -> options
                            val allowPrim : bool -> options -> options
                            val allowBindEqual : bool -> options -> options
                            val allowOverload : bool -> options -> options
                            val allowInfixingDot : bool -> options -> options
                            val nonexhaustiveBind : ignore_warn_error -> options -> options
                            val nonexhaustiveMatch : ignore_warn_error -> options -> options
                            val nonexhaustiveRaise : ignore_warn_error -> options -> options
                            val redundantBind : ignore_warn_error -> options -> options
                            val redundantMatch : ignore_warn_error -> options -> options
                            val redundantRaise : ignore_warn_error -> options -> options
                            val sequenceNonUnit : ignore_warn_error -> options -> options
                            val valDescInComments : ignore_warn_error -> options -> options
                        end
          end = struct
open LanguageOptionsRecord
datatype ignore_warn_error = IGNORE | WARN | ERROR
type options = { allowDoDecls : bool
               , allowExtendedNumConsts : bool
               , allowExtendedTextConsts : bool
               , allowLineComments : bool
               , allowOptBar : bool
               , allowOptSemicolon : bool
               , allowRecordPunExps : bool
               , allowSigWithtype : bool
               , allowVectorExps : bool
               , allowVectorPats : bool
               , allowRecordExtension : bool
               , allowRecordUpdate : bool
               , allowUtfEscapeSequences : bool
               , allowHexFloatConsts : bool
               , allowValRecTyVars : bool
               , allowValTyVarsRec : bool
               , allowFreeTyVarsInTypeDec : bool
               , allowWhereAndType : bool
               , allowPrim : bool
               , allowBindEqual : bool
               , allowOverload : bool
               , allowInfixingDot : bool
               , nonexhaustiveBind : ignore_warn_error
               , nonexhaustiveMatch : ignore_warn_error
               , nonexhaustiveRaise : ignore_warn_error
               , redundantBind : ignore_warn_error
               , redundantMatch : ignore_warn_error
               , redundantRaise : ignore_warn_error
               , sequenceNonUnit : ignore_warn_error
               , valDescInComments : ignore_warn_error
               }
fun setAllowExtendedConsts value : options -> options
    = set.allowExtendedNumConsts value
      o set.allowExtendedTextConsts value
fun setAllowVectorExpsAndPats value : options -> options
    = set.allowVectorExps value
      o set.allowVectorPats value
fun setAllowSuccessorML value : options -> options
    = set.allowDoDecls value
      o set.allowExtendedNumConsts value
      o set.allowExtendedTextConsts value
      o set.allowLineComments value
      o set.allowOptBar value
      o set.allowOptSemicolon value
      o set.allowRecordPunExps value
      o set.allowSigWithtype value
      o set.allowRecordExtension value
      o set.allowRecordUpdate value
      o set.allowValRecTyVars value
      o set.allowValTyVarsRec (not value)
      o set.allowFreeTyVarsInTypeDec (not value)
      o set.allowWhereAndType (not value)
fun setByName "allowDoDecls" = SOME set.allowDoDecls
  | setByName "allowExtendedNumConsts" = SOME set.allowExtendedNumConsts
  | setByName "allowExtendedTextConsts" = SOME set.allowExtendedTextConsts
  | setByName "allowLineComments" = SOME set.allowLineComments
  | setByName "allowOptBar" = SOME set.allowOptBar
  | setByName "allowOptSemicolon" = SOME set.allowOptSemicolon
  | setByName "allowRecordPunExps" = SOME set.allowRecordPunExps
  | setByName "allowSigWithtype" = SOME set.allowSigWithtype
  | setByName "allowVectorExps" = SOME set.allowVectorExps
  | setByName "allowVectorPats" = SOME set.allowVectorPats
  | setByName "allowRecordExtension" = SOME set.allowRecordExtension
  | setByName "allowRecordUpdate" = SOME set.allowRecordUpdate
  | setByName "allowUtfEscapeSequences" = SOME set.allowUtfEscapeSequences
  | setByName "allowHexFloatConsts" = SOME set.allowHexFloatConsts
  | setByName "allowValRecTyVars" = SOME set.allowValRecTyVars
  | setByName "allowValTyVarsRec" = SOME set.allowValTyVarsRec
  | setByName "allowFreeTyVarsInTypeDec" = SOME set.allowFreeTyVarsInTypeDec
  | setByName "allowWhereAndType" = SOME set.allowWhereAndType
  | setByName "allowPrim" = SOME set.allowPrim
  | setByName "allowBindEqual" = SOME set.allowBindEqual
  | setByName "allowOverload" = SOME set.allowOverload
  | setByName "allowInfixingDot" = SOME set.allowInfixingDot
  | setByName "allowExtendedConsts" = SOME setAllowExtendedConsts
  | setByName "allowVectorExpsAndPats" = SOME setAllowVectorExpsAndPats
  | setByName "allowSuccessorML" = SOME setAllowSuccessorML
  | setByName (_ : string) : (bool -> options -> options) option = NONE
val default : options = { allowDoDecls = true
                        , allowExtendedNumConsts = true
                        , allowExtendedTextConsts = true
                        , allowLineComments = true
                        , allowOptBar = true
                        , allowOptSemicolon = true
                        , allowRecordPunExps = true
                        , allowSigWithtype = true
                        , allowVectorExps = true
                        , allowVectorPats = true
                        , allowRecordExtension = true
                        , allowRecordUpdate = true
                        , allowUtfEscapeSequences = true
                        , allowHexFloatConsts = true
                        , allowValRecTyVars = true
                        , allowValTyVarsRec = true
                        , allowFreeTyVarsInTypeDec = true
                        , allowWhereAndType = true
                        , allowPrim = false
                        , allowBindEqual = false
                        , allowOverload = false
                        , allowInfixingDot = false
                        , nonexhaustiveBind = WARN
                        , nonexhaustiveMatch = WARN
                        , nonexhaustiveRaise = IGNORE
                        , redundantBind = WARN
                        , redundantMatch = WARN
                        , redundantRaise = WARN
                        , sequenceNonUnit = IGNORE
                        , valDescInComments = IGNORE
                        }
end;
