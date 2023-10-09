(*
 * Copyright (c) 2023 ARATA Mizuki
 * This file is part of LunarML.
 *)
structure LanguageOptions :> sig
              datatype ignore_warn_error = IGNORE | WARN | ERROR
              type options = { allowExtendedNumConsts : bool
                             , allowExtendedTextConsts : bool
                             , allowSigWithtype : bool
                             , allowVectorExps : bool
                             , allowVectorPats : bool
                             , allowRecordExtension : bool
                             , allowRecordUpdate : bool
                             , allowUtfEscapeSequences : bool
                             , allowHexFloatConsts : bool
                             , allowValRecTyVars : bool
                             , allowValTyVarsRec : bool
                             , allowFreeTyVarsInDatatypeDec : bool
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
              val setSuccessorML : bool -> options -> options
              structure set : sig
                            val allowExtendedNumConsts : bool -> options -> options
                            val allowExtendedTextConsts : bool -> options -> options
                            val allowSigWithtype : bool -> options -> options
                            val allowVectorExps : bool -> options -> options
                            val allowVectorPats : bool -> options -> options
                            val allowRecordExtension : bool -> options -> options
                            val allowRecordUpdate : bool -> options -> options
                            val allowUtfEscapeSequences : bool -> options -> options
                            val allowHexFloatConsts : bool -> options -> options
                            val allowValRecTyVars : bool -> options -> options
                            val allowValTyVarsRec : bool -> options -> options
                            val allowFreeTyVarsInDatatypeDec : bool -> options -> options
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
type options = { allowExtendedNumConsts : bool
               , allowExtendedTextConsts : bool
               , allowSigWithtype : bool
               , allowVectorExps : bool
               , allowVectorPats : bool
               , allowRecordExtension : bool
               , allowRecordUpdate : bool
               , allowUtfEscapeSequences : bool
               , allowHexFloatConsts : bool
               , allowValRecTyVars : bool
               , allowValTyVarsRec : bool
               , allowFreeTyVarsInDatatypeDec : bool
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
fun setByName "allowExtendedNumConsts" = SOME set.allowExtendedNumConsts
  | setByName "allowExtendedTextConsts" = SOME set.allowExtendedTextConsts
  | setByName "allowSigWithtype" = SOME set.allowSigWithtype
  | setByName "allowVectorExps" = SOME set.allowVectorExps
  | setByName "allowVectorPats" = SOME set.allowVectorPats
  | setByName "allowRecordExtension" = SOME set.allowRecordExtension
  | setByName "allowRecordUpdate" = SOME set.allowRecordUpdate
  | setByName "allowUtfEscapeSequences" = SOME set.allowUtfEscapeSequences
  | setByName "allowHexFloatConsts" = SOME set.allowHexFloatConsts
  | setByName "allowValRecTyVars" = SOME set.allowValRecTyVars
  | setByName "allowValTyVarsRec" = SOME set.allowValTyVarsRec
  | setByName "allowFreeTyVarsInDatatypeDec" = SOME set.allowFreeTyVarsInDatatypeDec
  | setByName "allowWhereAndType" = SOME set.allowWhereAndType
  | setByName "allowPrim" = SOME set.allowPrim
  | setByName "allowBindEqual" = SOME set.allowBindEqual
  | setByName "allowOverload" = SOME set.allowOverload
  | setByName "allowInfixingDot" = SOME set.allowInfixingDot
  | setByName (_ : string) : (bool -> options -> options) option = NONE
val default : options = { allowExtendedNumConsts = true
                        , allowExtendedTextConsts = true
                        , allowSigWithtype = true
                        , allowVectorExps = true
                        , allowVectorPats = true
                        , allowRecordExtension = true
                        , allowRecordUpdate = true
                        , allowUtfEscapeSequences = true
                        , allowHexFloatConsts = true
                        , allowValRecTyVars = true
                        , allowValTyVarsRec = true
                        , allowFreeTyVarsInDatatypeDec = true
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
fun setSuccessorML value : options -> options
    = set.allowExtendedNumConsts value
      o set.allowExtendedTextConsts value
      o set.allowSigWithtype value
      o set.allowRecordExtension value
      o set.allowRecordUpdate value
      o set.allowValRecTyVars value
      o set.allowValTyVarsRec (not value)
      o set.allowFreeTyVarsInDatatypeDec (not value)
      o set.allowWhereAndType (not value)
end;
