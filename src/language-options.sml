structure LanguageOptions = struct
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
               , allowSignatureComment : bool
               , nonexhaustiveBind : ignore_warn_error
               , nonexhaustiveMatch : ignore_warn_error
               , nonexhaustiveRaise : ignore_warn_error
               , redundantBind : ignore_warn_error
               , redundantMatch : ignore_warn_error
               , redundantRaise : ignore_warn_error
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
  | setByName "allowSignatureComment" = SOME set.allowSignatureComment
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
                        , allowSignatureComment = false
                        , nonexhaustiveBind = WARN
                        , nonexhaustiveMatch = WARN
                        , nonexhaustiveRaise = IGNORE
                        , redundantBind = WARN
                        , redundantMatch = WARN
                        , redundantRaise = WARN
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
