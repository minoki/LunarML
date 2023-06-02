structure LanguageOptions = struct
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
               }
fun setAllowExtendedNumConsts (allowExtendedNumConsts : bool) ({ allowExtendedNumConsts = _, allowExtendedTextConsts, allowSigWithtype, allowVectorExps, allowVectorPats, allowRecordExtension, allowRecordUpdate, allowUtfEscapeSequences, allowHexFloatConsts, allowValRecTyVars, allowValTyVarsRec, allowFreeTyVarsInDatatypeDec, allowWhereAndType, allowPrim, allowBindEqual, allowOverload, allowInfixingDot, nonexhaustiveBind, nonexhaustiveMatch, nonexhaustiveRaise, redundantBind, redundantMatch, redundantRaise } : options) = { allowExtendedNumConsts = allowExtendedNumConsts, allowExtendedTextConsts = allowExtendedTextConsts, allowSigWithtype = allowSigWithtype, allowVectorExps = allowVectorExps, allowVectorPats = allowVectorPats, allowRecordExtension = allowRecordExtension, allowRecordUpdate = allowRecordUpdate, allowUtfEscapeSequences = allowUtfEscapeSequences, allowHexFloatConsts = allowHexFloatConsts, allowValRecTyVars = allowValRecTyVars, allowValTyVarsRec = allowValTyVarsRec, allowFreeTyVarsInDatatypeDec = allowFreeTyVarsInDatatypeDec, allowWhereAndType = allowWhereAndType, allowPrim = allowPrim, allowBindEqual = allowBindEqual, allowOverload = allowOverload, allowInfixingDot = allowInfixingDot, nonexhaustiveBind = nonexhaustiveBind, nonexhaustiveMatch = nonexhaustiveMatch, nonexhaustiveRaise = nonexhaustiveRaise, redundantBind = redundantBind, redundantMatch = redundantMatch, redundantRaise = redundantRaise }
fun setAllowExtendedTextConsts (allowExtendedTextConsts : bool) ({ allowExtendedNumConsts, allowExtendedTextConsts = _, allowSigWithtype, allowVectorExps, allowVectorPats, allowRecordExtension, allowRecordUpdate, allowUtfEscapeSequences, allowHexFloatConsts, allowValRecTyVars, allowValTyVarsRec, allowFreeTyVarsInDatatypeDec, allowWhereAndType, allowPrim, allowBindEqual, allowOverload, allowInfixingDot, nonexhaustiveBind, nonexhaustiveMatch, nonexhaustiveRaise, redundantBind, redundantMatch, redundantRaise } : options) = { allowExtendedNumConsts = allowExtendedNumConsts, allowExtendedTextConsts = allowExtendedTextConsts, allowSigWithtype = allowSigWithtype, allowVectorExps = allowVectorExps, allowVectorPats = allowVectorPats, allowRecordExtension = allowRecordExtension, allowRecordUpdate = allowRecordUpdate, allowUtfEscapeSequences = allowUtfEscapeSequences, allowHexFloatConsts = allowHexFloatConsts, allowValRecTyVars = allowValRecTyVars, allowValTyVarsRec = allowValTyVarsRec, allowFreeTyVarsInDatatypeDec = allowFreeTyVarsInDatatypeDec, allowWhereAndType = allowWhereAndType, allowPrim = allowPrim, allowBindEqual = allowBindEqual, allowOverload = allowOverload, allowInfixingDot = allowInfixingDot, nonexhaustiveBind = nonexhaustiveBind, nonexhaustiveMatch = nonexhaustiveMatch, nonexhaustiveRaise = nonexhaustiveRaise, redundantBind = redundantBind, redundantMatch = redundantMatch, redundantRaise = redundantRaise }
fun setAllowSigWithtype (allowSigWithtype : bool) ({ allowExtendedNumConsts, allowExtendedTextConsts, allowSigWithtype = _, allowVectorExps, allowVectorPats, allowRecordExtension, allowRecordUpdate, allowUtfEscapeSequences, allowHexFloatConsts, allowValRecTyVars, allowValTyVarsRec, allowFreeTyVarsInDatatypeDec, allowWhereAndType, allowPrim, allowBindEqual, allowOverload, allowInfixingDot, nonexhaustiveBind, nonexhaustiveMatch, nonexhaustiveRaise, redundantBind, redundantMatch, redundantRaise } : options) = { allowExtendedNumConsts = allowExtendedNumConsts, allowExtendedTextConsts = allowExtendedTextConsts, allowSigWithtype = allowSigWithtype, allowVectorExps = allowVectorExps, allowVectorPats = allowVectorPats, allowRecordExtension = allowRecordExtension, allowRecordUpdate = allowRecordUpdate, allowUtfEscapeSequences = allowUtfEscapeSequences, allowHexFloatConsts = allowHexFloatConsts, allowValRecTyVars = allowValRecTyVars, allowValTyVarsRec = allowValTyVarsRec, allowFreeTyVarsInDatatypeDec = allowFreeTyVarsInDatatypeDec, allowWhereAndType = allowWhereAndType, allowPrim = allowPrim, allowBindEqual = allowBindEqual, allowOverload = allowOverload, allowInfixingDot = allowInfixingDot, nonexhaustiveBind = nonexhaustiveBind, nonexhaustiveMatch = nonexhaustiveMatch, nonexhaustiveRaise = nonexhaustiveRaise, redundantBind = redundantBind, redundantMatch = redundantMatch, redundantRaise = redundantRaise }
fun setAllowVectorExps (allowVectorExps : bool) ({ allowExtendedNumConsts, allowExtendedTextConsts, allowSigWithtype, allowVectorExps = _, allowVectorPats, allowRecordExtension, allowRecordUpdate, allowUtfEscapeSequences, allowHexFloatConsts, allowValRecTyVars, allowValTyVarsRec, allowFreeTyVarsInDatatypeDec, allowWhereAndType, allowPrim, allowBindEqual, allowOverload, allowInfixingDot, nonexhaustiveBind, nonexhaustiveMatch, nonexhaustiveRaise, redundantBind, redundantMatch, redundantRaise } : options) = { allowExtendedNumConsts = allowExtendedNumConsts, allowExtendedTextConsts = allowExtendedTextConsts, allowSigWithtype = allowSigWithtype, allowVectorExps = allowVectorExps, allowVectorPats = allowVectorPats, allowRecordExtension = allowRecordExtension, allowRecordUpdate = allowRecordUpdate, allowUtfEscapeSequences = allowUtfEscapeSequences, allowHexFloatConsts = allowHexFloatConsts, allowValRecTyVars = allowValRecTyVars, allowValTyVarsRec = allowValTyVarsRec, allowFreeTyVarsInDatatypeDec = allowFreeTyVarsInDatatypeDec, allowWhereAndType = allowWhereAndType, allowPrim = allowPrim, allowBindEqual = allowBindEqual, allowOverload = allowOverload, allowInfixingDot = allowInfixingDot, nonexhaustiveBind = nonexhaustiveBind, nonexhaustiveMatch = nonexhaustiveMatch, nonexhaustiveRaise = nonexhaustiveRaise, redundantBind = redundantBind, redundantMatch = redundantMatch, redundantRaise = redundantRaise }
fun setAllowVectorPats (allowVectorPats : bool) ({ allowExtendedNumConsts, allowExtendedTextConsts, allowSigWithtype, allowVectorExps, allowVectorPats = _, allowRecordExtension, allowRecordUpdate, allowUtfEscapeSequences, allowHexFloatConsts, allowValRecTyVars, allowValTyVarsRec, allowFreeTyVarsInDatatypeDec, allowWhereAndType, allowPrim, allowBindEqual, allowOverload, allowInfixingDot, nonexhaustiveBind, nonexhaustiveMatch, nonexhaustiveRaise, redundantBind, redundantMatch, redundantRaise } : options) = { allowExtendedNumConsts = allowExtendedNumConsts, allowExtendedTextConsts = allowExtendedTextConsts, allowSigWithtype = allowSigWithtype, allowVectorExps = allowVectorExps, allowVectorPats = allowVectorPats, allowRecordExtension = allowRecordExtension, allowRecordUpdate = allowRecordUpdate, allowUtfEscapeSequences = allowUtfEscapeSequences, allowHexFloatConsts = allowHexFloatConsts, allowValRecTyVars = allowValRecTyVars, allowValTyVarsRec = allowValTyVarsRec, allowFreeTyVarsInDatatypeDec = allowFreeTyVarsInDatatypeDec, allowWhereAndType = allowWhereAndType, allowPrim = allowPrim, allowBindEqual = allowBindEqual, allowOverload = allowOverload, allowInfixingDot = allowInfixingDot, nonexhaustiveBind = nonexhaustiveBind, nonexhaustiveMatch = nonexhaustiveMatch, nonexhaustiveRaise = nonexhaustiveRaise, redundantBind = redundantBind, redundantMatch = redundantMatch, redundantRaise = redundantRaise }
fun setAllowRecordExtension (allowRecordExtension : bool) ({ allowExtendedNumConsts, allowExtendedTextConsts, allowSigWithtype, allowVectorExps, allowVectorPats, allowRecordExtension = _, allowRecordUpdate, allowUtfEscapeSequences, allowHexFloatConsts, allowValRecTyVars, allowValTyVarsRec, allowFreeTyVarsInDatatypeDec, allowWhereAndType, allowPrim, allowBindEqual, allowOverload, allowInfixingDot, nonexhaustiveBind, nonexhaustiveMatch, nonexhaustiveRaise, redundantBind, redundantMatch, redundantRaise } : options) = { allowExtendedNumConsts = allowExtendedNumConsts, allowExtendedTextConsts = allowExtendedTextConsts, allowSigWithtype = allowSigWithtype, allowVectorExps = allowVectorExps, allowVectorPats = allowVectorPats, allowRecordExtension = allowRecordExtension, allowRecordUpdate = allowRecordUpdate, allowUtfEscapeSequences = allowUtfEscapeSequences, allowHexFloatConsts = allowHexFloatConsts, allowValRecTyVars = allowValRecTyVars, allowValTyVarsRec = allowValTyVarsRec, allowFreeTyVarsInDatatypeDec = allowFreeTyVarsInDatatypeDec, allowWhereAndType = allowWhereAndType, allowPrim = allowPrim, allowBindEqual = allowBindEqual, allowOverload = allowOverload, allowInfixingDot = allowInfixingDot, nonexhaustiveBind = nonexhaustiveBind, nonexhaustiveMatch = nonexhaustiveMatch, nonexhaustiveRaise = nonexhaustiveRaise, redundantBind = redundantBind, redundantMatch = redundantMatch, redundantRaise = redundantRaise }
fun setAllowRecordUpdate (allowRecordUpdate : bool) ({ allowExtendedNumConsts, allowExtendedTextConsts, allowSigWithtype, allowVectorExps, allowVectorPats, allowRecordExtension, allowRecordUpdate = _, allowUtfEscapeSequences, allowHexFloatConsts, allowValRecTyVars, allowValTyVarsRec, allowFreeTyVarsInDatatypeDec, allowWhereAndType, allowPrim, allowBindEqual, allowOverload, allowInfixingDot, nonexhaustiveBind, nonexhaustiveMatch, nonexhaustiveRaise, redundantBind, redundantMatch, redundantRaise } : options) = { allowExtendedNumConsts = allowExtendedNumConsts, allowExtendedTextConsts = allowExtendedTextConsts, allowSigWithtype = allowSigWithtype, allowVectorExps = allowVectorExps, allowVectorPats = allowVectorPats, allowRecordExtension = allowRecordExtension, allowRecordUpdate = allowRecordUpdate, allowUtfEscapeSequences = allowUtfEscapeSequences, allowHexFloatConsts = allowHexFloatConsts, allowValRecTyVars = allowValRecTyVars, allowValTyVarsRec = allowValTyVarsRec, allowFreeTyVarsInDatatypeDec = allowFreeTyVarsInDatatypeDec, allowWhereAndType = allowWhereAndType, allowPrim = allowPrim, allowBindEqual = allowBindEqual, allowOverload = allowOverload, allowInfixingDot = allowInfixingDot, nonexhaustiveBind = nonexhaustiveBind, nonexhaustiveMatch = nonexhaustiveMatch, nonexhaustiveRaise = nonexhaustiveRaise, redundantBind = redundantBind, redundantMatch = redundantMatch, redundantRaise = redundantRaise }
fun setAllowUtfEscapeSequences (allowUtfEscapeSequences : bool) ({ allowExtendedNumConsts, allowExtendedTextConsts, allowSigWithtype, allowVectorExps, allowVectorPats, allowRecordExtension, allowRecordUpdate, allowUtfEscapeSequences = _, allowHexFloatConsts, allowValRecTyVars, allowValTyVarsRec, allowFreeTyVarsInDatatypeDec, allowWhereAndType, allowPrim, allowBindEqual, allowOverload, allowInfixingDot, nonexhaustiveBind, nonexhaustiveMatch, nonexhaustiveRaise, redundantBind, redundantMatch, redundantRaise } : options) = { allowExtendedNumConsts = allowExtendedNumConsts, allowExtendedTextConsts = allowExtendedTextConsts, allowSigWithtype = allowSigWithtype, allowVectorExps = allowVectorExps, allowVectorPats = allowVectorPats, allowRecordExtension = allowRecordExtension, allowRecordUpdate = allowRecordUpdate, allowUtfEscapeSequences = allowUtfEscapeSequences, allowHexFloatConsts = allowHexFloatConsts, allowValRecTyVars = allowValRecTyVars, allowValTyVarsRec = allowValTyVarsRec, allowFreeTyVarsInDatatypeDec = allowFreeTyVarsInDatatypeDec, allowWhereAndType = allowWhereAndType, allowPrim = allowPrim, allowBindEqual = allowBindEqual, allowOverload = allowOverload, allowInfixingDot = allowInfixingDot, nonexhaustiveBind = nonexhaustiveBind, nonexhaustiveMatch = nonexhaustiveMatch, nonexhaustiveRaise = nonexhaustiveRaise, redundantBind = redundantBind, redundantMatch = redundantMatch, redundantRaise = redundantRaise }
fun setAllowHexFloatConsts (allowHexFloatConsts : bool) ({ allowExtendedNumConsts, allowExtendedTextConsts, allowSigWithtype, allowVectorExps, allowVectorPats, allowRecordExtension, allowRecordUpdate, allowUtfEscapeSequences, allowHexFloatConsts = _, allowValRecTyVars, allowValTyVarsRec, allowFreeTyVarsInDatatypeDec, allowWhereAndType, allowPrim, allowBindEqual, allowOverload, allowInfixingDot, nonexhaustiveBind, nonexhaustiveMatch, nonexhaustiveRaise, redundantBind, redundantMatch, redundantRaise } : options) = { allowExtendedNumConsts = allowExtendedNumConsts, allowExtendedTextConsts = allowExtendedTextConsts, allowSigWithtype = allowSigWithtype, allowVectorExps = allowVectorExps, allowVectorPats = allowVectorPats, allowRecordExtension = allowRecordExtension, allowRecordUpdate = allowRecordUpdate, allowUtfEscapeSequences = allowUtfEscapeSequences, allowHexFloatConsts = allowHexFloatConsts, allowValRecTyVars = allowValRecTyVars, allowValTyVarsRec = allowValTyVarsRec, allowFreeTyVarsInDatatypeDec = allowFreeTyVarsInDatatypeDec, allowWhereAndType = allowWhereAndType, allowPrim = allowPrim, allowBindEqual = allowBindEqual, allowOverload = allowOverload, allowInfixingDot = allowInfixingDot, nonexhaustiveBind = nonexhaustiveBind, nonexhaustiveMatch = nonexhaustiveMatch, nonexhaustiveRaise = nonexhaustiveRaise, redundantBind = redundantBind, redundantMatch = redundantMatch, redundantRaise = redundantRaise }
fun setAllowValRecTyVars (allowValRecTyVars : bool) ({ allowExtendedNumConsts, allowExtendedTextConsts, allowSigWithtype, allowVectorExps, allowVectorPats, allowRecordExtension, allowRecordUpdate, allowUtfEscapeSequences, allowHexFloatConsts, allowValRecTyVars = _, allowValTyVarsRec, allowFreeTyVarsInDatatypeDec, allowWhereAndType, allowPrim, allowBindEqual, allowOverload, allowInfixingDot, nonexhaustiveBind, nonexhaustiveMatch, nonexhaustiveRaise, redundantBind, redundantMatch, redundantRaise } : options) = { allowExtendedNumConsts = allowExtendedNumConsts, allowExtendedTextConsts = allowExtendedTextConsts, allowSigWithtype = allowSigWithtype, allowVectorExps = allowVectorExps, allowVectorPats = allowVectorPats, allowRecordExtension = allowRecordExtension, allowRecordUpdate = allowRecordUpdate, allowUtfEscapeSequences = allowUtfEscapeSequences, allowHexFloatConsts = allowHexFloatConsts, allowValRecTyVars = allowValRecTyVars, allowValTyVarsRec = allowValTyVarsRec, allowFreeTyVarsInDatatypeDec = allowFreeTyVarsInDatatypeDec, allowWhereAndType = allowWhereAndType, allowPrim = allowPrim, allowBindEqual = allowBindEqual, allowOverload = allowOverload, allowInfixingDot = allowInfixingDot, nonexhaustiveBind = nonexhaustiveBind, nonexhaustiveMatch = nonexhaustiveMatch, nonexhaustiveRaise = nonexhaustiveRaise, redundantBind = redundantBind, redundantMatch = redundantMatch, redundantRaise = redundantRaise }
fun setAllowValTyVarsRec (allowValTyVarsRec : bool) ({ allowExtendedNumConsts, allowExtendedTextConsts, allowSigWithtype, allowVectorExps, allowVectorPats, allowRecordExtension, allowRecordUpdate, allowUtfEscapeSequences, allowHexFloatConsts, allowValRecTyVars, allowValTyVarsRec = _, allowFreeTyVarsInDatatypeDec, allowWhereAndType, allowPrim, allowBindEqual, allowOverload, allowInfixingDot, nonexhaustiveBind, nonexhaustiveMatch, nonexhaustiveRaise, redundantBind, redundantMatch, redundantRaise } : options) = { allowExtendedNumConsts = allowExtendedNumConsts, allowExtendedTextConsts = allowExtendedTextConsts, allowSigWithtype = allowSigWithtype, allowVectorExps = allowVectorExps, allowVectorPats = allowVectorPats, allowRecordExtension = allowRecordExtension, allowRecordUpdate = allowRecordUpdate, allowUtfEscapeSequences = allowUtfEscapeSequences, allowHexFloatConsts = allowHexFloatConsts, allowValRecTyVars = allowValRecTyVars, allowValTyVarsRec = allowValTyVarsRec, allowFreeTyVarsInDatatypeDec = allowFreeTyVarsInDatatypeDec, allowWhereAndType = allowWhereAndType, allowPrim = allowPrim, allowBindEqual = allowBindEqual, allowOverload = allowOverload, allowInfixingDot = allowInfixingDot, nonexhaustiveBind = nonexhaustiveBind, nonexhaustiveMatch = nonexhaustiveMatch, nonexhaustiveRaise = nonexhaustiveRaise, redundantBind = redundantBind, redundantMatch = redundantMatch, redundantRaise = redundantRaise }
fun setAllowFreeTyVarsInDatatypeDec (allowFreeTyVarsInDatatypeDec : bool) ({ allowExtendedNumConsts, allowExtendedTextConsts, allowSigWithtype, allowVectorExps, allowVectorPats, allowRecordExtension, allowRecordUpdate, allowUtfEscapeSequences, allowHexFloatConsts, allowValRecTyVars, allowValTyVarsRec, allowFreeTyVarsInDatatypeDec = _, allowWhereAndType, allowPrim, allowBindEqual, allowOverload, allowInfixingDot, nonexhaustiveBind, nonexhaustiveMatch, nonexhaustiveRaise, redundantBind, redundantMatch, redundantRaise } : options) = { allowExtendedNumConsts = allowExtendedNumConsts, allowExtendedTextConsts = allowExtendedTextConsts, allowSigWithtype = allowSigWithtype, allowVectorExps = allowVectorExps, allowVectorPats = allowVectorPats, allowRecordExtension = allowRecordExtension, allowRecordUpdate = allowRecordUpdate, allowUtfEscapeSequences = allowUtfEscapeSequences, allowHexFloatConsts = allowHexFloatConsts, allowValRecTyVars = allowValRecTyVars, allowValTyVarsRec = allowValTyVarsRec, allowFreeTyVarsInDatatypeDec = allowFreeTyVarsInDatatypeDec, allowWhereAndType = allowWhereAndType, allowPrim = allowPrim, allowBindEqual = allowBindEqual, allowOverload = allowOverload, allowInfixingDot = allowInfixingDot, nonexhaustiveBind = nonexhaustiveBind, nonexhaustiveMatch = nonexhaustiveMatch, nonexhaustiveRaise = nonexhaustiveRaise, redundantBind = redundantBind, redundantMatch = redundantMatch, redundantRaise = redundantRaise }
fun setAllowWhereAndType (allowWhereAndType : bool) ({ allowExtendedNumConsts, allowExtendedTextConsts, allowSigWithtype, allowVectorExps, allowVectorPats, allowRecordExtension, allowRecordUpdate, allowUtfEscapeSequences, allowHexFloatConsts, allowValRecTyVars, allowValTyVarsRec, allowFreeTyVarsInDatatypeDec, allowWhereAndType = _, allowPrim, allowBindEqual, allowOverload, allowInfixingDot, nonexhaustiveBind, nonexhaustiveMatch, nonexhaustiveRaise, redundantBind, redundantMatch, redundantRaise } : options) = { allowExtendedNumConsts = allowExtendedNumConsts, allowExtendedTextConsts = allowExtendedTextConsts, allowSigWithtype = allowSigWithtype, allowVectorExps = allowVectorExps, allowVectorPats = allowVectorPats, allowRecordExtension = allowRecordExtension, allowRecordUpdate = allowRecordUpdate, allowUtfEscapeSequences = allowUtfEscapeSequences, allowHexFloatConsts = allowHexFloatConsts, allowValRecTyVars = allowValRecTyVars, allowValTyVarsRec = allowValTyVarsRec, allowFreeTyVarsInDatatypeDec = allowFreeTyVarsInDatatypeDec, allowWhereAndType = allowWhereAndType, allowPrim = allowPrim, allowBindEqual = allowBindEqual, allowOverload = allowOverload, allowInfixingDot = allowInfixingDot, nonexhaustiveBind = nonexhaustiveBind, nonexhaustiveMatch = nonexhaustiveMatch, nonexhaustiveRaise = nonexhaustiveRaise, redundantBind = redundantBind, redundantMatch = redundantMatch, redundantRaise = redundantRaise }
fun setAllowPrim (allowPrim : bool) ({ allowExtendedNumConsts, allowExtendedTextConsts, allowSigWithtype, allowVectorExps, allowVectorPats, allowRecordExtension, allowRecordUpdate, allowUtfEscapeSequences, allowHexFloatConsts, allowValRecTyVars, allowValTyVarsRec, allowFreeTyVarsInDatatypeDec, allowWhereAndType, allowPrim = _, allowBindEqual, allowOverload, allowInfixingDot, nonexhaustiveBind, nonexhaustiveMatch, nonexhaustiveRaise, redundantBind, redundantMatch, redundantRaise } : options) = { allowExtendedNumConsts = allowExtendedNumConsts, allowExtendedTextConsts = allowExtendedTextConsts, allowSigWithtype = allowSigWithtype, allowVectorExps = allowVectorExps, allowVectorPats = allowVectorPats, allowRecordExtension = allowRecordExtension, allowRecordUpdate = allowRecordUpdate, allowUtfEscapeSequences = allowUtfEscapeSequences, allowHexFloatConsts = allowHexFloatConsts, allowValRecTyVars = allowValRecTyVars, allowValTyVarsRec = allowValTyVarsRec, allowFreeTyVarsInDatatypeDec = allowFreeTyVarsInDatatypeDec, allowWhereAndType = allowWhereAndType, allowPrim = allowPrim, allowBindEqual = allowBindEqual, allowOverload = allowOverload, allowInfixingDot = allowInfixingDot, nonexhaustiveBind = nonexhaustiveBind, nonexhaustiveMatch = nonexhaustiveMatch, nonexhaustiveRaise = nonexhaustiveRaise, redundantBind = redundantBind, redundantMatch = redundantMatch, redundantRaise = redundantRaise }
fun setAllowBindEqual (allowBindEqual : bool) ({ allowExtendedNumConsts, allowExtendedTextConsts, allowSigWithtype, allowVectorExps, allowVectorPats, allowRecordExtension, allowRecordUpdate, allowUtfEscapeSequences, allowHexFloatConsts, allowValRecTyVars, allowValTyVarsRec, allowFreeTyVarsInDatatypeDec, allowWhereAndType, allowPrim, allowBindEqual = _, allowOverload, allowInfixingDot, nonexhaustiveBind, nonexhaustiveMatch, nonexhaustiveRaise, redundantBind, redundantMatch, redundantRaise } : options) = { allowExtendedNumConsts = allowExtendedNumConsts, allowExtendedTextConsts = allowExtendedTextConsts, allowSigWithtype = allowSigWithtype, allowVectorExps = allowVectorExps, allowVectorPats = allowVectorPats, allowRecordExtension = allowRecordExtension, allowRecordUpdate = allowRecordUpdate, allowUtfEscapeSequences = allowUtfEscapeSequences, allowHexFloatConsts = allowHexFloatConsts, allowValRecTyVars = allowValRecTyVars, allowValTyVarsRec = allowValTyVarsRec, allowFreeTyVarsInDatatypeDec = allowFreeTyVarsInDatatypeDec, allowWhereAndType = allowWhereAndType, allowPrim = allowPrim, allowBindEqual = allowBindEqual, allowOverload = allowOverload, allowInfixingDot = allowInfixingDot, nonexhaustiveBind = nonexhaustiveBind, nonexhaustiveMatch = nonexhaustiveMatch, nonexhaustiveRaise = nonexhaustiveRaise, redundantBind = redundantBind, redundantMatch = redundantMatch, redundantRaise = redundantRaise }
fun setAllowOverload (allowOverload : bool) ({ allowExtendedNumConsts, allowExtendedTextConsts, allowSigWithtype, allowVectorExps, allowVectorPats, allowRecordExtension, allowRecordUpdate, allowUtfEscapeSequences, allowHexFloatConsts, allowValRecTyVars, allowValTyVarsRec, allowFreeTyVarsInDatatypeDec, allowWhereAndType, allowPrim, allowBindEqual, allowOverload = _, allowInfixingDot, nonexhaustiveBind, nonexhaustiveMatch, nonexhaustiveRaise, redundantBind, redundantMatch, redundantRaise } : options) = { allowExtendedNumConsts = allowExtendedNumConsts, allowExtendedTextConsts = allowExtendedTextConsts, allowSigWithtype = allowSigWithtype, allowVectorExps = allowVectorExps, allowVectorPats = allowVectorPats, allowRecordExtension = allowRecordExtension, allowRecordUpdate = allowRecordUpdate, allowUtfEscapeSequences = allowUtfEscapeSequences, allowHexFloatConsts = allowHexFloatConsts, allowValRecTyVars = allowValRecTyVars, allowValTyVarsRec = allowValTyVarsRec, allowFreeTyVarsInDatatypeDec = allowFreeTyVarsInDatatypeDec, allowWhereAndType = allowWhereAndType, allowPrim = allowPrim, allowBindEqual = allowBindEqual, allowOverload = allowOverload, allowInfixingDot = allowInfixingDot, nonexhaustiveBind = nonexhaustiveBind, nonexhaustiveMatch = nonexhaustiveMatch, nonexhaustiveRaise = nonexhaustiveRaise, redundantBind = redundantBind, redundantMatch = redundantMatch, redundantRaise = redundantRaise }
fun setAllowInfixingDot (allowInfixingDot : bool) ({ allowExtendedNumConsts, allowExtendedTextConsts, allowSigWithtype, allowVectorExps, allowVectorPats, allowRecordExtension, allowRecordUpdate, allowUtfEscapeSequences, allowHexFloatConsts, allowValRecTyVars, allowValTyVarsRec, allowFreeTyVarsInDatatypeDec, allowWhereAndType, allowPrim, allowBindEqual, allowOverload, allowInfixingDot = _, nonexhaustiveBind, nonexhaustiveMatch, nonexhaustiveRaise, redundantBind, redundantMatch, redundantRaise } : options) = { allowExtendedNumConsts = allowExtendedNumConsts, allowExtendedTextConsts = allowExtendedTextConsts, allowSigWithtype = allowSigWithtype, allowVectorExps = allowVectorExps, allowVectorPats = allowVectorPats, allowRecordExtension = allowRecordExtension, allowRecordUpdate = allowRecordUpdate, allowUtfEscapeSequences = allowUtfEscapeSequences, allowHexFloatConsts = allowHexFloatConsts, allowValRecTyVars = allowValRecTyVars, allowValTyVarsRec = allowValTyVarsRec, allowFreeTyVarsInDatatypeDec = allowFreeTyVarsInDatatypeDec, allowWhereAndType = allowWhereAndType, allowPrim = allowPrim, allowBindEqual = allowBindEqual, allowOverload = allowOverload, allowInfixingDot = allowInfixingDot, nonexhaustiveBind = nonexhaustiveBind, nonexhaustiveMatch = nonexhaustiveMatch, nonexhaustiveRaise = nonexhaustiveRaise, redundantBind = redundantBind, redundantMatch = redundantMatch, redundantRaise = redundantRaise }
fun setNonexhaustiveBind (nonexhaustiveBind : ignore_warn_error) ({ allowExtendedNumConsts, allowExtendedTextConsts, allowSigWithtype, allowVectorExps, allowVectorPats, allowRecordExtension, allowRecordUpdate, allowUtfEscapeSequences, allowHexFloatConsts, allowValRecTyVars, allowValTyVarsRec, allowFreeTyVarsInDatatypeDec, allowWhereAndType, allowPrim, allowBindEqual, allowOverload, allowInfixingDot, nonexhaustiveBind = _, nonexhaustiveMatch, nonexhaustiveRaise, redundantBind, redundantMatch, redundantRaise } : options) = { allowExtendedNumConsts = allowExtendedNumConsts, allowExtendedTextConsts = allowExtendedTextConsts, allowSigWithtype = allowSigWithtype, allowVectorExps = allowVectorExps, allowVectorPats = allowVectorPats, allowRecordExtension = allowRecordExtension, allowRecordUpdate = allowRecordUpdate, allowUtfEscapeSequences = allowUtfEscapeSequences, allowHexFloatConsts = allowHexFloatConsts, allowValRecTyVars = allowValRecTyVars, allowValTyVarsRec = allowValTyVarsRec, allowFreeTyVarsInDatatypeDec = allowFreeTyVarsInDatatypeDec, allowWhereAndType = allowWhereAndType, allowPrim = allowPrim, allowBindEqual = allowBindEqual, allowOverload = allowOverload, allowInfixingDot = allowInfixingDot, nonexhaustiveBind = nonexhaustiveBind, nonexhaustiveMatch = nonexhaustiveMatch, nonexhaustiveRaise = nonexhaustiveRaise, redundantBind = redundantBind, redundantMatch = redundantMatch, redundantRaise = redundantRaise }
fun setNonexhaustiveMatch (nonexhaustiveMatch : ignore_warn_error) ({ allowExtendedNumConsts, allowExtendedTextConsts, allowSigWithtype, allowVectorExps, allowVectorPats, allowRecordExtension, allowRecordUpdate, allowUtfEscapeSequences, allowHexFloatConsts, allowValRecTyVars, allowValTyVarsRec, allowFreeTyVarsInDatatypeDec, allowWhereAndType, allowPrim, allowBindEqual, allowOverload, allowInfixingDot, nonexhaustiveBind, nonexhaustiveMatch = _, nonexhaustiveRaise, redundantBind, redundantMatch, redundantRaise } : options) = { allowExtendedNumConsts = allowExtendedNumConsts, allowExtendedTextConsts = allowExtendedTextConsts, allowSigWithtype = allowSigWithtype, allowVectorExps = allowVectorExps, allowVectorPats = allowVectorPats, allowRecordExtension = allowRecordExtension, allowRecordUpdate = allowRecordUpdate, allowUtfEscapeSequences = allowUtfEscapeSequences, allowHexFloatConsts = allowHexFloatConsts, allowValRecTyVars = allowValRecTyVars, allowValTyVarsRec = allowValTyVarsRec, allowFreeTyVarsInDatatypeDec = allowFreeTyVarsInDatatypeDec, allowWhereAndType = allowWhereAndType, allowPrim = allowPrim, allowBindEqual = allowBindEqual, allowOverload = allowOverload, allowInfixingDot = allowInfixingDot, nonexhaustiveBind = nonexhaustiveBind, nonexhaustiveMatch = nonexhaustiveMatch, nonexhaustiveRaise = nonexhaustiveRaise, redundantBind = redundantBind, redundantMatch = redundantMatch, redundantRaise = redundantRaise }
fun setNonexhaustiveRaise (nonexhaustiveRaise : ignore_warn_error) ({ allowExtendedNumConsts, allowExtendedTextConsts, allowSigWithtype, allowVectorExps, allowVectorPats, allowRecordExtension, allowRecordUpdate, allowUtfEscapeSequences, allowHexFloatConsts, allowValRecTyVars, allowValTyVarsRec, allowFreeTyVarsInDatatypeDec, allowWhereAndType, allowPrim, allowBindEqual, allowOverload, allowInfixingDot, nonexhaustiveBind, nonexhaustiveMatch, nonexhaustiveRaise = _, redundantBind, redundantMatch, redundantRaise } : options) = { allowExtendedNumConsts = allowExtendedNumConsts, allowExtendedTextConsts = allowExtendedTextConsts, allowSigWithtype = allowSigWithtype, allowVectorExps = allowVectorExps, allowVectorPats = allowVectorPats, allowRecordExtension = allowRecordExtension, allowRecordUpdate = allowRecordUpdate, allowUtfEscapeSequences = allowUtfEscapeSequences, allowHexFloatConsts = allowHexFloatConsts, allowValRecTyVars = allowValRecTyVars, allowValTyVarsRec = allowValTyVarsRec, allowFreeTyVarsInDatatypeDec = allowFreeTyVarsInDatatypeDec, allowWhereAndType = allowWhereAndType, allowPrim = allowPrim, allowBindEqual = allowBindEqual, allowOverload = allowOverload, allowInfixingDot = allowInfixingDot, nonexhaustiveBind = nonexhaustiveBind, nonexhaustiveMatch = nonexhaustiveMatch, nonexhaustiveRaise = nonexhaustiveRaise, redundantBind = redundantBind, redundantMatch = redundantMatch, redundantRaise = redundantRaise }
fun setRedundantBind (redundantBind : ignore_warn_error) ({ allowExtendedNumConsts, allowExtendedTextConsts, allowSigWithtype, allowVectorExps, allowVectorPats, allowRecordExtension, allowRecordUpdate, allowUtfEscapeSequences, allowHexFloatConsts, allowValRecTyVars, allowValTyVarsRec, allowFreeTyVarsInDatatypeDec, allowWhereAndType, allowPrim, allowBindEqual, allowOverload, allowInfixingDot, nonexhaustiveBind, nonexhaustiveMatch, nonexhaustiveRaise, redundantBind = _, redundantMatch, redundantRaise } : options) = { allowExtendedNumConsts = allowExtendedNumConsts, allowExtendedTextConsts = allowExtendedTextConsts, allowSigWithtype = allowSigWithtype, allowVectorExps = allowVectorExps, allowVectorPats = allowVectorPats, allowRecordExtension = allowRecordExtension, allowRecordUpdate = allowRecordUpdate, allowUtfEscapeSequences = allowUtfEscapeSequences, allowHexFloatConsts = allowHexFloatConsts, allowValRecTyVars = allowValRecTyVars, allowValTyVarsRec = allowValTyVarsRec, allowFreeTyVarsInDatatypeDec = allowFreeTyVarsInDatatypeDec, allowWhereAndType = allowWhereAndType, allowPrim = allowPrim, allowBindEqual = allowBindEqual, allowOverload = allowOverload, allowInfixingDot = allowInfixingDot, nonexhaustiveBind = nonexhaustiveBind, nonexhaustiveMatch = nonexhaustiveMatch, nonexhaustiveRaise = nonexhaustiveRaise, redundantBind = redundantBind, redundantMatch = redundantMatch, redundantRaise = redundantRaise }
fun setRedundantMatch (redundantMatch : ignore_warn_error) ({ allowExtendedNumConsts, allowExtendedTextConsts, allowSigWithtype, allowVectorExps, allowVectorPats, allowRecordExtension, allowRecordUpdate, allowUtfEscapeSequences, allowHexFloatConsts, allowValRecTyVars, allowValTyVarsRec, allowFreeTyVarsInDatatypeDec, allowWhereAndType, allowPrim, allowBindEqual, allowOverload, allowInfixingDot, nonexhaustiveBind, nonexhaustiveMatch, nonexhaustiveRaise, redundantBind, redundantMatch = _, redundantRaise } : options) = { allowExtendedNumConsts = allowExtendedNumConsts, allowExtendedTextConsts = allowExtendedTextConsts, allowSigWithtype = allowSigWithtype, allowVectorExps = allowVectorExps, allowVectorPats = allowVectorPats, allowRecordExtension = allowRecordExtension, allowRecordUpdate = allowRecordUpdate, allowUtfEscapeSequences = allowUtfEscapeSequences, allowHexFloatConsts = allowHexFloatConsts, allowValRecTyVars = allowValRecTyVars, allowValTyVarsRec = allowValTyVarsRec, allowFreeTyVarsInDatatypeDec = allowFreeTyVarsInDatatypeDec, allowWhereAndType = allowWhereAndType, allowPrim = allowPrim, allowBindEqual = allowBindEqual, allowOverload = allowOverload, allowInfixingDot = allowInfixingDot, nonexhaustiveBind = nonexhaustiveBind, nonexhaustiveMatch = nonexhaustiveMatch, nonexhaustiveRaise = nonexhaustiveRaise, redundantBind = redundantBind, redundantMatch = redundantMatch, redundantRaise = redundantRaise }
fun setRedundantRaise (redundantRaise : ignore_warn_error) ({ allowExtendedNumConsts, allowExtendedTextConsts, allowSigWithtype, allowVectorExps, allowVectorPats, allowRecordExtension, allowRecordUpdate, allowUtfEscapeSequences, allowHexFloatConsts, allowValRecTyVars, allowValTyVarsRec, allowFreeTyVarsInDatatypeDec, allowWhereAndType, allowPrim, allowBindEqual, allowOverload, allowInfixingDot, nonexhaustiveBind, nonexhaustiveMatch, nonexhaustiveRaise, redundantBind, redundantMatch, redundantRaise = _ } : options) = { allowExtendedNumConsts = allowExtendedNumConsts, allowExtendedTextConsts = allowExtendedTextConsts, allowSigWithtype = allowSigWithtype, allowVectorExps = allowVectorExps, allowVectorPats = allowVectorPats, allowRecordExtension = allowRecordExtension, allowRecordUpdate = allowRecordUpdate, allowUtfEscapeSequences = allowUtfEscapeSequences, allowHexFloatConsts = allowHexFloatConsts, allowValRecTyVars = allowValRecTyVars, allowValTyVarsRec = allowValTyVarsRec, allowFreeTyVarsInDatatypeDec = allowFreeTyVarsInDatatypeDec, allowWhereAndType = allowWhereAndType, allowPrim = allowPrim, allowBindEqual = allowBindEqual, allowOverload = allowOverload, allowInfixingDot = allowInfixingDot, nonexhaustiveBind = nonexhaustiveBind, nonexhaustiveMatch = nonexhaustiveMatch, nonexhaustiveRaise = nonexhaustiveRaise, redundantBind = redundantBind, redundantMatch = redundantMatch, redundantRaise = redundantRaise }
fun setByName "allowExtendedNumConsts" = SOME setAllowExtendedNumConsts
  | setByName "allowExtendedTextConsts" = SOME setAllowExtendedTextConsts
  | setByName "allowSigWithtype" = SOME setAllowSigWithtype
  | setByName "allowVectorExps" = SOME setAllowVectorExps
  | setByName "allowVectorPats" = SOME setAllowVectorPats
  | setByName "allowRecordExtension" = SOME setAllowRecordExtension
  | setByName "allowRecordUpdate" = SOME setAllowRecordUpdate
  | setByName "allowUtfEscapeSequences" = SOME setAllowUtfEscapeSequences
  | setByName "allowHexFloatConsts" = SOME setAllowHexFloatConsts
  | setByName "allowValRecTyVars" = SOME setAllowValRecTyVars
  | setByName "allowValTyVarsRec" = SOME setAllowValTyVarsRec
  | setByName "allowFreeTyVarsInDatatypeDec" = SOME setAllowFreeTyVarsInDatatypeDec
  | setByName "allowWhereAndType" = SOME setAllowWhereAndType
  | setByName "allowPrim" = SOME setAllowPrim
  | setByName "allowBindEqual" = SOME setAllowBindEqual
  | setByName "allowOverload" = SOME setAllowOverload
  | setByName "allowInfixingDot" = SOME setAllowInfixingDot
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
                        }
fun setSuccessorML value = setAllowExtendedNumConsts value
                           o setAllowExtendedTextConsts value
                           o setAllowSigWithtype value
                           o setAllowRecordExtension value
                           o setAllowRecordUpdate value
                           o setAllowValRecTyVars value
                           o setAllowValTyVarsRec (not value)
                           o setAllowFreeTyVarsInDatatypeDec (not value)
                           o setAllowWhereAndType (not value)
end;
