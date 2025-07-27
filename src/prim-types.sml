(*
 * Copyright (c) 2025 ARATA Mizuki
 * This file is part of LunarML.
 *)
structure PrimTypes :>
sig
  structure Names:
  sig
    val bool: TypedSyntax.TyName
    val int: TypedSyntax.TyName
    val int32: TypedSyntax.TyName
    val int54: TypedSyntax.TyName
    val int64: TypedSyntax.TyName
    val intInf: TypedSyntax.TyName
    val word: TypedSyntax.TyName
    val word32: TypedSyntax.TyName
    val word64: TypedSyntax.TyName
    val real: TypedSyntax.TyName
    val char: TypedSyntax.TyName
    val char16: TypedSyntax.TyName
    val string: TypedSyntax.TyName
    val string16: TypedSyntax.TyName
    val exn: TypedSyntax.TyName
    val exntag: TypedSyntax.TyName
    val lua_value: TypedSyntax.TyName
    val js_value: TypedSyntax.TyName
    val ref_: TypedSyntax.TyName
    val list: TypedSyntax.TyName
    val vector: TypedSyntax.TyName
    val array: TypedSyntax.TyName
    val function2: TypedSyntax.TyName
    val function3: TypedSyntax.TyName
    val prompt_tag: TypedSyntax.TyName
    val subcont: TypedSyntax.TyName
  end
  val unit: 'l TypedSyntax.BaseTy
  val bool: 'l TypedSyntax.BaseTy
  val int: 'l TypedSyntax.BaseTy
  val int32: 'l TypedSyntax.BaseTy
  val int54: 'l TypedSyntax.BaseTy
  val int64: 'l TypedSyntax.BaseTy
  val intInf: 'l TypedSyntax.BaseTy
  val word: 'l TypedSyntax.BaseTy
  val word32: 'l TypedSyntax.BaseTy
  val word64: 'l TypedSyntax.BaseTy
  val real: 'l TypedSyntax.BaseTy
  val char: 'l TypedSyntax.BaseTy
  val char16: 'l TypedSyntax.BaseTy
  val string: 'l TypedSyntax.BaseTy
  val string16: 'l TypedSyntax.BaseTy
  val exn: 'l TypedSyntax.BaseTy
  val exntag: 'l TypedSyntax.BaseTy
  val lua_value: 'l TypedSyntax.BaseTy
  val js_value: 'l TypedSyntax.BaseTy
  val ref_: 'l TypedSyntax.BaseTy -> 'l TypedSyntax.BaseTy
  val list: 'l TypedSyntax.BaseTy -> 'l TypedSyntax.BaseTy
  val vector: 'l TypedSyntax.BaseTy -> 'l TypedSyntax.BaseTy
  val array: 'l TypedSyntax.BaseTy -> 'l TypedSyntax.BaseTy
  val function1: 'l TypedSyntax.BaseTy
                 -> 'l TypedSyntax.BaseTy
                 -> 'l TypedSyntax.BaseTy
  val function2: 'l TypedSyntax.BaseTy * 'l TypedSyntax.BaseTy
                 -> 'l TypedSyntax.BaseTy
                 -> 'l TypedSyntax.BaseTy
  val function3:
    'l TypedSyntax.BaseTy * 'l TypedSyntax.BaseTy * 'l TypedSyntax.BaseTy
    -> 'l TypedSyntax.BaseTy
    -> 'l TypedSyntax.BaseTy
  val prompt_tag: 'l TypedSyntax.BaseTy -> 'l TypedSyntax.BaseTy
  val subcont: 'l TypedSyntax.BaseTy * 'l TypedSyntax.BaseTy
               -> 'l TypedSyntax.BaseTy
  val pair: 'l TypedSyntax.BaseTy * 'l TypedSyntax.BaseTy
            -> 'l TypedSyntax.BaseTy
end =
struct
  structure Names =
  struct
    val int = TypedSyntax.MkTyName ("int", 0)
    val word = TypedSyntax.MkTyName ("word", 1)
    val real = TypedSyntax.MkTyName ("real", 2)
    val string = TypedSyntax.MkTyName ("string", 3)
    val char = TypedSyntax.MkTyName ("char", 4)
    val exn = TypedSyntax.MkTyName ("exn", 5)
    val bool = TypedSyntax.MkTyName ("bool", 6)
    val ref_ = TypedSyntax.MkTyName ("ref", 7)
    val list = TypedSyntax.MkTyName ("list", 8)
    val vector = TypedSyntax.MkTyName ("vector", 9)
    val array = TypedSyntax.MkTyName ("array", 10)
    val exntag = TypedSyntax.MkTyName ("exntag", 11)
    val function2 = TypedSyntax.MkTyName ("function2", 12)
    val function3 = TypedSyntax.MkTyName ("function3", 13)
    val char16 = TypedSyntax.MkTyName ("Char16.char", 14)
    val string16 = TypedSyntax.MkTyName ("String16.string", 15)
    val intInf = TypedSyntax.MkTyName ("IntInf.int", 16)
    val lua_value = TypedSyntax.MkTyName ("Lua.value", 17)
    val js_value = TypedSyntax.MkTyName ("JavaScript.value", 18)
    val prompt_tag = TypedSyntax.MkTyName ("DelimCont.prompt_tag", 19)
    val subcont = TypedSyntax.MkTyName ("DelimCont.subcont", 20)
    val int32 = TypedSyntax.MkTyName ("Int32.int", 21)
    val int54 = TypedSyntax.MkTyName ("Int54.int", 22)
    val int64 = TypedSyntax.MkTyName ("Int64.int", 23)
    val word32 = TypedSyntax.MkTyName ("Word32.word", 24)
    val word64 = TypedSyntax.MkTyName ("Word64.word", 25)
  end

  val unit = TypedSyntax.RecordType (SourcePos.nullSpan, Syntax.LabelMap.empty)
  val int = TypedSyntax.TyCon (SourcePos.nullSpan, [], Names.int)
  val word = TypedSyntax.TyCon (SourcePos.nullSpan, [], Names.word)
  val real = TypedSyntax.TyCon (SourcePos.nullSpan, [], Names.real)
  val string = TypedSyntax.TyCon (SourcePos.nullSpan, [], Names.string)
  val char = TypedSyntax.TyCon (SourcePos.nullSpan, [], Names.char)
  val exn = TypedSyntax.TyCon (SourcePos.nullSpan, [], Names.exn)
  val exntag = TypedSyntax.TyCon (SourcePos.nullSpan, [], Names.exntag)
  val bool = TypedSyntax.TyCon (SourcePos.nullSpan, [], Names.bool)
  val char16 = TypedSyntax.TyCon (SourcePos.nullSpan, [], Names.char16)
  val string16 = TypedSyntax.TyCon (SourcePos.nullSpan, [], Names.string16)
  val intInf = TypedSyntax.TyCon (SourcePos.nullSpan, [], Names.intInf)
  val lua_value = TypedSyntax.TyCon (SourcePos.nullSpan, [], Names.lua_value)
  val js_value = TypedSyntax.TyCon (SourcePos.nullSpan, [], Names.js_value)
  val int32 = TypedSyntax.TyCon (SourcePos.nullSpan, [], Names.int32)
  val int54 = TypedSyntax.TyCon (SourcePos.nullSpan, [], Names.int54)
  val int64 = TypedSyntax.TyCon (SourcePos.nullSpan, [], Names.int64)
  val word32 = TypedSyntax.TyCon (SourcePos.nullSpan, [], Names.word32)
  val word64 = TypedSyntax.TyCon (SourcePos.nullSpan, [], Names.word64)
  fun ref_ ty =
    TypedSyntax.TyCon (SourcePos.nullSpan, [ty], Names.ref_)
  fun list ty =
    TypedSyntax.TyCon (SourcePos.nullSpan, [ty], Names.list)
  fun vector ty =
    TypedSyntax.TyCon (SourcePos.nullSpan, [ty], Names.vector)
  fun array ty =
    TypedSyntax.TyCon (SourcePos.nullSpan, [ty], Names.array)
  fun function1 a result = TypedSyntax.FnType (SourcePos.nullSpan, a, result)
  fun function2 (a, b) result =
    TypedSyntax.TyCon (SourcePos.nullSpan, [a, b, result], Names.function2)
  fun function3 (a, b, c) result =
    TypedSyntax.TyCon (SourcePos.nullSpan, [a, b, c, result], Names.function3)
  fun prompt_tag ty =
    TypedSyntax.TyCon (SourcePos.nullSpan, [ty], Names.prompt_tag)
  fun subcont (a, b) =
    TypedSyntax.TyCon (SourcePos.nullSpan, [a, b], Names.subcont)
  fun pair (ty1, ty2) = TypedSyntax.PairType (SourcePos.nullSpan, ty1, ty2)
end;
