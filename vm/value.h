#ifndef LUNARML_VALUE_H
#define LUNARML_VALUE_H

#include <assert.h>
#include <stddef.h>
#include <stdint.h>
#include <stdbool.h>

typedef uintptr_t Value;

#define V_EMPTY ((Value)0)
#define V_UNIT ((Value)0x01)
#define V_NIL ((Value)0x03)
#define V_FALSE ((Value)0x05)
#define V_TRUE ((Value)0x07)

static inline bool is_boxed(Value v)
{
    return (v & 1) == 0;
}

enum Type {
    T_EMPTY,
    T_UNBOXED,
    T_BOXED_INT32,
    T_BOXED_INT64,
    T_BOXED_WORD32,
    T_BOXED_WORD64,
    T_BOXED_REAL32,
    T_BOXED_REAL64,
    T_BIGINT,
    T_CODE,
    T_CLOSURE,
    T_TUPLE,
    T_VECTOR,
    T_ARRAY,
    T_DATA,
    T_EXCEPTION_TAG,
    T_EXCEPTION,
    T_MONO_SEQUENCE,
    T_REF,
    T_CONS,
    T_PROGRAM
};

struct GCHeader {
    struct GCHeader *next;
    enum Type type;
    bool mark;
};

static inline enum Type get_type(Value v)
{
    if (v & 1) {
        return T_UNBOXED;
    } else if (v == V_EMPTY) {
        return T_EMPTY;
    } else {
        struct GCHeader *obj = (struct GCHeader *)v;
        return obj->type;
    }
}

struct Scalar {
    struct GCHeader header; // type=T_BOXED_INT32|T_BOXED_INT64|T_BOXED_WORD32|T_BOXED_WORD64|T_BOXED_REAL32|T_BOXED_REAL64
    union {
        int64_t i64;
        uint64_t u64;
        float f32;
        double f64;
    };
};

static inline int32_t check_i32(Value v)
{
    if (v & 1) {
        intptr_t x = (intptr_t)v >> 1; // Assume arithmetic right shift
        assert(INT32_MIN <= x && x <= INT32_MAX);
        return (int32_t)x;
    } else {
        assert(get_type(v) == T_BOXED_INT32);
        return (int32_t)((struct Scalar *)v)->i64;
    }
}

static inline int64_t check_i64(Value v)
{
    if (v & 1) {
        intptr_t x = (intptr_t)v >> 1; // Assume arithmetic right shift
        assert(INT64_MIN <= x && x <= INT64_MAX);
        return (int64_t)x;
    } else {
        assert(get_type(v) == T_BOXED_INT64);
        return ((struct Scalar *)v)->i64;
    }
}

static inline uint32_t check_w32(Value v)
{
    if (v & 1) {
        uintptr_t x = (uint32_t)(v >> 1);
        assert(x <= UINT32_MAX);
        return (uint32_t)x;
    } else {
        assert(get_type(v) == T_BOXED_INT32);
        return (int32_t)((struct Scalar *)v)->i64;
    }
}

static inline uint64_t check_w64(Value v)
{
    if (v & 1) {
        uintptr_t x = (uint32_t)(v >> 1);
        assert(x <= UINT64_MAX);
        return (uint32_t)x;
    } else {
        assert(get_type(v) == T_BOXED_INT64);
        return ((struct Scalar *)v)->i64;
    }
}

struct BigInt {
    struct GCHeader header; // type=T_BIGINT
    signed char sign; // -1 or 1
    size_t n_limbs;
    uint64_t limbs[];
};

static inline struct BigInt *check_bigint(Value v)
{
    assert((v & 1) == 0 && v != V_EMPTY);
    struct GCHeader *obj = (struct GCHeader *)v;
    assert(obj->type == T_BIGINT);
    return (struct BigInt *)v;
}

struct Program {
    struct GCHeader header; // type=T_PROGRAM
    size_t n_codes;
    struct Code *codes[];
};

struct Code {
    struct GCHeader header; // type=T_CODE
    struct Program *program;
    size_t code_size;
    uint8_t *code;
    size_t required_stack;
    size_t required_control_stack;
    size_t n_args;
    size_t n_frees;
    size_t n_consts;
    Value consts[];
};

struct Closure {
    struct GCHeader header; // type=T_CLOSURE
    struct Code *code;
    Value free[];
};

static inline struct Closure *check_closure(Value v)
{
    assert((v & 1) == 0 && v != V_EMPTY);
    struct GCHeader *obj = (struct GCHeader *)v;
    assert(obj->type == T_CLOSURE);
    return (struct Closure *)v;
}

struct Tuple {
    struct GCHeader header; // type=T_TUPLE
    size_t n;
    Value elems[];
};

static inline struct Tuple *check_tuple(Value v)
{
    assert((v & 1) == 0 && v != V_EMPTY);
    struct GCHeader *obj = (struct GCHeader *)v;
    assert(obj->type == T_TUPLE);
    return (struct Tuple *)v;
}

struct Sequence {
    struct GCHeader header; // type=T_VECTOR|T_ARRAY
    size_t n;
    Value elems[];
};

static inline struct Sequence *check_vector(Value v)
{
    assert((v & 1) == 0 && v != V_EMPTY);
    struct GCHeader *obj = (struct GCHeader *)v;
    assert(obj->type == T_VECTOR);
    return (struct Sequence *)v;
}

static inline struct Sequence *check_array(Value v)
{
    assert((v & 1) == 0 && v != V_EMPTY);
    struct GCHeader *obj = (struct GCHeader *)v;
    assert(obj->type == T_ARRAY);
    return (struct Sequence *)v;
}

struct Data {
    struct GCHeader header; // type=T_DATA
    uint16_t tag;
    Value payload;
};

static inline struct Data *check_data(Value v)
{
    assert((v & 1) == 0 && v != V_EMPTY);
    struct GCHeader *obj = (struct GCHeader *)v;
    assert(obj->type == T_DATA);
    return (struct Data *)v;
}

struct ExceptionTag {
    struct GCHeader header; // type=T_EXCEPTION_TAG
    char name[]; // NUL-terminated
};

static inline struct ExceptionTag *check_exception_tag(Value v)
{
    assert((v & 1) == 0 && v != V_EMPTY);
    struct GCHeader *obj = (struct GCHeader *)v;
    assert(obj->type == T_EXCEPTION_TAG);
    return (struct ExceptionTag *)v;
}

struct Exception {
    struct GCHeader header; // type=T_EXCEPTION
    struct ExceptionTag *tag;
    Value payload;
};

static inline struct Exception *check_exception(Value v)
{
    assert((v & 1) == 0 && v != V_EMPTY);
    struct GCHeader *obj = (struct GCHeader *)v;
    assert(obj->type == T_EXCEPTION);
    return (struct Exception *)v;
}

struct MonoSequence {
    struct GCHeader header; // type=T_MONO_SEQUENCE
    size_t n_bytes;
    unsigned char buffer[];
};

static inline struct MonoSequence *check_mono_sequence(Value v)
{
    assert((v & 1) == 0 && v != V_EMPTY);
    struct GCHeader *obj = (struct GCHeader *)v;
    assert(obj->type == T_MONO_SEQUENCE);
    return (struct MonoSequence *)v;
}

struct Ref {
    struct GCHeader header; // type=T_REF
    Value payload;
};

static inline struct Ref *check_ref(Value v)
{
    assert((v & 1) == 0 && v != V_EMPTY);
    struct GCHeader *obj = (struct GCHeader *)v;
    assert(obj->type == T_REF);
    return (struct Ref *)v;
}

struct Cons {
    struct GCHeader header; // type=T_CONS
    Value elem;
    struct Cons *next; // may be NULL
};

static inline struct Cons *check_list(Value v)
{
    if (v == V_NIL) {
        return NULL;
    } else {
        assert((v & 1) == 0 && v != V_EMPTY);
        struct GCHeader *obj = (struct GCHeader *)v;
        assert(obj->type == T_CONS);
        return (struct Cons *)v;
    }
}

#endif
