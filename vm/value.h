#ifndef LUNARML_VALUE_H
#define LUNARML_VALUE_H

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
    T_BOXED_INT,
    T_BOXED_WORD,
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
    struct GCHeader header; // type=T_BOXED_INT|T_BOXED_WORD|T_BOXED_REAL32|T_BOXED_REAL64
    union {
        int64_t i64;
        uint64_t u64;
        float f32;
        double f64;
    };
};

struct BigInt {
    struct GCHeader header; // type=T_BIGINT
    signed char sign; // -1 or 1
    size_t n_limbs;
    uint64_t limbs[];
};

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

struct Tuple {
    struct GCHeader header; // type=T_TUPLE
    size_t n;
    Value elems[];
};

struct Sequence {
    struct GCHeader header; // type=T_VECTOR|T_ARRAY
    size_t n;
    Value elems[];
};

struct Data {
    struct GCHeader header; // type=T_DATA
    uint16_t tag;
    Value payload;
};

struct ExceptionTag {
    struct GCHeader header; // type=T_EXCEPTION_TAG
    char name[]; // NUL-terminated
};

struct Exception {
    struct GCHeader header; // type=T_EXCEPTION
    struct ExceptionTag *tag;
    Value payload;
};

struct MonoSequence {
    struct GCHeader header; // type=T_MONO_SEQUENCE
    size_t n_bytes;
    unsigned char buffer[];
};

struct Ref {
    struct GCHeader header; // type=T_REF
    Value payload;
};

struct Cons {
    struct GCHeader header; // type=T_CONS
    Value elem;
    struct Cons *next; // may be NULL
};

#endif
