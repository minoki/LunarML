#include "value.h"
#include "vm.h"
#include <stdio.h>
#include <stdlib.h>

struct Closure *make_closure(struct State *state, struct Code *code)
{
    size_t n_frees = code->n_frees;
    struct Closure *obj = malloc(sizeof(struct Closure) + sizeof(Value) * n_frees);
    if (obj == NULL) {
        fputs("make_closure: memory allocation failed\n", stderr);
        exit(1);
    }
    obj->header.next = state->objects;
    obj->header.type = T_CLOSURE;
    obj->header.mark = false;
    state->objects = (struct GCHeader *)obj;
    obj->code = code;
    for (size_t i = 0; i < n_frees; ++i) {
        obj->free[i] = V_EMPTY;
    }
    return obj;
}

struct Tuple *make_tuple(struct State *state, size_t n)
{
    struct Tuple *obj = malloc(sizeof(struct Tuple) + sizeof(Value) * n);
    if (obj == NULL) {
        fputs("make_tuple: memory allocation failed\n", stderr);
        exit(1);
    }
    obj->header.next = state->objects;
    obj->header.type = T_TUPLE;
    obj->header.mark = false;
    state->objects = (struct GCHeader *)obj;
    obj->n = n;
    for (size_t i = 0; i < n; ++i) {
        obj->elems[i] = V_EMPTY;
    }
    return obj;
}

struct Sequence *make_sequence(struct State *state, enum Type type, size_t n)
{
    assert(type == T_VECTOR || type == T_ARRAY);
    struct Sequence *obj = malloc(sizeof(struct Sequence) + sizeof(Value) * n);
    if (obj == NULL) {
        fputs("make_sequence: memory allocation failed\n", stderr);
        exit(1);
    }
    obj->header.next = state->objects;
    obj->header.type = type;
    obj->header.mark = false;
    state->objects = (struct GCHeader *)obj;
    obj->n = n;
    for (size_t i = 0; i < n; ++i) {
        obj->elems[i] = V_EMPTY;
    }
    return obj;
}

struct Data *make_data(struct State *state, uint16_t tag)
{
    struct Data *obj = malloc(sizeof(struct Data));
    if (obj == NULL) {
        fputs("make_data: memory allocation failed\n", stderr);
        exit(1);
    }
    obj->header.next = state->objects;
    obj->header.type = T_DATA;
    obj->header.mark = false;
    state->objects = (struct GCHeader *)obj;
    obj->tag = tag;
    obj->payload = V_EMPTY;
    return obj;
}

struct Exception *make_exception(struct State *state)
{
    struct Exception *obj = malloc(sizeof(struct Exception));
    if (obj == NULL) {
        fputs("make_exception: memory allocation failed\n", stderr);
        exit(1);
    }
    obj->header.next = state->objects;
    obj->header.type = T_EXCEPTION;
    obj->header.mark = false;
    state->objects = (struct GCHeader *)obj;
    obj->tag = NULL;
    obj->payload = V_EMPTY;
    return obj;
}

struct Cons *make_cons(struct State *state)
{
    struct Cons *obj = malloc(sizeof(struct Cons));
    if (obj == NULL) {
        fputs("make_cons: memory allocation failed\n", stderr);
        exit(1);
    }
    obj->header.next = state->objects;
    obj->header.type = T_CONS;
    obj->header.mark = false;
    state->objects = (struct GCHeader *)obj;
    obj->elem = V_EMPTY;
    obj->next = NULL;
    return obj;
}
