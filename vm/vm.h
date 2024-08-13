#ifndef LUNARML_VM_H
#define LUNARML_VM_H

#include "opcode.h"
#include "value.h"
#include <stddef.h>

typedef struct Label {
    struct Closure *closure;
    uint8_t *target;
    size_t stack_base;
    size_t stack_pos;
    size_t control_stack_pos; // SIZE_MAX if function call
} Label;

struct State {
    struct GCHeader *objects;
    size_t stack_size;
    Value *stack_top;
    Value *stack_bottom;
    size_t control_stack_size;
    Label *control_stack_top;
    Label *control_stack_bottom;
    Label current_exception_handler;
};

struct Closure *make_closure(struct State *state, struct Code *code);
struct Tuple *make_tuple(struct State *state, size_t n);
struct Data *make_data(struct State *state, uint16_t tag);
struct Exception *make_exception(struct State *state);
struct Cons *make_cons(struct State *state);

struct Result {
    bool success;
    Value value_or_exception;
};

struct Result run(struct State *state, size_t n_args);

#endif
