#include "vm.h"
#include "opcode.h"
#include <assert.h>
#include <stdlib.h>
#include <stdio.h>

bool check_stack(struct State *state, size_t required_space)
{
    size_t prev_size = state->stack_size;
    size_t n_elems = state->stack_top - state->stack_bottom;
    if (prev_size - n_elems < required_space) {
        size_t new_size = prev_size + prev_size / 2 + required_space; // TODO: Check integer overflow
        Value *new_stack = realloc(state->stack_bottom, sizeof(Value) * new_size); // TODO: Check integer overflow
        if (new_stack == NULL) {
            fprintf(stderr, "fatal error: stack allocation failed (prev_size=%zu, new_size=%zu)\n", prev_size, new_size);
            exit(1);
        }
        state->stack_bottom = new_stack;
        state->stack_top = new_stack + n_elems;
        for (size_t i = n_elems; i < new_size; ++i) {
            new_stack[i] = V_EMPTY;
        }
        return true;
    } else {
        return false;
    }
}

bool check_control_stack(struct State *state, size_t required_space)
{
    size_t prev_size = state->control_stack_size;
    size_t n_elems = state->control_stack_top - state->control_stack_bottom;
    if (prev_size - n_elems < required_space) {
        size_t new_size = prev_size + prev_size / 2 + required_space; // TODO: Check integer overflow
        Label *new_stack = realloc(state->control_stack_bottom, sizeof(Label) * new_size); // TODO: Check integer overflow
        if (new_stack == NULL) {
            fprintf(stderr, "fatal error: control stack allocation failed (prev_size=%zu, new_size=%zu)\n", prev_size, new_size);
            exit(1);
        }
        state->control_stack_bottom = new_stack;
        state->control_stack_top = new_stack + n_elems;
        for (size_t i = n_elems; i < new_size; ++i) {
            new_stack[i].closure = NULL;
            new_stack[i].target = NULL;
            new_stack[i].stack_pos = 0;
        }
        return true;
    } else {
        return false;
    }
}

struct Result run(struct State *state, size_t n_args)
{
    // stack: [bottom] <closure> [frame] <arg[0]> ... <arg[n_args-1]> [top]
    assert(state->stack_top - state->stack_bottom >= n_args + 1);
    Value closure_value = *(state->stack_top - n_args - 1);
    assert(get_type(closure_value) == T_CLOSURE);
    struct Closure *current_closure = (struct Closure *)closure_value;
    struct Code *current_code_object = current_closure->code;
    uint8_t *code = current_code_object->code;
    check_stack(state, current_code_object->required_stack);
    check_control_stack(state, current_code_object->required_control_stack);
    size_t stack_size = state->stack_size;
    Value *stack_top = state->stack_top;
    Value *stack_bottom = state->stack_bottom;
    assert(stack_top - stack_bottom >= current_code_object->n_args);
    Value *current_frame = stack_top - current_code_object->n_args;
    Label *control_stack_top = state->control_stack_top;
    Label *control_stack_bottom = state->control_stack_bottom;
    for (;;) {
        uint8_t opcode = *code++;
        switch (opcode) {
        case OP_RETURN:
            {
                assert(stack_top - stack_bottom >= 1);
                Value retval = *--stack_top;
                if (control_stack_top == control_stack_bottom) {
                    stack_top = current_frame - 1;
                    state->stack_top = stack_top;
                    state->control_stack_top = control_stack_top;
                    return (struct Result){.success = true, .value_or_exception = retval};
                } else {
                    Label *return_address = --control_stack_top;
                    assert(return_address->control_stack_pos == SIZE_MAX);
                    current_closure = return_address->closure;
                    current_code_object = current_closure->code;
                    code = return_address->target;
                    current_frame = stack_bottom + return_address->stack_base;
                    stack_top = stack_bottom + return_address->stack_pos;
                    *stack_top++ = retval;
                }
                break;
            }
        case OP_PUSH_EMPTY:
            {
                assert(stack_top - stack_bottom < stack_size);
                *stack_top++ = V_EMPTY;
                break;
            }
        case OP_PUSH_UNIT:
            {
                assert(stack_top - stack_bottom < stack_size);
                *stack_top++ = V_UNIT;
                break;
            }
        case OP_PUSH_NIL:
            {
                assert(stack_top - stack_bottom < stack_size);
                *stack_top++ = V_NIL;
                break;
            }
        case OP_PUSH_FALSE:
            {
                assert(stack_top - stack_bottom < stack_size);
                *stack_top++ = V_FALSE;
                break;
            }
        case OP_PUSH_TRUE:
            {
                assert(stack_top - stack_bottom < stack_size);
                *stack_top++ = V_TRUE;
                break;
            }
        case OP_PUSH_CONST:
            {
                assert(stack_top - stack_bottom < stack_size);
                uint8_t lo = *code++;
                uint8_t hi = *code++;
                uint16_t index = lo + hi * 256;
                assert(index < current_code_object->n_consts);
                *stack_top++ = current_code_object->consts[index];
                break;
            }
        case OP_PUSH_FREE:
            {
                assert(stack_top - stack_bottom < stack_size);
                uint8_t lo = *code++;
                uint8_t hi = *code++;
                uint16_t index = lo + hi * 256;
                assert(index < current_code_object->n_frees);
                *stack_top++ = current_closure->free[index];
                break;
            }
        case OP_POP:
            {
                assert(stack_top != stack_bottom);
                --stack_top;
                break;
            }
        case OP_POP_AND_SET:
            {
                assert(stack_top != stack_bottom);
                uint8_t index = *code++;
                assert(index < stack_top - current_frame);
                Value val = *--stack_top;
                current_frame[index] = val;
                break;
            }
        case OP_COPY_AND_PUSH:
            {
                assert(stack_top - stack_bottom < stack_size);
                uint8_t index = *code++;
                assert(index < stack_top - current_frame);
                Value val = current_frame[index];
                *stack_top++ = val;
                break;
            }
        case OP_CALL:
            {
                uint8_t n_args = *code++;
                assert(stack_top - current_frame > n_args);
                Value callee_value = *(stack_top - n_args - 1);
                struct Closure *callee = check_closure(callee_value);
                struct Code *new_code_object = callee->code;
                assert(n_args == new_code_object->n_args);
                assert(control_stack_top - control_stack_bottom < state->control_stack_size);
                control_stack_top->closure = current_closure;
                control_stack_top->target = code;
                control_stack_top->stack_base = current_frame - stack_bottom;
                control_stack_top->stack_pos = stack_top - stack_bottom - n_args - 1;
                control_stack_top->control_stack_pos = SIZE_MAX;
                ++control_stack_top;
                current_closure = callee;
                current_code_object = new_code_object;
                code = new_code_object->code;
                if (check_stack(state, new_code_object->required_stack)) { // may trigger GC
                    stack_size = state->stack_size;
                    stack_top = state->stack_top;
                    stack_bottom = state->stack_bottom;
                }
                if (check_control_stack(state, new_code_object->required_control_stack)) { // may trigger GC
                    control_stack_top = state->control_stack_top;
                    control_stack_bottom = state->control_stack_bottom;
                }
                current_frame = stack_top - n_args;
                break;
            }
        case OP_TAILCALL:
            {
                uint8_t n_args = *code++;
                assert(stack_top - current_frame > n_args);
                Value callee_value = *(stack_top - n_args - 1);
                struct Closure *callee = check_closure(callee_value);
                struct Code *new_code_object = callee->code;
                assert(n_args == new_code_object->n_args);
                assert(control_stack_top - control_stack_bottom < state->control_stack_size);
                current_closure = callee;
                current_code_object = new_code_object;
                code = new_code_object->code;
                if (check_stack(state, new_code_object->required_stack)) { // may trigger GC
                    stack_size = state->stack_size;
                    stack_top = state->stack_top;
                    stack_bottom = state->stack_bottom;
                }
                if (check_control_stack(state, new_code_object->required_control_stack)) { // may trigger GC
                    control_stack_top = state->control_stack_top;
                    control_stack_bottom = state->control_stack_bottom;
                }
                *(current_frame - 1) = callee_value;
                for (size_t i = 0; i < n_args; ++i) {
                    current_frame[i] = *(stack_top - n_args + i);
                }
                stack_top = current_frame + n_args;
                break;
            }
        case OP_PRIMCALL:
            {
                uint8_t primop = *code++;
                fprintf(stderr, "primop not implemented yet: %u\n", (unsigned int)primop);
                exit(1);
            }
        case OP_MAKE_TUPLE:
            {
                uint8_t lo = *code++;
                uint8_t hi = *code++;
                uint16_t n = lo + hi * 256;
                assert(n > 0);
                assert(stack_top - current_frame >= n);
                struct Tuple *tuple = make_tuple(state, n); // may trigger GC
                for (size_t i = n; i > 0; --i) {
                    tuple->elems[i - 1] = *--stack_top;
                }
                *stack_top++ = (Value)tuple;
                break;
            }
        case OP_PROJECT:
            {
                uint8_t lo = *code++;
                uint8_t hi = *code++;
                uint16_t index = lo + hi * 256;
                assert(stack_top - current_frame >= 1);
                Value tuple_value = *--stack_top;
                struct Tuple *tuple = check_tuple(tuple_value);
                assert(index < tuple->n);
                *stack_top++ = tuple->elems[index];
                break;
            }
        case OP_MAKE_CLOSURE:
            {
                uint8_t lo = *code++;
                uint8_t hi = *code++;
                uint16_t index = lo + hi * 256;
                struct Program *program = current_code_object->program;
                assert(index < program->n_codes);
                struct Code *new_code = program->codes[index];
                struct Closure *new_closure = make_closure(state, new_code); // may trigger GC
                size_t n_frees = new_code->n_frees;
                assert(stack_top - current_frame >= n_frees);
                for (size_t i = n_frees; i > 0; --i) {
                    new_closure->free[i - 1] = *--stack_top;
                }
                *stack_top++ = (Value)new_closure;
                break;
            }
        case OP_UPDATE_CLOSURE:
            {
                uint8_t lo = *code++;
                uint8_t hi = *code++;
                uint16_t index = lo + hi * 256;
                assert(stack_top - current_frame >= 2);
                Value new_value = *--stack_top;
                Value closure_value = *(stack_top - 1);
                struct Closure *closure = check_closure(closure_value);
                assert(index < closure->code->n_frees);
                closure->free[index] = new_value;
                break;
            }
        case OP_MAKE_LIST:
            {
                uint8_t lo = *code++;
                uint8_t hi = *code++;
                uint16_t n = lo + hi * 256;
                assert(stack_top - current_frame >= n);
                assert(stack_top - stack_bottom - n < stack_size);
                Value *cursor = stack_top;
                Value *init = stack_top - n;
                struct Cons *xs = NULL; // nil
                while (cursor != init) {
                    Value elem = *--cursor;
                    struct Cons *new_cons = make_cons(state); // may trigger GC
                    new_cons->elem = elem;
                    new_cons->next = xs;
                    *--stack_top = (Value)new_cons;
                    xs = new_cons;
                }
                *stack_top++ = xs == NULL ? V_NIL : (Value)xs;
                break;
            }
        case OP_MAKE_VECTOR:
            {
                uint8_t lo = *code++;
                uint8_t hi = *code++;
                uint16_t n = lo + hi * 256;
                assert(stack_top - current_frame >= n);
                assert(stack_top - stack_bottom - n < stack_size);
                Value *cursor = stack_top;
                Value *init = stack_top - n;
                struct Sequence *vector = make_sequence(state, T_VECTOR, n); // may trigger GC
                *stack_top++ = (Value)vector;
                while (cursor != init) {
                    Value elem = *--cursor;
                    vector->elems[cursor - init] = elem;
                }
                *init = (Value)vector;
                stack_top = init + 1;
                break;
            }
        case OP_CONS:
            {
                assert(stack_top - current_frame >= 2);
                struct Cons *cons = make_cons(state); // may trigger GC
                Value next_value = *--stack_top;
                Value elem = *--stack_top;
                cons->elem = elem;
                cons->next = check_list(next_value);
                *stack_top++ = (Value)cons;
                break;
            }
        case OP_MAKE_DATA_WITHOUT_PAYLOAD:
            {
                assert(stack_top - stack_bottom < stack_size);
                uint8_t lo = *code++;
                uint8_t hi = *code++;
                uint16_t tag = lo + hi * 256;
                struct Data *data = make_data(state, tag); // may trigger GC
                *stack_top++ = (Value)data;
                break;
            }
        case OP_MAKE_DATA_WITH_PAYLOAD:
            {
                assert(stack_top - current_frame >= 1);
                uint8_t lo = *code++;
                uint8_t hi = *code++;
                uint16_t tag = lo + hi * 256;
                struct Data *data = make_data(state, tag); // may trigger GC
                data->payload = *--stack_top;
                *stack_top++ = (Value)data;
                break;
            }
        case OP_DATA_TAG:
            {
                assert(stack_top - current_frame >= 1);
                Value v = *--stack_top;
                struct Data *data = check_data(v);
                *stack_top++ = (Value)((data->tag << 1) | 1);
                break;
            }
        case OP_DATA_PAYLOAD:
            {
                assert(stack_top - current_frame >= 1);
                Value v = *--stack_top;
                struct Data *data = check_data(v);
                assert(data->payload != V_EMPTY);
                *stack_top++ = data->payload;
                break;
            }
        case OP_MAKE_EXCEPTION_WITHOUT_PAYLOAD:
            {
                assert(stack_top - current_frame >= 1);
                struct Exception *e = make_exception(state); // may trigger GC
                Value tag = *--stack_top;
                assert(get_type(tag) == T_EXCEPTION_TAG);
                e->tag = (struct ExceptionTag *)tag;
                *stack_top++ = (Value)e;
                break;
            }
        case OP_MAKE_EXCEPTION_WITH_PAYLOAD:
            {
                assert(stack_top - current_frame >= 1);
                struct Exception *e = make_exception(state); // may trigger GC
                Value tag = *--stack_top;
                assert(get_type(tag) == T_EXCEPTION_TAG);
                e->tag = (struct ExceptionTag *)tag;
                e->payload = *--stack_top;
                *stack_top++ = (Value)e;
                break;
            }
        case OP_EXCEPTION_PAYLOAD:
            {
                assert(stack_top - current_frame >= 1);
                Value v = *--stack_top;
                struct Exception *e = check_exception(v);
                assert(e->payload != V_EMPTY);
                *stack_top++ = e->payload;
                break;
            }
        case OP_JUMP:
            {
                uint8_t lo = *code++;
                uint8_t hi = *code++;
                int16_t offset = (int16_t)(uint16_t)(lo + hi * 256);
                assert(0 <= code - current_code_object->code + offset && code - current_code_object->code + offset < current_code_object->code_size);
                code += offset;
                break;
            }
        case OP_JUMP_IF_TRUE:
            {
                uint8_t lo = *code++;
                uint8_t hi = *code++;
                int16_t offset = (int16_t)(uint16_t)(lo + hi * 256);
                assert(0 <= code - current_code_object->code + offset && code - current_code_object->code + offset < current_code_object->code_size);
                Value v = *--stack_top;
                assert(v == V_TRUE || v == V_FALSE);
                if (v == V_TRUE) {
                    code += offset;
                }
                break;
            }
        case OP_PUSH_EXCEPTION_HANDLER:
            {
                uint8_t lo = *code++;
                uint8_t hi = *code++;
                int16_t offset = (int16_t)(uint16_t)(lo + hi * 256);
                assert(0 <= code - current_code_object->code + offset && code - current_code_object->code + offset < current_code_object->code_size);
                assert(control_stack_top - control_stack_bottom < state->control_stack_size);
                *control_stack_top++ = state->current_exception_handler;
                state->current_exception_handler.closure = current_closure;
                state->current_exception_handler.target = code + offset;
                state->current_exception_handler.stack_base = current_frame - stack_bottom;
                state->current_exception_handler.stack_pos = stack_top - stack_bottom;
                state->current_exception_handler.control_stack_pos = control_stack_top - control_stack_bottom;
                break;
            }
        case OP_POP_EXCEPTION_HANDLER:
            {
                assert(control_stack_top - control_stack_bottom >= 1);
                state->current_exception_handler = *--control_stack_top;
                break;
            }
        case OP_RAISE:
            {
                assert(stack_top - stack_bottom >= 1);
                Value e = *--stack_top;
                if (state->current_exception_handler.closure == NULL) {
                    stack_top = current_frame - 1;
                    state->stack_top = stack_top;
                    state->control_stack_top = control_stack_top;
                    return (struct Result){.success = false, .value_or_exception = e};
                } else {
                    assert(control_stack_top - control_stack_bottom >= 1);
                    Label handler = state->current_exception_handler;
                    assert(handler.control_stack_pos != SIZE_MAX);
                    current_closure = handler.closure;
                    current_code_object = current_closure->code;
                    code = handler.target;
                    current_frame = stack_bottom + handler.stack_base;
                    stack_top = stack_bottom + handler.stack_pos;
                    assert(handler.control_stack_pos < state->control_stack_size);
                    control_stack_top = control_stack_bottom + handler.control_stack_pos;
                    *stack_top++ = e;
                    state->current_exception_handler = *--control_stack_top;
                }
                break;
            }
        default:
            {
                fprintf(stderr, "invalid opcode: %u\n", (unsigned int)opcode);
                exit(1);
            }
        }
    }
}
