#ifndef LUNARML_OPCODE_H
#define LUNARML_OPCODE_H

enum Opcode {
    /* stack: [... <value to be returned>] */
    OP_RETURN = 0,
    /* stack: [...] -> [... 'empty'] */
    OP_PUSH_EMPTY,
    /* stack: [...] -> [... 'unit'] */
    OP_PUSH_UNIT,
    /* stack: [...] -> [... 'nil'] */
    OP_PUSH_NIL,
    /* stack: [...] -> [... 'false'] */
    OP_PUSH_FALSE,
    /* stack: [...] -> [... 'true'] */
    OP_PUSH_TRUE,
    /* args: <index:u16le>, stack: [...] -> [... <const>] */
    OP_PUSH_CONST,
    /* args: <index:u16le>, stack: [...] -> [... <free>] */
    OP_PUSH_FREE,
    /* stack: [... <value to be discarded>] -> [...] */
    OP_POP,
    /* args: <index:u8>, stack: [... <value to be set>] -> [...] */
    OP_POP_AND_SET,
    /* args: <index:u8>, stack: [...] -> [... <value>] */
    OP_COPY_AND_PUSH,
    /* args: <number of arguments:u8>, stack: [... <callee> <arg0> ... <arg(N-1)>] -> [... <result>] */
    OP_CALL,
    /* args: <number of arguments:u8>, stack: [... <callee> <arg0> ... <arg(N-1)>] */
    OP_TAILCALL,
    /* args: <prim op code:u8>, stack: [... <arg0> ... <arg(N-1)>] -> [... <result0> ... <result(N-1)>] */
    OP_PRIMCALL,
    /* args: <length:u16le>, stack: [... <elem0> ... <elem(N-1)>] -> [... <tuple>] */
    OP_MAKE_TUPLE,
    /* args: <index:u16le>, stack: [... <tuple>] -> [... <elem>] */
    OP_PROJECT,
    /* args: <code index:u16le>, stack: [... <free0> ... <free(N-1)>] -> [... <closure>] */
    OP_MAKE_CLOSURE,
    /* args: <free index:u16le>, stack: [... <closure> <new free>] -> [... <closure>] */
    OP_UPDATE_CLOSURE,
    /* args: <length:u16le>, stack: [... <elem0> ... <elem(N-1)>] -> [... <list>] */
    OP_MAKE_LIST,
    /* args: <length:u16le>, stack: [... <elem0> ... <elem(N-1)>] -> [... <vector>] */
    OP_MAKE_VECTOR,
    /* stack: [... <head> <tail>] -> [... <cons>] */
    OP_CONS,
    /* args: <tag:u16le>, stack: [...] -> [... <data>] */
    OP_MAKE_DATA_WITHOUT_PAYLOAD,
    /* args: <tag:u16le>, stack: [... <payload>] -> [... <data>] */
    OP_MAKE_DATA_WITH_PAYLOAD,
    /* stack: [... <data>] -> [... <tag>] */
    OP_DATA_TAG,
    /* stack: [... <data>] -> [... <payload>] */
    OP_DATA_PAYLOAD,
    /* stack: [... <tag>] -> [... <exception>] */
    OP_MAKE_EXCEPTION_WITHOUT_PAYLOAD,
    /* stack: [... <payload> <tag>] -> [... <exception>] */
    OP_MAKE_EXCEPTION_WITH_PAYLOAD,
    /* stack: [... <exception>] -> [... <payload>] */
    OP_EXCEPTION_PAYLOAD,
    /* args: <offset:i16le>, stack: [...] -> [...] */
    OP_JUMP,
    /* args: <offset:i16le>, stack: [... <condition>] -> [...] */
    OP_JUMP_IF_TRUE,
    /* args: <offset:i16le>, control stack: [...] -> [... <saved handler>] */
    OP_PUSH_EXCEPTION_HANDLER,
    /* control stack: [... <saved handler>] -> [...] */
    OP_POP_EXCEPTION_HANDLER,
    /* stack: [... <exception>] */
    OP_RAISE
};

#endif
