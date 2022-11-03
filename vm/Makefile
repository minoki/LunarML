LUA = lua

all: test-program

ir.lex.sml: ir.lex
	mllex $<

ir.grm.sml ir.grm.sig: ir.grm
	mlyacc $<

vm.sml: vm.sml.in opcode.lua
	$(LUA) opcode.lua translate $< $@

compile.sml: compile.sml.in opcode.lua
	$(LUA) opcode.lua translate $< $@

primitives.sml: opcode.lua
	$(LUA) opcode.lua gen-primitives $@

disasm.sml: opcode.lua
	$(LUA) opcode.lua gen-disasm $@

test-program: test.mlb syntax.sml ir.grm.sig ir.lex.sml ir.grm.sml parser.sml value.sml compile.sml vm.sml test.sml disasm.sml primitives.sml
	mlton -output $@ test.mlb
