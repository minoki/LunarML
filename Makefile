LUA = lua
LUAJIT = luajit
NODE = node
MKDIR = mkdir -p
CP = cp -fpR
INSTALL_EXEC = install -p -m 0755
SMLFMT = smlfmt

include version.mk

all: bin/lunarml

sources = \
  pluto/token-stream.sig \
  pluto/string-stream.sig \
  pluto/string-stream.sml \
  pluto/parser-combinator.sig \
  pluto/parser-combinator.fun \
  pluto/char-parser.sig \
  pluto/char-parser.fun \
  src/compat/ord_set.sig \
  src/compat/ord_map.sig \
  src/compat/red-black-set-fn.sml \
  src/compat/red-black-map-fn.sml \
  src/util/ord_map_x.sig \
  src/util/map_extra.fun \
  src/void.sml \
  src/list-util.sml \
  src/vector-util.sml \
  src/string-set.sml \
  src/string-map.sml \
  src/numeric.sml \
  src/text.sml \
  src/strongly-connected-components.sml \
  src/language-options-record.sml \
  src/language-options.sml \
  src/target-info.sml \
  src/primitives.sml \
  src/syntax.grm.sig \
  src/sourcepos.sml \
  src/message.sml \
  src/tokenizer.sml \
  src/syntax.sml \
  src/syntax.grm.sml \
  src/parser.sml \
  src/postparsing.sml \
  src/typed.sml \
  src/typing.sml \
  src/initialenv.sml \
  src/printer.sml \
  src/fsyntax.sml \
  src/fprinter.sml \
  src/fcheck.sml \
  src/pattern-match.sml \
  src/ftransform.sml \
  src/cps.sml \
  src/cps/dead-code.sml \
  src/cps/uncurry.sml \
  src/cps/loop.sml \
  src/cps/ref-cell.sml \
  src/cps/inline.sml \
  src/cps/decompose-recursive.sml \
  src/cps/unpack-record-parameter.sml \
  src/cps/eta.sml \
  src/backend.sml \
  src/nested.sml \
  src/lua-syntax.sml \
  src/lua-transform.sml \
  src/codegen-lua.sml \
  src/js-syntax.sml \
  src/js-transform.sml \
  src/codegen-js.sml \
  src/mlb-syntax.sml \
  src/mlb-parser.sml \
  src/driver.sml \
  src/mlb-eval.sml \
  src/command-line-settings.sml \
  src/version.sml \
  src/initfile.sml \
  src/main.sml \
  src/main-default.sml

autogen_sources = \
  src/language-options-record.sml \
  src/primitives.sml \
  src/syntax.grm.sig \
  src/syntax.grm.sml \
  src/command-line-settings.sml \
  src/version.sml

formatted_sources = \
  $(filter-out $(autogen_sources),$(sources)) \
  src/main-esmod.sml

typecheck: src/lunarml-main.mlb $(sources)
	mlton -stop tc -default-ann "warnUnused true" $<

.PHONY: format
format:
	$(SMLFMT) --force $(formatted_sources)

.PHONY: check-format
check-format:
	$(SMLFMT) --check $(formatted_sources)

bin/lunarml: src/lunarml-main.mlb $(sources)
	mlton -output $@ $<

bin/lunarml.gen2.lua: src/lunarml-main.mlb bin/lunarml $(sources)
	bin/lunarml compile --default-ann "valDescInComments error" --internal-consistency-check -o $@ $<

bin/lunarml.gen2-luajit.lua: src/lunarml-main.mlb bin/lunarml $(sources)
	bin/lunarml compile --default-ann "valDescInComments error" --internal-consistency-check --luajit -o $@ $<

bin/lunarml.gen2.mjs: src/lunarml-main.mlb bin/lunarml $(sources)
	bin/lunarml compile --default-ann "valDescInComments error" --internal-consistency-check --nodejs-cps -o $@ $<

bin/lunarml.lua: bin/lunarml.gen2.lua
	echo '#!/usr/bin/env lua' > $@
	cat bin/lunarml.gen2.lua >> $@
	chmod +x $@

bin/lunarml.mjs: bin/lunarml.gen2.mjs
	echo '#!/usr/bin/env node' > $@
	cat bin/lunarml.gen2.mjs >> $@
	chmod +x $@

src/syntax.grm.sml src/syntax.grm.sig: src/syntax.grm
	mlyacc $<

src/primitives.sml: src/primitives.lua
	$(LUA) src/primitives.lua $@ > /dev/null

src/command-line-settings.sml: src/command-line-settings.lua util/record.lua
	$(LUA) util/record.lua $< > $@

src/language-options-record.sml: src/language-options-record.lua util/record.lua
	$(LUA) util/record.lua $< > $@

test: test-lua

test-lua: bin/lunarml
	$(MAKE) -C test VARIANT=lua

test-lua-continuations: bin/lunarml
	$(MAKE) -C test VARIANT=lua-continuations LUA=$(LUA)

test-luajit: bin/lunarml
	$(MAKE) -C test VARIANT=luajit LUAJIT=$(LUAJIT)

test-nodejs: bin/lunarml
	$(MAKE) -C test VARIANT=nodejs NODE=$(NODE)

test-nodejs-cps: bin/lunarml
	$(MAKE) -C test VARIANT=nodejs-cps NODE=$(NODE)

validate-lua: bin/lunarml
	bin/lunarml compile --default-ann "valDescInComments error" -o lunarml.gen2.lua --print-timings --internal-consistency-check src/lunarml-main.mlb
	$(LUA) lunarml.gen2.lua -Blib/lunarml compile --default-ann "valDescInComments error" -o lunarml.gen3.lua --print-timings src/lunarml-main.mlb
	diff --report-identical-files lunarml.gen2.lua lunarml.gen3.lua

validate-luajit: bin/lunarml
	bin/lunarml compile --default-ann "valDescInComments error" -o lunarml.gen2-luajit.lua --luajit --print-timings --internal-consistency-check src/lunarml-main.mlb
	$(LUAJIT) lunarml.gen2-luajit.lua -Blib/lunarml compile --default-ann "valDescInComments error" -o lunarml.gen3-luajit.lua --luajit --print-timings src/lunarml-main.mlb
	diff --report-identical-files lunarml.gen2-luajit.lua lunarml.gen3-luajit.lua

validate-js: bin/lunarml
	bin/lunarml compile --default-ann "valDescInComments error" -o lunarml.gen2.mjs --nodejs-cps --print-timings --internal-consistency-check src/lunarml-main.mlb
	$(NODE) lunarml.gen2.mjs -Blib/lunarml compile --default-ann "valDescInComments error" -o lunarml.gen3.mjs --nodejs-cps --print-timings src/lunarml-main.mlb
	diff --report-identical-files lunarml.gen2.mjs lunarml.gen3.mjs

verify-lua: bin/lunarml bin/lunarml.gen2.lua
	$(MAKE) -C test verify-lua VARIANT=lua LUA=$(LUA) LUNARML_GEN2="$(LUA) ../bin/lunarml.gen2.lua" VARIANT_GEN2=gen2

verify-luajit: bin/lunarml bin/lunarml.gen2-luajit.lua
	$(MAKE) -C test verify-luajit VARIANT=luajit LUAJIT=$(LUAJIT) LUNARML_GEN2="$(LUAJIT) ../bin/lunarml.gen2-luajit.lua" VARIANT_GEN2=gen2-luajit

verify-js: bin/lunarml bin/lunarml.gen2.mjs
	$(MAKE) -C test verify-nodejs-cps VARIANT=nodejs-cps NODE=$(NODE) LUNARML_GEN2="$(NODE) ../bin/lunarml.gen2.mjs" VARIANT_GEN2=gen2

.PHONY: all typecheck test test-lua test-lua-continuations test-luajit test-nodejs test-nodejs-cps validate-lua validate-luajit validate-js verify-lua verify-luajit verify-js

#
# install
#

PREFIX = /usr/local
libdir = $(PREFIX)/lib/lunarml

install: bin/lunarml
	make -C thirdparty install
	$(MKDIR) $(PREFIX)/bin $(PREFIX)/lib
	sed -e "s;__LIBDIR__;$(libdir);" < bin/lunarml-wrapper > $(PREFIX)/bin/lunarml
	chmod a+x $(PREFIX)/bin/lunarml
	$(CP) lib/lunarml/ $(PREFIX)/lib/lunarml
	$(INSTALL_EXEC) bin/lunarml $(libdir)/lunarml

install-precompiled-lua:
	make -C thirdparty install
	$(MKDIR) $(PREFIX)/bin $(PREFIX)/lib
	sed -e "s;__LIBDIR__;$(libdir);" < bin/lunarml-wrapper > $(PREFIX)/bin/lunarml
	chmod a+x $(PREFIX)/bin/lunarml
	$(CP) lib/lunarml/ $(PREFIX)/lib/lunarml
	$(INSTALL_EXEC) bin/lunarml.lua $(libdir)/lunarml

install-precompiled-node:
	make -C thirdparty install
	$(MKDIR) $(PREFIX)/bin $(PREFIX)/lib
	sed -e "s;__LIBDIR__;$(libdir);" < bin/lunarml-wrapper-node > $(PREFIX)/bin/lunarml
	chmod a+x $(PREFIX)/bin/lunarml
	$(CP) lib/lunarml/ $(PREFIX)/lib/lunarml
	$(INSTALL_EXEC) bin/lunarml.mjs $(libdir)/lunarml.mjs

.PHONY: install install-precompiled-lua install-precompiled-node

#
# install-npm
#

package/npm/lunarml.mjs: src/lunarml-esmod.mlb bin/lunarml $(sources) src/main-esmod.sml
	bin/lunarml compile --nodejs-cps --lib --default-ann "valDescInComments error" --internal-consistency-check -o $@ $<

install-npm: package/npm/lunarml.mjs
	make -C thirdparty install
	cp -R lib/ package/npm/lib

.PHONY: install-npm

#
# archive
#

archive: bin/lunarml.lua bin/lunarml.mjs src/syntax.grm.sml src/syntax.grm.sig
	git archive -o "lunarml-$(VERSION).tar.gz" --prefix="lunarml-$(VERSION)/bin/" --add-file=bin/lunarml.lua --add-file=bin/lunarml.mjs --prefix="lunarml-$(VERSION)/src/" --add-file=src/syntax.grm.sml --add-file=src/syntax.grm.sig --prefix="lunarml-$(VERSION)/" HEAD
	git archive -o "lunarml-$(VERSION).zip" --prefix="lunarml-$(VERSION)/bin/" --add-file=bin/lunarml.lua --add-file=bin/lunarml.mjs --prefix="lunarml-$(VERSION)/src/" --add-file=src/syntax.grm.sml --add-file=src/syntax.grm.sig --prefix="lunarml-$(VERSION)/" HEAD

ARCH = $(shell uname -m)
OS = $(shell uname -s | tr A-Z a-z)
ARCH_OS = $(ARCH)-$(OS)

archive-bindist: bin/lunarml src/syntax.grm.sml src/syntax.grm.sig
	git archive -o "lunarml-$(VERSION)-$(ARCH_OS).tar.gz" --prefix="lunarml-$(VERSION)/bin/" --add-file=bin/lunarml --prefix="lunarml-$(VERSION)/src/" --add-file=src/syntax.grm.sml --add-file=src/syntax.grm.sig --prefix="lunarml-$(VERSION)/" HEAD
	git archive -o "lunarml-$(VERSION)-$(ARCH_OS).zip" --prefix="lunarml-$(VERSION)/bin/" --add-file=bin/lunarml --prefix="lunarml-$(VERSION)/src/" --add-file=src/syntax.grm.sml --add-file=src/syntax.grm.sig --prefix="lunarml-$(VERSION)/" HEAD

.PHONY: archive archive-bindist
