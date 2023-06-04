# Variables:
# VARIANT: one of lua, lua-continuations, luajit, nodejs, nodejs-cps

LUNARML= ../bin/lunarml
LUA= lua
LUAJIT= luajit
NODE= node

LUNARML_GEN2= false
VARIANT_GEN2= gen2

ifeq ($(VARIANT), lua)
LUNARML_OPTION= --lua
OUTEXT= .lua
INTERPRETER= $(LUA) -e 'setmetatable(_G,{__index=function(t,k)error("undefined variable "..k,2)end})'
else ifeq ($(VARIANT), lua-continuations)
LUNARML_OPTION= --lua-continuations
OUTEXT= .lua
INTERPRETER= $(LUA) -e 'setmetatable(_G,{__index=function(t,k)error("undefined variable "..k,2)end})'
else ifeq ($(VARIANT), luajit)
LUNARML_OPTION= --luajit
OUTEXT= .lua
INTERPRETER= $(LUAJIT) -e 'setmetatable(_G,{__index=function(t,k)error("undefined variable "..k,2)end})'
else ifeq ($(VARIANT), nodejs)
LUNARML_OPTION= --js
OUTEXT= .js
INTERPRETER= $(NODE)
else ifeq ($(VARIANT), nodejs-cps)
LUNARML_OPTION= --js-cps
OUTEXT= .js
INTERPRETER= $(NODE)
else
$(error VARIANT must be one of lua, lua-continuations, luajit, nodejs or nodejs-cps)
endif

should_run += $(addprefix should_run/,\
  fizzbuzz.sml \
  fun.sml \
  fib_slow.sml \
  fib_linear.sml \
  fib_monoid.sml \
  listrev.sml \
  prime_trialdiv.sml \
  prime_eratos.sml \
  fix_by_datatype.sml \
  fix_by_ref.sml \
  prime_as_typevar.sml \
  open.sml \
  exception.sml \
  local_exception.sml \
  local_datatype.sml \
  equality.sml \
  signature1.sml \
  signature2.sml \
  signature3.sml \
  signature4.sml \
  signature5.sml \
  opaque1.sml \
  opaque2.sml \
  abstype.sml \
  functor1.sml \
  functor2.sml \
  functor3.sml \
  parser_combinator.sml \
  val_projection.sml \
  functor_exception.sml \
  functor_exception_2.sml \
  val-rec.sml \
  datatype-equality.sml \
  capture.sml \
)
ifeq ($(VARIANT), lua)
should_run += should_run/xorshift64.sml
else ifeq ($(VARIANT), lua-continuations)
should_run += should_run/xorshift64.sml
endif
should_compile += $(addprefix should_compile/,\
  signature_sharing1.sml \
  signature_sharing2.sml \
  signature_sharing3.sml \
  withtype.sml \
  sharing_structures.sml \
  generalization1.sml \
  generalization2.sml \
  record.sml \
)
should_not_compile += $(addprefix should_not_compile/,\
  fixity.sml \
  pat.sml \
  typevar_unification.sml \
  typevar_scope.sml \
  local_datatype_1.sml \
  local_datatype_2.sml \
  local_datatype_3.sml \
  datatype_scope.sml \
  ref.sml \
  equality_real.sml \
  equality_fn.sml \
  equality_exn.sml \
  equality_tyvar.sml \
  nonfree_tyvar.sml \
  signature1.sml \
  signature2.sml \
  signature3.sml \
  signature4.sml \
  signature5.sml \
  signature_sharing1.sml \
  signature_sharing2.sml \
  vector.sml \
  syntax_expression_row_label.sml \
  syntax_pattern_row_label.sml \
  syntax_type_expression_row_label.sml \
  syntax_valbind.sml \
  syntax_typbind.sml \
  syntax_datbind.sml \
  syntax_datbind2.sml \
  syntax_exbind.sml \
  opaque1.sml \
  opaque2.sml \
  abstype.sml \
  sharing_structures1.sml \
  sharing_structures2.sml \
  functor1.sml \
  space_in_qualified_id.sml \
  duplicate_datbind.sml \
  duplicate_exbind.sml \
  duplicate_typbind.sml \
  duplicate_val.sml \
  reserved_datbind.sml \
  reserved_datdesc.sml \
  reserved_exbind.sml \
  reserved_exdesc.sml \
  reserved_exrep.sml \
  reserved_valdesc.sml \
  invalid-bound-name1.sml \
  invalid-bound-name2.sml \
  flex-record.sml \
  int-constant.sml \
  int-constant-2.sml \
  word-constant.sml \
  word-constant-2.sml \
  inexact-hex-float.sml \
  inexact-hex-float-2.sml \
  unicode-char.sml \
)
should_run += $(addprefix mlbasis/should_run/,\
  general.sml \
  string.sml \
  string_fields.sml \
  word.sml \
  word_law.sml \
  word8.sml \
  word16.sml \
  word32.sml \
  word64.sml \
  word8-law.sml \
  word16-law.sml \
  word32-law.sml \
  word64-law.sml \
  word_shift.sml \
  xorshift64-word64.sml \
  vector.sml \
  overflow.sml \
  int8.sml \
  int16.sml \
  int32.sml \
  int54.sml \
  int64.sml \
  int-inf-rand.sml \
  int-inf.sml \
  int-inf-fib-hex.sml \
  int-inf-fib.sml \
  int-inf-fib-monoid.sml \
  int-inf-factorial-hex.sml \
  int-inf-wilson.sml \
  seki-bernoulli.sml \
  exn-name.sml \
  real.sml \
  real-intinf.sml \
  substring.sml \
  char.sml \
  scan-word.sml \
)
# int-inf-factorial.sml
ifeq ($(VARIANT), lua)
should_run += mlbasis/should_run/os-path.sml mlbasis/should_run/scan-real.sml lua/should_run/nil_in_vector.sml
else ifeq ($(VARIANT), lua-continuations)
should_run += mlbasis/should_run/os-path.sml mlbasis/should_run/scan-real.sml lua/should_run/nil_in_vector.sml
else ifeq ($(VARIANT), luajit)
should_run += mlbasis/should_run/os-path.sml mlbasis/should_run/scan-real.sml lua/should_run/nil_in_vector.sml
endif
should_run += $(addprefix successor_ml/should_run/,\
  record_extension_pattern.sml \
  record_extension_pattern_2.sml \
  record_extension_pattern_3.sml \
  record_extension_expression.sml \
  record_extension.sml \
  record_extension_2.sml \
  record_update.sml \
  num-underscore.sml \
  binary.sml \
)
should_compile += $(addprefix successor_ml/should_compile/,\
  typealias_in_signature.sml \
  withtype_in_signature.sml \
  abstype.sml \
  record_extension_type.sml \
)
should_not_compile += $(addprefix successor_ml/should_not_compile/,\
  nonexhaustive_bind.sml \
  nonexhaustive_bind_2.sml \
  val_rec_override.sml \
  typealias_in_signature.sml \
  record_extension_pattern_1.sml \
  record_extension_pattern_2.sml \
  record_extension_pattern.sml \
  record_extension_expression.sml \
  record_extension_type.sml \
  num-underscore-1.sml \
  num-underscore-2.sml \
)
should_run += $(addprefix extension/should_run/,\
  vector_exp.sml \
  overload.mlb \
  helloja.sml \
)
should_compile += $(addprefix extension/should_compile/,\
  vector_exp_generalize.sml \
)
ifeq ($(VARIANT), lua-continuations)
should_run += $(addprefix cps/should_run/,\
  product.sml \
  exception.sml \
)
else ifeq ($(VARIANT), nodejs-cps)
should_run += $(addprefix cps/should_run/,\
  product.sml \
  nondet.sml \
  exception.sml \
)
endif

all: test-$(VARIANT)

define should_run_template
test-$(VARIANT): $(basename $1).$(VARIANT).ok
verify-$(VARIANT): $(basename $1).$(VARIANT).verify

$(basename $1).$(VARIANT)$(OUTEXT): $1 $(lunarml)
	@$(LUNARML) compile $(LUNARML_OPTION) --output "$$@" "$$<"

$(basename $1).$(VARIANT).ok: $(basename $1).$(VARIANT)$(OUTEXT)
	@echo "Running $1..."
	@$(INTERPRETER) "$$<" 2>&1 > $(basename $1).$(VARIANT).output
	@diff $(basename $1).stdout $(basename $1).$(VARIANT).output

$(basename $1).$(VARIANT).verify: $1 $(basename $1).$(VARIANT)$(OUTEXT)
	$(LUNARML_GEN2) compile $(LUNARML_OPTION) --output "$(basename $1).$(VARIANT)-$(VARIANT_GEN2)$(OUTEXT)" "$$<"
	diff $(basename $1).$(VARIANT)$(OUTEXT) $(basename $1).$(VARIANT)-$(VARIANT_GEN2)$(OUTEXT)

.PHONY: $(basename $1).$(VARIANT).ok $(basename $1).$(VARIANT).verify

INTERMEDIATE_FILES += $(basename $1).$(VARIANT)$(OUTEXT) $(basename $1).$(VARIANT).output $(basename $1).$(VARIANT)-$(VARIANT_GEN2)$(OUTEXT)

endef

$(foreach file, $(should_run), $(eval $(call should_run_template, $(file))))

define should_compile_template
test-$(VARIANT): $(basename $1).$(VARIANT).ok

$(basename $1).$(VARIANT).ok: $1 $(lunarml)
	@echo "Compiling $1..."
	@$(LUNARML) compile $(LUNARML_OPTION) --output "$(basename $1).$(VARIANT)$(OUTEXT)" "$$<"

.PHONY: $(basename $1).$(VARIANT).ok

INTERMEDIATE_FILES += $(basename $1).$(VARIANT)$(OUTEXT)

endef

$(foreach file, $(should_compile), $(eval $(call should_compile_template, $(file))))

define should_not_compile_template
test-$(VARIANT): $(basename $1).$(VARIANT).ok

$(basename $1).$(VARIANT).ok: $1 $(lunarml)
	@echo "Compiling $1..."
	@! $(LUNARML) compile $(LUNARML_OPTION) --output "$(basename $1).$(VARIANT)$(OUTEXT)" "$$<" >/dev/null 2>&1

.PHONY: $(basename $1).$(VARIANT).ok

INTERMEDIATE_FILES += $(basename $1).$(VARIANT)$(OUTEXT)

endef

$(foreach file, $(should_not_compile), $(eval $(call should_not_compile_template, $(file))))

.PHONY: clean

clean:
	-rm $(INTERMEDIATE_FILES)
