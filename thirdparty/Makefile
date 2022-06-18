all: smlnj-lib/Util/smlnj-lib.mlb ml-yacc/lib/mlyacc-lib.mlb

#
# smlnj-lib
#

smlnj-lib.tgz:
	curl -LO http://smlnj.cs.uchicago.edu/dist/working/2021.1/smlnj-lib.tgz
	echo "8448a25a70bb9e72e66a1a9c5204b13e35ec2b0d0c05d6fec39fb9d2a2c3041d *smlnj-lib.tgz" | shasum -a 256 -b -c

smlnj-lib/Util/smlnj-lib.mlb: smlnj-lib.tgz
	tar -zxf smlnj-lib.tgz
	patch -N -p 1 -d smlnj-lib < smlnj-lib-lunarml.patch

smlnj-lib: smlnj-lib/Util/smlnj-lib.mlb

# For LunarML maintainer
smlnj-lib-lunarml.patch:
	diff --recursive -u -N smlnj-lib-original smlnj-lib-lunarml > smlnj-lib-lunarml.patch || true

.PHONY: smlnj-lib smlnj-lib-lunarml.patch

#
# mlyacc-lib
#

ml-yacc.tgz:
	curl -LO http://smlnj.cs.uchicago.edu/dist/working/2021.1/ml-yacc.tgz
	echo "bdf26afcba783f6ee5cbd49b48d7966604d91bee625f72677f192750aec1d688 *ml-yacc.tgz" | shasum -a 256 -b -c

ml-yacc/lib/sources.cm: ml-yacc.tgz
	tar -zxf ml-yacc.tgz
	touch $@

ml-yacc/lib/mlyacc-lib.mlb: mlyacc-lib.mlb ml-yacc/lib/sources.cm
	cp $< $@

ml-yacc: ml-yacc/lib/mlyacc-lib.mlb

.PHONY: ml-yacc
