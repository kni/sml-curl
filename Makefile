USER_SML_LIB?=${HOME}/SML

help:
	@echo "target: all (poly mlton ev-poly ev-mlton) clean"
	@echo "make depends && make USER_SML_LIB=lib all"

all: poly mlton ev-poly ev-mlton

poly:
	env USER_SML_LIB=${USER_SML_LIB} polyc -o t-poly t.mlp

ev-poly:
	env USER_SML_LIB=${USER_SML_LIB} polyc -o t-ev-poly t-ev.mlp

mlton:
	mlton -mlb-path-var 'USER_SML_LIB ${USER_SML_LIB}' -default-ann 'allowFFI true' -link-opt -lcurl -output t-mlton t.mlb

ev-mlton:
	mlton -mlb-path-var 'USER_SML_LIB ${USER_SML_LIB}' -default-ann 'allowFFI true' -link-opt -lcurl -output t-ev-mlton t-ev.mlb

depends: lib lib/ev

lib:
	mkdir lib

lib/ev:
	git clone https://github.com/kni/sml-ev.git lib/ev

clean:
	rm -rf lib t-poly t-mlton t-ev-poly t-ev-mlton
