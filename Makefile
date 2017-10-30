help:
	@echo "target: all (poly mlton ev-poly ev-mlton) clean"

all: poly mlton ev-poly ev-mlton

poly:
	polyc -o t-poly t.mlp

ev-poly: ev
	polyc -o t-ev-poly t-ev.mlp

mlton:
	mlton -default-ann 'allowFFI true' -link-opt -lcurl -output t-mlton t.mlb

ev-mlton: ev
	mlton -default-ann 'allowFFI true' -link-opt -lcurl -output t-ev-mlton t-ev.mlb

ev:
	git clone https://github.com/kni/sml-ev.git ev

sml-ev: ev

clean:
	rm -rf t-poly t-mlton t-ev-poly t-ev-mlton
