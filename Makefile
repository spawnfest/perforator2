REBAR=./rebar

.PHONEY: deps distclean

all: deps compile

deps:
	$(REBAR) get-deps

compile:
	$(REBAR) compile
	make -C priv/js build

distclean:
	$(REBAR) delete-deps
