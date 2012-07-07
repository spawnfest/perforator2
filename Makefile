REBAR=./rebar

.PHONEY: deps distclean

all: deps compile

deps:
	$(REBAR) get-deps

compile:
	$(REBAR) compile

distclean:
	$(REBAR) delete-deps
