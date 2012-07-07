REBAR=./rebar

.PHONY: deps distclean test

all: deps compile

deps:
	$(REBAR) get-deps

compile:
	$(REBAR) compile
	make -C priv/js build

distclean:
	$(REBAR) delete-deps

test:
	$(REBAR) skip_deps=true eunit

test_%:
	$(REBAR) skip_deps=true verbose=1 suites=$* eunit

nodeps:
	$(REBAR) compile skip_deps=true
