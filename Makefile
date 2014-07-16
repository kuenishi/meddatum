.PHONY: all clean test compile

all:
	./rebar get-deps
	./rebar compile
	./rebar escriptize skip_deps=true
## ./rebar update-deps

compile:
	./rebar compile

clean:
	./rebar clean

DIALYZER_APPS = kernel stdlib sasl erts ssl tools os_mon runtime_tools crypto inets \
	webtool eunit syntax_tools compiler
PLT ?= $(HOME)/.meddatum_plt

include tools.mk