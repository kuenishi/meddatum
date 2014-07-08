.PHONY: all clean test compile

all:
	./rebar get-deps
	./rebar compile
	./rebar escriptize skip_deps=true
## ./rebar update-deps

compile:
	./rebar compile

test: compile
	./rebar eunit skip_deps=true

clean:
	./rebar clean
