.PHONY: all clean test compile

all:
	./rebar get-deps
## ./rebar update-deps

compile:
	./rebar compile

test: compile
	./rebar eunit skip_deps=true

clean:
	./rebar clean
