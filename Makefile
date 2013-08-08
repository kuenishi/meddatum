.PHONY: all dist package.src package clean

all:
	./rebar get-deps
	## ./rebar update-deps
	./rebar compile

clean:
	./rebar clean

PKG_ID=hl7
PKG_VERSION=$(shell echo $(PKG_ID) | sed -e 's/^$(REPO)-//')

dist: package.src
	cp package/$(PKG_ID).tar.gz .

package: package.src
	mkdir -p package
	make -C package -f $(PKG_ID)/deps/node_package/Makefile
