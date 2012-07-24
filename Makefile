.PHONY: all compile rel clean deep-clean

all: compile

compile: rebar
	./rebar get-deps compile

rel: compile
	./rebar generate -f

clean: rebar
	./rebar clean

deep-clean: clean
	./rebar delete-deps
	rm -rf ./rebar

rebar:
	[ -e ./rebar ] || wget -q https://github.com/basho/rebar/wiki/rebar
	chmod u+x rebar
