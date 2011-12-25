all: compile

deps:
	./rebar get-deps

compile: deps
	./rebar compile

dev: compile
	erl -pa deps/*/ebin -pa ebin -s inets start -s reloader start

spec: compile
	./bin/espec spec

clean:
	./rebar clean
	rm -Rf .eunit
