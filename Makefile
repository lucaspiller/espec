all: generate

deps:
	./rebar get-deps

compile: deps
	./rebar compile

generate: compile
	./rebar escriptize
	mv espec bin/espec

dev: compile
	erl -pa deps/*/ebin -pa ebin -s inets start -s reloader start

spec: generate
	./bin/espec spec

clean:
	./rebar clean
	rm -Rf .eunit
	rm -Rf bin/espec
