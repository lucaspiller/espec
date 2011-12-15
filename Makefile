all: compile

deps:
	./rebar get-deps

compile: deps
	./rebar compile

dev: compile
	erl -pa deps/*/ebin -pa ebin -s inets start -s reloader start

test: compile
	erl -pa deps/*/ebin -pa ebin -noshell -s espec run_no_shell espec_espec

clean:
	./rebar clean
	rm -Rf .eunit
