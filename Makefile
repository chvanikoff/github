REBAR = `which rebar || ./rebar`

all: deps compile

compile:
	@($(REBAR) compile)

deps:
	@($(REBAR) get-deps)

run:
	erl -pa ebin deps/*/ebin -s github

test:
	@($(REBAR) eunit)

clean:
	@($(REBAR) clean)

.PHONY: all deps test