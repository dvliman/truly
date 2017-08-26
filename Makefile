REBAR3=./rebar3

all: compile shell

compile:
	${REBAR3} compile

shell:
	${REBAR3} shell --config config/app.config --apps truly

clean:
	rm -rf _build
