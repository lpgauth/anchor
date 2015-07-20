PROJECT=anchor
REBAR=./rebar3

all: compile

clean:
	@echo "Running rebar3 clean..."
	@$(REBAR) clean

compile:
	@echo "Running rebar3 compile..."
	@$(REBAR) compile

dialyzer:
	@echo "Running rebar3 dialyze..."
	@$(REBAR) dialyzer

edoc:
	@echo "Running rebar3 edoc..."
	@$(REBAR) as doc edoc

eunit:
	@echo "Running rebar3 eunit..."
	@rm -rf _build/test/lib
	@$(REBAR) do eunit, cover --verbose

test: dialyzer eunit xref

xref:
	@echo "Running rebar3 xref..."
	@$(REBAR) xref

.PHONY: deps doc test xref
