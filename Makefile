PROJECT=anchor
REBAR=./rebar

all: compile

build-plt: all
	@dialyzer --build_plt --output_plt ~/.$(PROJECT).plt \
		--apps erts kernel stdlib crypto public_key ssl

check-plt:
	@dialyzer --check_plt --plt ~/.$(PROJECT).plt

clean:
	@echo "Running rebar clean..."
	@$(REBAR) clean
	@rm -rf ebin

compile:
	@echo "Running rebar compile..."
	@$(REBAR) compile

dialyze:
	@dialyzer ebin/*.beam --plt ~/.$(PROJECT).plt -I include

eunit:
	@echo "Running EUnit suite..."
	@$(REBAR) skip_deps=true eunit

test: all eunit

.PHONY: test
