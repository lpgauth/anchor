PROJECT=anchor
REBAR=./rebar

.phony: deps

all: deps compile

build-plt: all
	@dialyzer --build_plt --output_plt ~/.$(PROJECT).plt \
		--apps erts kernel stdlib crypto public_key ssl

check-plt:
	@dialyzer --check_plt --plt ~/.$(PROJECT).plt

clean:
	@echo "Running rebar clean..."
	@$(REBAR) clean
	@rm -rf deps ebin

compile:
	@echo "Running rebar compile..."
	@$(REBAR) compile

deps:
	@echo "Running rebar update-deps..."
	@$(REBAR) update-deps

dialyze:
	@dialyzer ebin/*.beam --plt ~/.$(PROJECT).plt -I include
