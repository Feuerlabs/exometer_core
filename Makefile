.PHONY: all clean clean_plt deps compile test doc dialyzer xref ci

EXOMETER_PLT=exometer_core.plt
DIALYZER_OPTS = # -Wunderspecs
DIALYZER_APPS = erts kernel stdlib compiler syntax_tools \
		test_server common_test folsom \
		parse_trans setup

all: deps compile xref test

ci: deps compile xref dialyzer test

deps:
	rebar get-deps

compile:
	rebar compile

clean: clean_plt
	rebar clean

clean-all: clean
	rm -rf deps

test:
	ERL_LIBS=./examples rebar ct skip_deps=true

xref:
	ERL_LIBS=./deps rebar xref skip_deps=true

doc:
	mkdocs build

$(EXOMETER_PLT):
	rebar get-deps compile
	ERL_LIBS=deps dialyzer --build_plt --output_plt $(EXOMETER_PLT) \
	--apps $(DIALYZER_APPS) | \
	fgrep -v -f ./dialyzer.ignore-warnings

clean_plt:
	rm -f $(EXOMETER_PLT)

dialyzer: deps compile $(EXOMETER_PLT)
	dialyzer -r ebin --plt $(EXOMETER_PLT) $(DIALYZER_OPTS) | \
	fgrep -v -f ./dialyzer.ignore-warnings
