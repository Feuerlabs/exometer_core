REBAR3=$(shell which rebar3 || echo ./rebar3)

.PHONY: all clean clean_plt compile test doc dialyzer xref ci

EXOMETER_PLT=exometer_core.plt
DIALYZER_OPTS = # -Wunderspecs
DIALYZER_APPS = erts kernel stdlib compiler syntax_tools \
		test_server common_test folsom \
		parse_trans setup

all: compile xref test

ci: compile xref dialyzer test

compile:
	$(REBAR3) compile

clean: clean_plt
	$(REBAR3) clean

clean-all: clean
	rm -rf _build

test:
	$(REBAR3) as test do eunit
	$(REBAR3) as test do ct

xref:
	$(REBAR3) xref

doc:
	$(REBAR3) as docs do edoc

$(EXOMETER_PLT):
	$(REBAR3) compile
	ERL_LIBS=deps dialyzer --build_plt --output_plt $(EXOMETER_PLT) \
	--apps $(DIALYZER_APPS) | \
	fgrep -v -f ./dialyzer.ignore-warnings

clean_plt:
	rm -f $(EXOMETER_PLT)

dialyzer: compile $(EXOMETER_PLT)
	dialyzer -r ebin --plt $(EXOMETER_PLT) $(DIALYZER_OPTS) | \
	fgrep -v -f ./dialyzer.ignore-warnings
