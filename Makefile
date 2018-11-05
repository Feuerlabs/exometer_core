REBAR3 ?= rebar3

.PHONY: all clean clean_plt compile test doc dialyzer xref ci

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

clean_plt:
	rm -f $(EXOMETER_PLT)

dialyzer: compile
	$(REBAR3) dialyzer
