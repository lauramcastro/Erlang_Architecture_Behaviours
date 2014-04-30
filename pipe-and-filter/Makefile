ERL = $(shell which erl)
ERLFLAGS= -pa $(CURDIR)/.eunit -pa $(CURDIR)/ebin -pa $(CURDIR)/deps/*/ebin

REBAR_DIR := /local/scratch/sw/rebar
REBAR := $(REBAR_DIR)/rebar

DEPS_PLT=$(CURDIR)/.deps_plt
DEPS=erts kernel stdlib

.PHONY: all compile doc clean test dialyzer typer shell distclean update-deps rebuild

all: deps compile dialyzer test

# =============================================================================
# Rules to build the system
# =============================================================================

deps: $(REBAR)
	@$(REBAR) get-deps
	@$(REBAR) compile

update-deps:
	@$(REBAR) update-deps
	@$(REBAR) compile

compile:
	@$(REBAR) skip_deps=true compile

doc:
	@$(REBAR) skip_deps=true doc

test: compile
	@$(REBAR) skip_deps=true eunit

check: xref dialyzer

xref: compile
	@$(REBAR) skip_deps=true xref

$(DEPS_PLT):
	@echo Building local plt at $(DEPS_PLT)
	@echo
	dialyzer --output_plt $(DEPS_PLT) --build_plt --apps $(DEPS)

dialyzer: $(DEPS_PLT)
	dialyzer --fullpath --plt $(DEPS_PLT) -Wrace_conditions -r ./ebin

typer:
	typer --plt $(DEPS_PLT) -r ./src

shell: deps compile
	- @$(REBAR) skip_deps=true eunit
	@$(ERL) $(ERLFLAGS)

clean:
	@$(REBAR) skip_deps=true clean

distclean: clean
	- rm -rf $(DEPS_PLT)
	- rm -rf $(CURDIR)/deps
	- rm -rf $(CURDIR)/doc
	- rm -rf $(CURDIR)/ebin

rebuild: distclean deps compile dialyzer test
