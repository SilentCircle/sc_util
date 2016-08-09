.PHONY: compile ct check dialyze clean realclean distclean doc

# This Makefile works with rebar3 and the profiles that rebar3 supports. This
# makefile will run with the 'default' profile unless REBAR_PROFILE is
# provided, e.g. in bash,
#
# make rel REBAR_PROFILE=prod
#
REBAR_PROFILE ?= default
THIS_MAKEFILE := $(lastword $(MAKEFILE_LIST))

$(info $(THIS_MAKEFILE) is using REBAR_PROFILE=$(REBAR_PROFILE))

REBAR3_URL = https://s3.amazonaws.com/rebar3/rebar3

# If there is a rebar in the current directory, use it
ifeq ($(wildcard rebar3),rebar3)
REBAR = $(CURDIR)/rebar3
endif

# Fallback to rebar on PATH
REBAR ?= $(shell which rebar3)

# And finally, prep to download rebar if all else fails
ifeq ($(REBAR),)
REBAR = $(CURDIR)/rebar3
endif

ERLANG_VER=$(shell erl -noinput -eval 'io:format("~s",[erlang:system_info(otp_release)]), halt().')

all: info compile check

info: $(REBAR)
	@echo 'Erlang version building this is: $(ERLANG_VER)'
	@echo 'REBAR: $(REBAR) --version'

compile: $(REBAR)
	$(REBAR) do clean, compile

ct: $(REBAR)
	$(REBAR) ct

check:	dialyze ct

dialyze: $(REBAR)
	$(REBAR) dialyzer

clean: $(REBAR)
	$(REBAR) clean
	@rm -rf priv ebin

distclean: clean
	@rm -rf logs .test
	@rm -f doc/*.html doc/edoc-info

doc: $(REBAR)
	$(REBAR) edoc

$(REBAR):
	curl -s -Lo rebar3 $(REBAR3_URL) || wget $(REBAR3_URL)
	chmod a+x $(REBAR)

# ex: ts=4 sts=4 sw=4 noet
