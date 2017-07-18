REBAR3=./rebar3

##
## define the default release path in order to
## manage the core system. The release must be generated
## before try to start using `make rel`.
## TODO: Check if after creation release could be
## moved to another dir for easy access.
##
RELEASE_PATH=_build/default/rel/kaa/bin

##
## define the main script that controls 
## the management of the release.
##
KAA=kaa

.PHONY: jun compile all doc test

all: compile

compile:
	$(REBAR3) compile

clean:
	$(REBAR3) clean

rel:
	$(REBAR3) release

run:
	@sh $(RELEASE_PATH)/$(KAA) start

live:
	@sh $(RELEASE_PATH)/$(KAA) console

stop:
	@sh $(RELEASE_PATH)/$(KAA) stop
