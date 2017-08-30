ERLC=/usr/bin/erlc
ERLCFLAGS=-o
SRCDIR=src
BEAMDIR=./ebin

all: 
	@ mkdir -p $(BEAMDIR);
	@ $(ERLC) $(ERLCFLAGS) $(BEAMDIR) $(SRCDIR)/*.erl ;
clean: 
	@ rm -rf $(BEAMDIR) ;
	@ rm -rf erl_crush.dump
test:
	@erl -pa $(BEAMDIR) -s chess_test run_test
boardtest:
	@erl -pa $(BEAMDIR) -s board test
