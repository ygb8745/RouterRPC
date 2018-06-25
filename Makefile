
CC = erlc
# CFLAGS =

all : beam

beam : router.beam router_rpc.beam router_help.beam

router.beam : router.erl router_def.hrl
	${CC} router.erl

router_rpc.beam : router_rpc.erl router_def.hrl
	${CC} router_rpc.erl

router_help.beam : router_help.erl router_def.hrl
	${CC} router_help.erl

clean:
	rm *.beam
