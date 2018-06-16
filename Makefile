
all : beam

beam : router.beam router_rpc.beam router_code.beam

router.beam : router.erl router_def.hrl
	erlc router.erl

router_rpc.beam : router_rpc.erl router_def.hrl
	erlc router_rpc.erl

router_code.beam : router_code.erl router_def.hrl
	erlc router_code.erl

clean:
	rm *.beam
