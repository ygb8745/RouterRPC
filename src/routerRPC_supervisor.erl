-module(routerRPC_supervisor).
-behaviour(supervisor).

%% Application callbacks
-export([start_link/0, init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init(_Args) ->
    {ok,
        {#{strategy =>one_for_one,
            intensity=>1,
            period => 1},
            [
                #{id => routerRPCid,
                    start => {router, start_link, []},
                    restart => permanent,
                    shutdown => infinity,
                    type => worker,
                    modules => [router]}
            ]
        }
    }.
