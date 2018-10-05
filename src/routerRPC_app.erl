%%%-------------------------------------------------------------------
%% @doc routerRPC public API
%% @end
%%%-------------------------------------------------------------------

-module(routerRPC_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    routerRPC_supervisor:start_link().

stop(_State)->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================