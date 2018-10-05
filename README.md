routerRPC
=====

Erlang RouterRPC: Nodes interconnection in non-fully connected networks.

## Build
-----

    $ rebar3 compile

## Usage
Start Node:`erl -name name@hostName -setcookie 123`

Start router:`application:start(routerRPC).`

RPC:`router_rpc:call('otherName@otherHost',M,F,A).`
