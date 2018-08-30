routerRPC
=====

Erlang RouterRPC: Nodes interconnection in non-fully connected networks.

## Build
-----

    $ rebar3 compile

## Usage
Start Node:`erl -name name@hostName -setcookie 123`

Start router:`router:start().`

RPC:`router_rpc:call('otherName@otherHost',M,F,A).`
