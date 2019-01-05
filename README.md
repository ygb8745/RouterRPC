routerRPC
=====

[![Build Status](https://travis-ci.org/ygb8745/RouterRPC.svg?branch=master)](https://travis-ci.org/ygb8745/RouterRPC)

Erlang RouterRPC: RPC for non-fully connected networks .

        A -- B
        |    |
        C -- D
        |  \
        E    F

## Build
-----

    $ rebar3 compile

## Usage
Start Node:`erl -name name@hostName -setcookie 123`

Start router:`application:start(routerRPC).`

RPC:`router_rpc:call('otherName@otherHost',M,F,A).`
