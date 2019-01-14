routerRPC
=====

[![Build Status](https://travis-ci.org/ygb8745/RouterRPC.svg?branch=master)](https://travis-ci.org/ygb8745/RouterRPC)

Erlang RouterRPC: RPC for non-fully connected networks .

This project can discover the network topology in a non-fully connected network 
and perform RPC between nodes that are not directly connected.

## Build
-----

    $ rebar3 compile

## Usage
Start Node:`erl -name name@hostName -setcookie 123`

Start router:`application:start(routerRPC).`

RPC:`router_rpc:call('otherName@otherHost',M,F,A).`

## Config
Read config `router.config` from home director.

Default value:

    #{
        % Lowest Log level that will be dispaly in terminal
        % Highest level is 0.
        log_level => 10,
        
        % Role of this node.
        role => undefned,
        
        % Whether router will try to start this application in other node automaticly
        infect_others => true,
        
        % Whether router will try to connect all the nodes that he knows
        automatically_connect => true
    }.
