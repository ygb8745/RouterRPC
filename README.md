# RouterRPC
Erlang RouterRPC: Nodes interconnection in non-fully connected networks.

## Usage
Start Node:`erl -name name@hostName -setcookie 123`

Start router:`router:start().`

RPC:`router_rpc:call('otherName@otherHost',M,F,A).`
