# RouterRPC
Erlang RouterRPC

## Usage
在多个节点上以相同的cookie启动erlang节点:`erl -name name@hostName -setcookie 123`

在每个节点启动router进程:`router:start().`

更新路由表:`router:update_router().`

等待更新路由完成`router update done`

RPC:`router_rpc:call('otherName@otherHost',M,F,A).`

todo:<br>
