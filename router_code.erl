-module(router_code).
-compile(export_all).

-include("router_def.hrl").

% 将本地的代码加载到所有其他节点上.
% -spec nl(Module)-> [{Node, Result}]
nl(Module)->
    {Module, Binary, Filename} = code:get_object_code(Module),
    AllNodes = gen_server:call(router, get_all_nodes),
    ResultList = router_rpc:multicall(AllNodes, code, load_binary, [Module, Filename, Binary]),
    lists:zip(AllNodes, ResultList).
