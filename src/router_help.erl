-module(router_help).
-compile(export_all).

-include("router_def.hrl").

% 将本地的代码加载到所有其他节点上.
% -spec nl(Module)-> [{Node, Result}]
nl(Module)->
    %l(Module)
    code:purge(Module),
    code:load_file(Module),
    {Module, Binary, Filename} = code:get_object_code(Module),
    AllNodes = router:all_nodes(),
    ResultList = router_rpc:multicall(AllNodes, code, load_binary, [Module, Filename, Binary]),
    lists:zip(AllNodes, ResultList).

%   获取各个节点OPT版本信息:
list_otp()->
    lists:foreach(
        fun(N)->
            io:format("~s\t ~s~n",[N, router_rpc:call(N, erlang, system_info, [system_version])])
        end,
        router:all_nodes()).

list_role()->
    lists:foreach(
        fun(N)->
            io:format("~s\t ~p~n",[N, router_rpc:call(N, router, get_role, [])])
        end,
        router:all_nodes()).

% 其他
%   load config to all other node:
%           router_rpc:multicall(gen_server, call, [router, {update_config, gen_server:call(router, get_config)}]).
