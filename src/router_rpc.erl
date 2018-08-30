-module(router_rpc).
-compile(export_all).

-include("router_def.hrl").

call(NodeOrPath,M,F,A)->
    commonRPC(call, NodeOrPath,M,F,A).

multicall(M,F,A)->
    AllNodes = router:all_nodes(),
    multicall(AllNodes,M,F,A).
multicall(Nodes,M,F,A)->
    [call(N,M,F,A) || N <- Nodes].

role_call(Role,M,F,A)->
    Nodes = router:get_nodes_of_role(Role),
    [call(N,M,F,A) || N <- Nodes].

cast(NodeOrPath,M,F,A)->
    commonRPC(cast, NodeOrPath,M,F,A).

%% trace router.
tracert(Node)->
    case router:get_path_to(Node) of
        {ok, Path}->
            {_, Result} = lists:foldl(
                fun(N,{PathAcc, ResAcc})->
                    NewPath = PathAcc ++ [N],
                    Result = call(NewPath,?MODULE,echo,[trace_ok]),
                    {NewPath, ResAcc ++ [{N,Result}]}
                end,
                {[],[]},
                Path),
            Result;
        error ->
            {error, node_path_not_found, Node}
    end.

echo(Info)->
    Info.

%% ============================================================================================
%% Internal Function
%% ============================================================================================

% Method :: call | cast
commonRPC(Method, Path,M,F,A) when is_list(Path)->
    [Target|PathrLeft] = lists:reverse(Path),
    ArgList = lists:foldr(
        fun(N,Acc) ->
            [N, rpc, Method, Acc]
        end,
        [Target,M,F,A],
        PathrLeft),
    erlang:apply(rpc, Method, ArgList);
commonRPC(Method, Node,M,F,A)->
    case router:get_path_to(Node) of
        {ok, Path}->
            commonRPC(Method, Path, M, F, A);
        error ->
            {error, node_path_not_found, Node}
    end.
