-module(router_rpc).
-compile(export_all).

-include("router_def.hrl").

call(Path,M,F,A) when is_list(Path)->
    [Target|PathrLeft] = lists:reverse(Path),
    ArgList = lists:foldr(
        fun(N,Acc) ->
            [N, rpc, call, Acc]
        end,
        [Target,M,F,A],
        PathrLeft
    ),
    erlang:apply(rpc, call, ArgList);
call(Node,M,F,A)->
    case gen_server:call(router, {get_path_to_other, Node}) of
        {ok, Path}->
            call(Path, M, F, A);
        error ->
            {error, node_path_not_found, Node}
    end.

cast(Path,M,F,A) when is_list(Path)->
    [Target|PathrLeft] = lists:reverse(Path),
    ArgList = lists:foldr(
        fun(N,Acc) ->
            [N, rpc, cast, Acc]
        end,
        [Target,M,F,A],
        PathrLeft
    ),
    erlang:apply(rpc, cast, ArgList);
cast(Node,M,F,A)->
    case gen_server:call(router, {get_path_to_other, Node}) of
        {ok, Path}->
            cast(Path, M, F, A);
        error ->
            {error, node_path_not_found, Node}
    end.

%% trace router.
tracert(Node)->
    case gen_server:call(router, {get_path_to_other, Node}) of
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
