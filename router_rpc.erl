-module(router_rpc).
-compile(export_all).

-include("router_def.hrl").

call(Path,M,F,A) when is_list(Path)->
    [Target|PathrLeft] = lists:reverse(Path),
    ArgList = lists:foldr(
        fun(N,Acc) ->
            [N,rpc,call,Acc]
        end,
        [Target,M,F,A],
        PathrLeft
    ),
    erlang:apply(rpc, call, ArgList);
call(Node,M,F,A)->
    case maps:find(Node, gen_server:call(router, get_path_to_other)) of
        {ok, Path}->
            call(Path, M, F, A);
        error ->
            {error, node_path_not_found, Node}
    end.
