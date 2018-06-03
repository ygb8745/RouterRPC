-module(ygb).
-compile(export_all).


test()->
    {test_ok, node()}.

path_rpc(Path,M,F,A)->
    [Target|PathrLeft] = lists:reverse(Path),
    ArgList = lists:foldr(
        fun(N,Acc) ->
            [N,rpc,call,Acc]
        end,
        [Target,M,F,A],
        PathrLeft
    ),
    erlang:apply(rpc, call, ArgList).

start()->
    register(router, spawn(fun()-> loop() end)).

loop()->
?MODULE,
ok.
