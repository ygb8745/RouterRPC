-module(router).
-behaviour(gen_server).

% -export([start_link/0]).
% -export([alloc/0, free/1]).
% -export([init/1, handle_call/3, handle_cast/2]).
-compile(export_all).

-define(undef, undefined).

start() -> start_link().
start_link() ->
    gen_server:start_link({local, router},%本地注册为router ,也可以通过whereis()函数来获得pid.
                          router,%回调模块的名字
                          [],%给init函数的参数
                          []).%是参数的列表。具体的参数请查看 gen_server(3) 。

%     gen_server:call(router, alloc).
%     gen_server:cast(router, {free, Ch}).

init(_Args) ->
    {ok, [node()|nodes()]}.%init返回 {ok, State} ，其中 State 是gen_server的内部状态。

stop() ->
    gen_server:cast(router, stop).

update_router()->
    gen_server:call(router, update_router_start).

handle_call(update_router_start, _From, State) ->
    Ref = erlang:make_ref(),
    % put(Ref, update_router_start),
    % 每个节点返回的路由信息应该是 {本节点名, [本节点可达的节点列表]}
    {RestList, _todo_BadNodes} = rpc:multicall(sen_server, call, [router,
                                                                    {update_router_request, _KnowenNodeList = [node()|nodes()], Ref}]),
    FlattenList = lists:flatten(RestList),
    lists:foreach(
        fun({NodeName, ConnectiongList})->
            % 放在进程字典中的路由信息是  {router_item, 节点名} -> [该节点可达的节点列表]
            put({router_item, NodeName}, ConnectiongList)
        end,
        FlattenList),
    {reply, ok, State};
handle_call({update_router_request, KnowenNodeList, Ref}, _From, OldState) ->
    case get(Ref) of
        ?undef ->
            put(Ref, update_router_start),
            % 检查与自己连接的节点是不是都在 KnowenNodeList中,如果有未知节点就向未知节点也发信.
            UnKnowenNodeList = sets:subtract(sets:from_list(nodes()),
                          sets:from_list(KnowenNodeList)),
            UnKnowenNodeRestList=
            case UnKnowenNodeList of
                []-> %do nothing
                    [];
                _ ->
                    {UnKnowenNodeRestList, _todo_BadNodes} = rpc:multicall(sen_server, call, [router,
                                                                    {update_router_request, UnKnowenNodeList, Ref}]),
                    UnKnowenNodeRestList
            end,
            [{node(), nodes()}|UnKnowenNodeRestList];
        _ -> ignore %??
    end;
handle_call(Request, _From, OldState) ->
    io:format("router.unhandle_call:~p~n",[{Request, _From, OldState}]),
    {reply, Request, OldState}. %{reply, Reply, State1}。Reply是需要回馈给客户端的答复，同时 State1 是gen_server的状态的新值。

handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast(_Request, OldState) ->
    io:format("router.unhandle_call:~p~n",[{_Request, OldState}]),
    {noreply, OldState}.

terminate(normal, _State) ->
    ok.
