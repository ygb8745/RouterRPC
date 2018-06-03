-module(router).
-behaviour(gen_server).

% -export([start_link/0]).
% -export([alloc/0, free/1]).
% -export([init/1, handle_call/3, handle_cast/2]).
-compile(export_all).

-define(undef, undefined).

-record(?MODULE,{
    % all_known_nodes = sets:new(),
    reouter_items = #{} % 每条记录是: 节点名-> [该节点可达的节点列表]
}).

start() -> start_link().
start_link() ->
    gen_server:start_link({local, router},%本地注册为router ,也可以通过whereis()函数来获得pid.
                          router,%回调模块的名字
                          [],%给init函数的参数
                          []).%是参数的列表。具体的参数请查看 gen_server(3) 。

%     gen_server:call(router, alloc).
%     gen_server:cast(router, {free, Ch}).

init(_Args) ->
    {ok, #?MODULE{} }.%init返回 {ok, State} ，其中 State 是gen_server的内部状态。

stop() ->
    gen_server:cast(router, stop).

terminate(normal, _State) ->
    ok.

update_router()->
    gen_server:cast(router, update_router_start).

whow()->
    log({"Show route tab:",
        gen_server:call(router, get_all_router_items)}).

handle_call(get_all_router_items, _From, State) ->
    {reply, State#?MODULE.reouter_items, State};
handle_call({update_router_request, KnowenNodeList, Ref}, _From, State) ->
    log({handle_call, {update_router_request, KnowenNodeList, Ref}}),
    Reply =
        case get(Ref) of
            ?undef ->
                put(Ref, update_router_start),% todo 删除Ref
                % 检查与自己连接的节点是不是都在 KnowenNodeList中,如果有未知节点就向未知节点也发信.
                UnKnowenNodeList = sets:to_list(sets:subtract(sets:from_list(nodes()),
                                                 sets:from_list(KnowenNodeList))),
                log({"UnKnowenNodeList", UnKnowenNodeList}),
                UnKnowenNodeRestList=
                    case UnKnowenNodeList of
                        []-> %do nothing
                            [];
                        _ ->
                            {RestList, _todo_BadNodes} = rpc:multicall(UnKnowenNodeList, gen_server, call, [router,
                                                                            {update_router_request, KnowenNodeList ++ UnKnowenNodeList, Ref}]),
                            RestList
                    end,
                [{node(), nodes()} | UnKnowenNodeRestList];
            _ ->
            [] % ignore 已经处理过这个router请求了
        end,
    {reply, Reply, State};
handle_call(Request, _From, OldState) ->
    log({"router.unhandle_call:~p~n",[{Request, _From, OldState}]}),
    {reply, Request, OldState}. %{reply, Reply, State1}。Reply是需要回馈给客户端的答复，同时 State1 是gen_server的状态的新值。

handle_cast(update_router_start, State) ->
    spawn(fun()->
        Ref = erlang:make_ref(),
        % put(Ref, update_router_start),
        % 每个节点的路由条目是: {节点名, [该节点可达的节点列表]}
        % 每个节点返回的路由信息应该是 [本节点路由条目, 其他节点路由条目]
        % multicall包括本节点.
        % gen_server循环call的问题.
        % 拼写错误 gen_server  sen_server.
        {RestList, _todo_BadNodes} = rpc:multicall(gen_server, call, [router,
                                                                        {update_router_request,
                                                                        _KnowenNodeList = [node()|nodes()],
                                                                        Ref}]),
        % todo 处理{badrpc, Why}
        FlattenList = lists:flatten(RestList),
        gen_server:cast(router,{update_router_done, FlattenList})
    end),
    {noreply, State};
handle_cast({update_router_done, FlattenList}, State) ->
    OldRouterMap = State#?MODULE.reouter_items,
    NewRouterMap =
        lists:foldl(
                    fun({NodeName, ConnectiongList}, RouterMap)->
                        % 放在进程字典中的路由信息是  {router_item, 节点名} -> [该节点可达的节点列表]
                        log({"router item",[{NodeName}, "->", ConnectiongList]}),
                        % put({router_item, NodeName}, ConnectiongList)
                        RouterMap#{NodeName => ConnectiongList}
                    end,
                    OldRouterMap,
                    FlattenList),
    {noreply, State#?MODULE{reouter_items = NewRouterMap}};
handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast(_Request, OldState) ->
    log({"router.unhandle_call:~p~n",[{_Request, OldState}]}),
    {noreply, OldState}.

handle_info(_Info,State)-> {noreply, State}.
code_change(_OldVsn, State, _Extra)-> {ok, State}.

log(What)->
    io:format("Log who:~p-~p, ~nwhat:~p~n",[node(), self(), What]).

% todo,在每个节点上都启动router进程