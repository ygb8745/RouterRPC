-module(router).
-behaviour(gen_server).

% -export([start_link/0]).
% -export([alloc/0, free/1]).
% -export([init/1, handle_call/3, handle_cast/2]).
-compile(export_all).

-define(DEBUG, true).
-ifdef(DEBUG).
-define(log(What),
            io:format(
                "=> ~p~n"
                "\t\t\t\t from: N:~p P:~p M:~p L:~p T:~s~n",[What, node(), self(), ?MODULE,?LINE,format_timestamp()])).
format_timestamp() ->
    {_,_,Micro} = os:timestamp(),
    {{_Year,_Month,_Day},{Hour,Minute,Second}} =
        calendar:local_time(),
     io_lib:format("~p:~p:~p.~p",
                    [Hour,Minute,Second,Micro]).
-else.
-define(log(What), void).
-endif.

-define(undef, undefined).

-record(?MODULE,{
    % all_known_nodes = sets:new(),
    reouter_items = #{}, % 每条记录是: 节点名-> [该节点可达的节点列表]
    path_to_other = #{}  % 每条记录是到达其他节点的路径: 节点名->[到达该节点的路径列表]
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

rpc_call(Path,M,F,A) when is_list(Path)->
    [Target|PathrLeft] = lists:reverse(Path),
    ArgList = lists:foldr(
        fun(N,Acc) ->
            [N,rpc,call,Acc]
        end,
        [Target,M,F,A],
        PathrLeft
    ),
    erlang:apply(rpc, call, ArgList);
rpc_call(Node,M,F,A)->
    case maps:find(Node, gen_server:call(router, get_path_to_other)) of
        {ok, Path}->
            rpc_call(Path, M, F, A);
        error ->
            {error, node_not_found, Node}
    end.

show()->
    ?log({"This Node:",node(),
        "Route tab:", gen_server:call(router, get_all_router_items),
        "Path to other:", gen_server:call(router, get_path_to_other)}).

handle_call(get_path_to_other, _From, State) ->
    {reply, State#?MODULE.path_to_other, State};
handle_call(get_all_router_items, _From, State) ->
    {reply, State#?MODULE.reouter_items, State};
handle_call({update_router_request, KnowenNodeList, Ref}, _From, State) ->
    ?log({handle_call, {update_router_request, KnowenNodeList, Ref}}),
    Reply =
        case get(Ref) of
            ?undef ->
                put(Ref, update_router_start),
                spawn(fun()->
                        timer:sleep(100*1000),%100s
                        gen_server:cast(router,{del_ref, Ref})
                      end),
                % 检查与自己连接的节点是不是都在 KnowenNodeList中,如果有未知节点就向未知节点也发信.
                UnKnowenNodeList = sets:to_list(sets:subtract(sets:from_list(nodes()),
                                                              sets:from_list(KnowenNodeList))),
                ?log({"UnKnowenNodeList", UnKnowenNodeList}),
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
    ?log({"router.unhandle_call:~p~n",[{Request, _From, OldState}]}),
    {reply, Request, OldState}. %{reply, Reply, State1}。Reply是需要回馈给客户端的答复，同时 State1 是gen_server的状态的新值。

handle_cast(update_router_start, State) ->
    spawn(fun()->
        Ref = erlang:make_ref(),
        % 每个节点的路由条目是: {节点名, [该节点可达的节点列表]}
        % 每个节点返回的路由信息应该是 [本节点路由条目, 其他节点路由条目]
        {RestList, _todo_BadNodes} = rpc:multicall(gen_server, call, [router,
                                                                        {update_router_request,
                                                                        _KnowenNodeList = [node()|nodes()],
                                                                        Ref}]),
        FlattenList = lists:flatten(RestList),
        RouterList = lists:foldl(
            fun(I, Acc)->
                case I of
                    {badrpc, Why} ->
                        ?log({badrpc, Why});
                    GoodItem ->
                        [GoodItem | Acc]
                end
            end,
            [],
            FlattenList
        ),
        gen_server:cast(router,{update_router_done, RouterList})
    end),
    {noreply, State};
handle_cast({update_router_done, RouterList}, State) ->
    OldRouterMap = State#?MODULE.reouter_items,
    NewRouterMap =
        lists:foldl(
                    fun({NodeName, ConnectiongList}, RouterMap)->
                        % 路由信息是  节点名 => [该节点可达的节点列表]
                        ?log({"router item:",[NodeName, "->", ConnectiongList]}),
                        RouterMap#{NodeName => ConnectiongList}
                    end,
                    OldRouterMap,
                    RouterList),
    PathMap = find_path_for_all(NewRouterMap),
    ?log("router update done"),
    {noreply, State#?MODULE{
        reouter_items = NewRouterMap,
        path_to_other = PathMap}};
handle_cast({del_ref, Ref}, State) ->
    erase(Ref),
    {noreply, State};
handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast(_Request, OldState) ->
    ?log({"router.unhandle_call:~p~n",[{_Request, OldState}]}),
    {noreply, OldState}.

handle_info(_Info,State)-> {noreply, State}.
code_change(_OldVsn, State, _Extra)-> {ok, State}.

find_path_for_all(RouterMap)->
    Nodes = nodes(),
    PathMap = lists:foldl(
        fun(N,Acc)->
            Acc#{N => [N]}
        end,
        #{},
        Nodes),
    find_path_for_all_help(RouterMap, PathMap#{node() => [node()]}, queue:from_list(Nodes)).

find_path_for_all_help(RouterMap, PathMap, Queue)->
    case queue:out(Queue) of
        {{value, Item}, NewQueue} ->
            NodesConnToThis = maps:get(Item, RouterMap),
            NewPathMap =
                lists:foldl(
                    fun(N,Acc)->
                        case maps:is_key(N, Acc) of
                            true-> Acc;
                            false-> PathMap#{N => maps:get(Item, PathMap) ++ [N]}
                        end
                    end,
                    PathMap,
                    NodesConnToThis),
            find_path_for_all_help(RouterMap, NewPathMap, NewQueue);
        {empty, _} ->
            PathMap
    end.

% todo
% 在每个节点上都启动router进程
% 改进路由算法.
