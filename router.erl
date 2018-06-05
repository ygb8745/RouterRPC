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

-record(router_state,{
    % all_known_nodes = sets:new(),
    reouter_items = #{}, % 每条记录是: 节点名-> [该节点可达的节点列表]
    path_to_other = #{}  % 每条记录是到达其他节点的路径: 节点名->[到达该节点的路径列表]
}).

% 用法 用于作为router_map的一项: node() => #router_item{}
% -type router_map() = #{node_name() => #router_item{}}.
% -type path_map() =   #{node_name() => [_Path_to_this_node = node_name()]}.
% -type node_name() = atom().
-record(router_item,{
    connected_list, % [直连节点列表]
    timestamp % 最后更新该项的时间戳
}).

%% ============================================================================================
%% API Function
%% ============================================================================================
%     gen_server:call(router, alloc).
%     gen_server:cast(router, {free, Ch}).
start() -> start_link().
start_link() ->
    gen_server:start_link({local, router},%本地注册为router ,也可以通过whereis()函数来获得pid.
                          router,%回调模块的名字
                          [],%给init函数的参数
                          []).%是参数的列表。具体的参数请查看 gen_server(3) 。

stop() ->
    gen_server:cast(router, stop).

update_router()->
    Ref = erlang:make_ref(),
    RouterMap = gen_server:call(router, {update_router_request, [node()], Ref}),
    gen_server:cast(router, {update_router_info,RouterMap}).

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
            {error, node_path_not_found, Node}
    end.

show()->
    ?log({"This Node:",node(),
        "Route tab:", gen_server:call(router, get_all_router_items),
        "Path to other:", gen_server:call(router, get_path_to_other)}).

%% ============================================================================================
%% gen_server Function
%% ============================================================================================
init(_Args) ->
    {ok, #router_state{} }.%init返回 {ok, State} ，其中 State 是gen_server的内部状态。

terminate(normal, _State) ->
    ok.

handle_call(get_path_to_other, _From, State) ->
    {reply, State#router_state.path_to_other, State};
handle_call(get_all_router_items, _From, State) ->
    {reply, State#router_state.reouter_items, State};
handle_call({update_router_request, KnowenNodeList, Ref}, _From, State) ->
    % KnowenNodeList :: [已知节点]
    ?log({update_router_request, KnowenNodeList, Ref}),
    RouterMap =
        case get(Ref) of
            ?undef ->
                put(Ref, update_router_start),
                spawn(fun()->
                        timer:sleep(100*1000),%100s
                        gen_server:cast(router,{del_ref, Ref})
                      end),
                % 检查与自己连接的节点是不是都在 KnowenNodeList中,如果有未知节点就向未知节点也发信.
                SysUnKnowenNodeList = sets:to_list(sets:subtract(sets:from_list(nodes()),
                                                                 sets:from_list(KnowenNodeList))),
                ?log({"SysUnKnowenNodeList", SysUnKnowenNodeList}),
                SysUnKnowenNodeRouterMapList=
                    case SysUnKnowenNodeList of
                        []-> %do nothing
                            [];
                        _ ->
                            {RouterMapList, _todo_BadNodes} = rpc:multicall(SysUnKnowenNodeList, gen_server, call,
                                            [router, {update_router_request, KnowenNodeList ++ SysUnKnowenNodeList, Ref}]),
                            RouterMapList
                    end,
                lists:foldl(
                    fun(Item, Acc)->
                        case Item of
                            RouterMap when is_map(RouterMap) ->
                                maps:merge(RouterMap, Acc);
                            BadItem ->
                                ?log({"BadRouterItem:",BadItem}),
                                Acc
                        end
                    end,
                    #{node() => #router_item{connected_list = nodes(),
                                             timestamp = os:timestamp()} }, %本节点直连点. todo:时间戳改为整数形式
                    SysUnKnowenNodeRouterMapList);
            _ ->
                #{} % ignore 已经处理过这个router请求了
        end,
    {reply, RouterMap, State};
handle_call(Request, _From, OldState) ->
    ?log({"router.unhandle_call:~p~n",[{Request, _From, OldState}]}),
    {reply, Request, OldState}. %{reply, Reply, State1}。Reply是需要回馈给客户端的答复，同时 State1 是gen_server的状态的新值。

handle_cast({update_router_info,RouterMap}, State) ->
    NewState = update_router_info(State, RouterMap),
    {noreply, NewState};
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

%% ============================================================================================
%% Internal Function
%% ============================================================================================

update_router_info(State, NewRouterMap)-> % NewState
    OldRouterMap = State#router_state.reouter_items,
    % TODO : 按照时间戳来决定merge的结果.
    NewRouterMap = maps:merge(NewRouterMap, OldRouterMap),
    ?log({"update_router_info new:",NewRouterMap}),
    PathMap = find_path_for_all(NewRouterMap),
    State#router_state{reouter_items = NewRouterMap,
                  path_to_other = PathMap}.

find_path_for_all(RouterMap)->
    Nodes = [node()|nodes()],
    InitPathMap = lists:foldl(
        fun(N,Acc)->
            Acc#{N => [N]}
        end, #{}, Nodes),
    find_path_for_all_help(RouterMap, InitPathMap, queue:from_list(Nodes)).

find_path_for_all_help(RouterMap, PathMap, Queue)->
    case queue:out(Queue) of
        {{value, Item}, TQueue} ->
            #router_item{connected_list = NodesConnToThis} = maps:get(Item, RouterMap),
            {NewPathMap,NewQueue} =
                lists:foldl(
                    fun(N,{AccPathMap,AccQueue})->
                        case maps:is_key(N, AccPathMap) of
                            true-> {AccPathMap,AccQueue}; % N 节点已经在PathMap中了
                            false->
                                {AccPathMap#{N => maps:get(Item, PathMap) ++ [N]},
                                 queue:in(N, AccQueue)}
                        end
                    end, {PathMap,TQueue}, NodesConnToThis),
            find_path_for_all_help(RouterMap, NewPathMap, NewQueue);
        {empty, _} ->
            PathMap
    end.

% doc
% 路由算法:
%   这里面设计了两套路由逻辑:
%       1.某节点主动向全网发起请求,以获得全网的拓扑.
%           这个的过程是,本节点调用update_router()向本节点发出call({update_router_request, KnowenNodeList, Ref} 开始本过程
%           向全网其他节点发出call({update_router_request, KnowenNodeList, Ref}
%           全网的节点都回答收集的路由信息汇总到update_router()然后调用gen_server:cast(router, {update_router_info,RouterMap})更新本节点的信息.
%       2.todo 每个节点都会在随机时间后尝试向所有已知节点发出ping请求连接,并向周围直连节点发出自己的路由表,
%         周围节点收到路由表后开始更新自己的路由表.
%           为了实现这一点,我们要在每个路由表项上加上一个时间戳来判断要不要更新路由表.
% todo
% 在每个节点上都启动router进程
% 改进路由算法.
