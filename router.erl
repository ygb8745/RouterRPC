-module(router).
-behaviour(gen_server).

% -export([start_link/0]).
% -export([alloc/0, free/1]).
% -export([init/1, handle_call/3, handle_cast/2]).
-compile(export_all).

-include("router_def.hrl").

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

handle_call({get_path_to_other, Node}, _From, State) ->
    Path = maps:find(Node, State#router_state.path_to_other),
    {reply, Path, State};
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
    update_router_info(State, RouterMap),% todo save info.
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
    % TODO : 按照时间戳来决定merge的结果. 是否有新的路由信息加入,有的话还要更新path信息
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
            {NewPathMap,NewQueue} =
                case maps:find(Item, RouterMap) of
                    {ok, #router_item{connected_list = NodeListConnToThis}} ->
                        lists:foldl(
                            fun(N,{AccPathMap,AccQueue})->
                                case maps:is_key(N, AccPathMap) of
                                    true->
                                        {AccPathMap,AccQueue}; % N 节点已经在PathMap中了
                                    false->
                                        {AccPathMap#{N => maps:get(Item, PathMap) ++ [N]}, queue:in(N, AccQueue)}
                                end
                            end, {PathMap,TQueue}, NodeListConnToThis);
                    error ->
                        % 没有该节点的直连节点列表信息,不能通过此节点向外扩展.
                        ?log({"Router Item not found: node", Item}),
                        {PathMap,TQueue}
                end,
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
%           --有一定的死锁风险,当两个节点同时call对方时就会产生死锁.
%           目前和路由先相关的主要有4个函数:
%               update_router()
%               handle_call({update_router_request, KnowenNodeList, Ref}, _From, State)
%               handle_cast({update_router_info,RouterMap}, State)
%               update_router_info(State, NewRouterMap)
%       2.todo 每个节点都会在随机时间后尝试向所有已知节点发出ping请求连接,并向周围直连节点发出自己的路由表,
%         周围节点收到路由表后开始更新自己的路由表.
%           为了实现这一点,我们要在每个路由表项上加上一个时间戳来判断要不要更新路由表.
%           ---------------
%           时间戳无需,在这个系统里我们遵循以下原则:
%               每个节点要么在自己节点直联表变化时发出路由信息,要么转发别人的变化信息,不会主动发出别人节点的路由信息.
%           每个节点启动一个进程,每隔一段时间后就尝试ping所有已知节点,
%           当本节点的nodes()表和State中的本节点路由项不同时就 1.更新自己的路由信息和path表 2.向其他节点广播自己的新变化
%           其他节点收到新路由项后做 1.更新自己的路由信息和path表 2.向未知节点广播路由变化 3.X尝试ping所有节点,检查自己是否有路由信息变化.
% todo
% 在每个节点上都启动router进程
% 改进路由算法.
