-module(router).
-behaviour(gen_server).

-compile(export_all).

-include("router_def.hrl").

%% ============================================================================================
%% API Function
%% ============================================================================================
start() ->
    gen_server:start({local, ?MODULE},%本地注册为?MODULE ,也可以通过whereis()函数来获得pid.
                          ?MODULE,%回调模块的名字
                          [],%给init函数的参数
                          []).%是参数的列表。具体的参数请查看 gen_server(3) 。
start_link() ->
    gen_server:start_link({local, ?MODULE},%本地注册为?MODULE ,也可以通过whereis()函数来获得pid.
                          ?MODULE,%回调模块的名字
                          [],%给init函数的参数
                          []).%是参数的列表。具体的参数请查看 gen_server(3) 。

stop() ->
    gen_server:cast(?MODULE, stop).

update_router()->
    Ref = erlang:make_ref(),
    RouterMap = gen_server:call(?MODULE, {collect_router_request, [node()], Ref}),
    gen_server:cast(?MODULE, {update_router_info,RouterMap}).

set_log_level(Level)->
    gen_server:call(?MODULE,{update_config,#{?log_level => Level}}).

update_config()->
    Config = read_config(),
    gen_server:call(?MODULE, {update_config, Config}).

% -spec get_config(Key)-> {ok, Vlue} | error.
get_config(Key)->
    gen_server:call(?MODULE, {get_config, Key}).

show()->
    State = gen_server:call(?MODULE, get_state),
    {"This Node:",      node(),
     "Route tab:",      State#router_state.reouter_items,
     "Path to other:",  State#router_state.path_to_other,
     "help pid:",       State#router_state.help_pid,
     "Config:",         State#router_state.config}.

%% ============================================================================================
%% gen_server Function
%% ============================================================================================
init(_Args) ->
    % Log在本地输出.
    erlang:group_leader(whereis(init),self()),
    Pid = create_help_process(),
    Config = read_config(),
    {ok, #router_state{help_pid = Pid,
                       config = Config}}.%init返回 {ok, State} ，其中 State 是gen_server的内部状态。

terminate(normal, _State) ->
    ok.

handle_call(get_state, _From, State) ->
    % for debug
    {reply, State, State};

% -- for config
handle_call({update_config, NewConfig}, _From, #router_state{config = OldConfig} = State) ->
    {reply, ok, State#router_state{config = maps:merge(OldConfig, NewConfig)}};
handle_call(get_config, _From, #router_state{config = Config} = State) ->% ConfigMap
    {reply, Config, State};
handle_call({get_config, Key}, _From, #router_state{config = Config} = State) ->% {ok, Value} | error.
    Reply = maps:find(Key, Config),
    {reply, Reply, State};

% -- to get router info
handle_call(get_all_nodes, _From, State) ->
    AllNodesList = maps:keys(State#router_state.path_to_other),
    {reply, AllNodesList, State};
handle_call({get_path_to_other, Node}, _From, State) ->
    Path = maps:find(Node, State#router_state.path_to_other),
    {reply, Path, State};
handle_call(get_all_router_items, _From, State) ->
    {reply, State#router_state.reouter_items, State};

% -- to collect router info
handle_call({collect_router_request, KnowenNodeList, Ref}, _From, State) ->
    % KnowenNodeList :: [已知节点]
    ?log({collect_router_request, KnowenNodeList, Ref}),
    RouterMap =
        case get(Ref) of
            ?undef ->
                handle_ref(State, Ref),
                % 检查与自己连接的节点是不是都在 KnowenNodeList中,如果有未知节点就向未知节点也发信.
                SysUnKnowenNodeList = sets:to_list(sets:subtract(sets:from_list(nodes()),
                                                                 sets:from_list(KnowenNodeList))),
                ?log({"SysUnKnowenNodeList", SysUnKnowenNodeList}),
                {SysUnKnowenNodeRouterMapList, _BadNodes} = rpc:multicall(SysUnKnowenNodeList, gen_server, call,
                                [?MODULE, {collect_router_request, KnowenNodeList ++ SysUnKnowenNodeList, Ref}]),
                lists:foldl(
                    fun(RouterMap, Acc) when is_map(RouterMap)->
                            maps:merge(RouterMap, Acc);
                       (BadRouterMap, Acc) ->
                            ?log(9, {"BadRouterItem:",BadRouterMap}),
                            Acc
                    end,
                    #{node() => #router_item{connected_list = nodes(),
                                             timestamp = erlang:system_time(millisecond)} }, %本节点直连点.
                    SysUnKnowenNodeRouterMapList);
            _ ->
                #{} % ignore 已经处理过这个router请求了
        end,
    {reply, RouterMap, State};

handle_call(Request, _From, OldState) ->
    ?log(1, {"router.unhandle_call:",{Request, _From, OldState}}),
    {reply, Request, OldState}. %{reply, Reply, State1}。Reply是需要回馈给客户端的答复，同时 State1 是gen_server的状态的新值。

handle_cast({update_router_item, NewRouterMap, KnowenNodeList, Ref}, State) ->
    % KnowenNodeList :: [已知节点]
    ?log({update_router_item, NewRouterMap, KnowenNodeList, Ref}),
    NewState =
        case get(Ref) of
            ?undef ->
                handle_ref(State, Ref),
                % 检查与自己连接的节点是不是都在 KnowenNodeList中,如果有未知节点就向未知节点也发信.
                SysUnKnowenNodeList = sets:to_list(sets:subtract(sets:from_list(nodes()),
                                                                 sets:from_list(KnowenNodeList))),
                rpc:multicall(SysUnKnowenNodeList, gen_server, cast,
                                [router, {update_router_item, NewRouterMap, KnowenNodeList ++ SysUnKnowenNodeList, Ref}]),
                % 更新本地路由信息
                update_router_info(State, NewRouterMap);
            _ ->
                State % ignore 已经处理过这个请求
        end,
    {noreply, NewState};
handle_cast({update_router_info,NewRouterMap}, State) ->
    NewState = update_router_info(State, NewRouterMap),
    {noreply, NewState};

handle_cast({del_ref, Ref}, State) ->
    erase(Ref),
    {noreply, State};
handle_cast({log, {Level, What, Node, Pid, Module, Line, Time}}, State)->
    {ok, InternalLevel} = get_config_from_state(?log_level, State),
    if Level =< InternalLevel ->
            io:format("=> ~p~n"
                      "\t\t\t\t->log: N:~p P:~p M:~p L:~p T:~s~n",[What, Node, Pid, Module, Line, Time]);
        true -> void
    end,
    {noreply, State};

handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast(_Request, OldState) ->
    ?log(1, {"router.unhandle_cast:",{_Request, OldState}}),
    {noreply, OldState}.

handle_info(Info,State)->
    ?log(1, {"router.unhandle_info:",Info}),
    {noreply, State}.

code_change(_OldVsn, State, _Extra)->
    ?log(1, {"router.code_change",_OldVsn, State, _Extra}),
    {ok, State}.

%% ============================================================================================
%% Internal Function
%% ============================================================================================

create_help_process()->
    spawn_link(fun Fun()->
            SleepTime = case get_config(?time_to_update_router) of
                {ok, Value} -> Value;
                error -> 3000 % The first time.
            end,
            timer:sleep(SleepTime),
            ?log({"help proc",self()}),
            AllNodesList = gen_server:call(?MODULE, get_all_nodes),
            lists:foreach(fun(N)->
                net_adm:ping(N),
                % 尝试在各个节点启动router
                router_rpc:cast(N, ?MODULE, start, [])
            end, AllNodesList),
            NewRouterMap = #{node() => #router_item{connected_list = nodes(),
                                                    timestamp = erlang:system_time(millisecond)}},
            rpc:multicall(gen_server, cast,
                            [?MODULE, {update_router_item, NewRouterMap, [node()|nodes()], erlang:make_ref()}]),
            Fun()
        end).

handle_ref(State, Ref)->
    put(Ref, true),
    spawn(fun()->
        {ok, TimeToDelRef} = get_config_from_state(?time_to_del_ref, State),
        timer:sleep(TimeToDelRef),
        gen_server:cast(?MODULE,{del_ref, Ref})
    end).

update_router_info(State, NewRouterMap)-> % NewState
    OldRouterMap = State#router_state.reouter_items,
    NewRouterMap1 = maps:merge(OldRouterMap, NewRouterMap),% the value in Map1 is superseded by the value in Map2
    % 过滤掉太老的路由项.
    NewRouterMap2 = maps:filter(
        fun(Node, #router_item{timestamp = Timestamp})->
            {ok, LivePeriod} = get_config_from_state(?live_period_for_router_item, State),
            {ok, TimeToUpdateRouter} = get_config_from_state(?time_to_update_router, State),
            LegalTime = erlang:system_time(millisecond) - LivePeriod * TimeToUpdateRouter,
            Result = Timestamp > LegalTime,
            case Result of
                true -> do_nothing;
                false -> ?log(1,{"Node disconnectd", Node})
            end,
            Result
        end,
        NewRouterMap1),
    ?log({"update_router_info new:",NewRouterMap2}),
    PathMap = find_path_for_all(NewRouterMap2),
    State#router_state{reouter_items = NewRouterMap2,
                       path_to_other = PathMap}.

find_path_for_all(RouterMap)->%PathMap
    Nodes = [node()|nodes()],
    InitPathMap = lists:foldl(
        fun(N,Acc)->
            Acc#{N => [N]}
        end, #{}, Nodes),
    find_path_for_all_help(RouterMap, InitPathMap, queue:from_list(Nodes)).

find_path_for_all_help(RouterMap, PathMap, Queue)->%PathMap
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

% -spec read_config()-> ConfigMap
read_config()->
    UserConfigedMap =
        try
            % todo: read from home dir.
            {ok,[ConfigMap | _]} = file:consult("router.config"),
            ConfigMap
        catch
            T:P ->
                ?log(9, {"Error when read config",T,P}),
                #{}
        end,
    DefaultConfigMap =
        #{
            %删除ref的时间.
            ?time_to_del_ref=> 100*1000,
            % 更新路由信息的时间
            ?time_to_update_router => 2*1000,
            % 每条路由生存周期.
            ?live_period_for_router_item => 10,
            % 要显示log的最低等级
            % log优先级,数字越小优先级越高,最高为0.
            ?log_level => 10
        },
    maps:merge(DefaultConfigMap, UserConfigedMap).

% -spec get_config_from_state(Key)-> {ok, Vlue} | error.
get_config_from_state(Key, #router_state{config = Config})->
    maps:find(Key, Config).

% doc
% 路由算法:
%   这里面设计了两套路由逻辑:
%       1.某节点主动向全网发起请求,以获得全网的拓扑.
%           这个的过程是,本节点调用update_router()向本节点发出call({collect_router_request, KnowenNodeList, Ref} 开始本过程
%           向全网其他节点发出call({collect_router_request, KnowenNodeList, Ref}
%           全网的节点都回答收集的路由信息汇总到update_router()然后调用gen_server:cast(router, {update_router_info,RouterMap})更新本节点的信息.
%           --有一定的死锁风险,当两个节点同时call对方时就会产生死锁.
%           目前和此路由相关的主要有函数:
%               update_router()
%               handle_call({collect_router_request, KnowenNodeList, Ref}, _From, State)
%               handle_cast({update_router_info,RouterMap}, State)
%               *update_router_info(State, NewRouterMap)
%       2.每个节点都会在随机时间后尝试向所有已知节点发出ping请求连接,并向周围直连节点发出自己的路由表,
%         周围节点收到路由表后开始更新自己的路由表.
%           在这个系统里我们遵循以下原则:
%               每个节点要么在自己节点直联表变化时发出路由信息,要么转发别人的变化信息,不会主动发出别人节点的路由信息.
%           每个节点启动一个进程,每隔一段时间后就尝试ping所有已知节点,
%           当本节点的nodes()表和State中的本节点路由项不同时就 1.更新自己的路由信息和path表 2.向其他节点广播自己的新变化
%           其他节点收到新路由项后做 1.更新自己的路由信息和path表 2.向未知节点广播路由变化 3.X尝试ping所有节点,检查自己是否有路由信息变化.
%           目前和此路由相关的主要有函数:
%               create_help_process()
%               handle_cast({update_router_item, NewRouterMap, KnowenNodeList, Ref}, State)
%               *update_router_info(State, NewRouterMap)
%
% Log系统
%   带有优先级的log系统.
% 代码远程加载
% 配置文件

% todo
%   删除第一种路由逻辑.
%   io输出有冲突
%   trace
%   跨网段代理
%   配置文件map读取
%       从家目录读取.
%           os:type() -> {Osfamily, Osname}
%           os:getenv(VarName) -> Value | false     win:"USERPROFILE"  Lin:"HOME"
%   角色系统,全局注册名称.
%       node name conflict.
%   通信加密.

% 其他信息
%   获取各个节点OPT版本信息:    router_rpc:multicall(erlang, apply, [fun()-> {node(), erlang:system_info(system_version)} end,[]]).
%   load config to all other node:  router_rpc:multicall(gen_server, call, [router, {update_config, gen_server:call(router, get_config)}])
