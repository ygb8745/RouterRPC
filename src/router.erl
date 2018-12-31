-module(router).
-behaviour(gen_server).

-compile(export_all).

-include("router_def.hrl").

-define(router_items, router_items).
-define(path_to_other, path_to_other).
-define(help_pid, help_pid).
-define(config, config).

-define(init_router_state,
    #{
        ?router_items => #{}, % #{node_name() => #router_item{}}
        % 每条记录是到达其他节点的路径: 节点名->[到达该节点的路径列表]
        % 这个map下所有作为key的node()就是全部的已知节点.
        ?path_to_other => #{},
        ?help_pid => ?undef,            % help process
        ?config => #{}               % map #{}
    }).

%% ============================================================================================
%% API Function
%% ============================================================================================
start_link() ->
    error_logger:info_msg("Starting router...\n"),
    gen_server:start_link({local, ?MODULE},%本地注册为?MODULE ,也可以通过whereis()函数来获得pid.
                          ?MODULE,%回调模块的名字
                          [],%给init函数的参数
                          []).%是参数的列表。具体的参数请查看 gen_server(3) 。

stop() ->
    gen_server:cast(?MODULE, stop).

%%update_router()->
%%    Ref = erlang:make_ref(),
%%    RouterMap = gen_server:call(?MODULE, {collect_router_request, [node()], Ref}),
%%    gen_server:cast(?MODULE, {update_router_info,RouterMap}).

% 包括本节点
all_nodes()->
    gen_server:call(?MODULE, get_all_nodes).

% spec get_path_to(Node)-> {ok, Path} | error.
get_path_to(Node)->
    gen_server:call(?MODULE, {get_path_to_other, Node}).

set_log_level(Level)->
    gen_server:call(?MODULE,{update_config,#{?log_level => Level}}).

set_role(Role)->
    gen_server:call(?MODULE,{update_config, #{?role => Role}}).

% -spec get_role()-> Role.
get_role()->
    {ok, Role} = get_config(?role),
    Role.

get_nodes_of_role(Role)->
    AllNodes = all_nodes(),
    lists:filter(
        fun(Node)->
            router_rpc:call(Node, ?MODULE, get_role, []) == Role
        end,
        AllNodes).

update_config(Key, Value)->
    gen_server:call(?MODULE,{update_config, #{Key => Value}}).

update_config()->
    Config = read_config(),
    gen_server:call(?MODULE, {update_config, Config}).

-spec get_config(_Key)-> {ok, _Value} | error.
get_config(Key)->
    gen_server:call(?MODULE, {get_config, Key}).

show()->
    State = gen_server:call(?MODULE, get_state),
    {"This Node:",      node(),
     "Route tab:",      maps:get(?router_items,  State),
     "Path to other:",  maps:get(?path_to_other, State),
     "help pid:",       maps:get(?help_pid,      State),
     "Config:",         maps:get(?config,        State)}.

%% ============================================================================================
%% gen_server Function
%% ============================================================================================
init(_Args) ->
    % Log在本地输出.
    erlang:group_leader(whereis(init),self()),
    Pid = spawn_link(fun()-> help_process_loop() end),
    Config = read_config(),
    ok = net_kernel:monitor_nodes(true),
    %init返回 {ok, State} ，其中 State 是gen_server的内部状态。
    {ok, ?init_router_state#{?help_pid := Pid,
                             ?config := Config}}.

terminate(normal, _State) ->
    error_logger:info_msg("Stoping router...\n"),
    ok.

handle_call(get_state, _From, State) ->
    % for debug
    {reply, State, State};

% -- for config
handle_call({update_config, NewConfig}, _From, #{?config := OldConfig} = State) ->
    {reply, ok, State#{?config := maps:merge(OldConfig, NewConfig)}};
handle_call(get_config, _From, #{?config := Config} = State) ->% ConfigMap
    {reply, Config, State};
handle_call({get_config, Key}, _From, #{?config := Config} = State) ->% {ok, Value} | error.
    Reply = maps:find(Key, Config),
    {reply, Reply, State};

% -- to get router info
handle_call(get_all_nodes, _From, State) ->
    AllNodesList = maps:keys(maps:get(?path_to_other, State)),
    {reply, AllNodesList, State};
handle_call({get_path_to_other, Node}, _From, State) ->
    Path = maps:find(Node, maps:get(?path_to_other, State)),
    {reply, Path, State};
handle_call(get_all_router_items, _From, State) ->
    {reply, maps:get(?router_items, State), State};

%%% -- to collect router info
%%handle_call({collect_router_request, KnowenNodeList, Ref}, _From, State) ->
%%    % KnowenNodeList :: [已知节点]
%%    ?log(11,{collect_router_request, KnowenNodeList, Ref}),
%%    RouterMap =
%%        case get(Ref) of
%%            ?undef ->
%%                handle_ref(State, Ref),
%%                % 检查与自己连接的节点是不是都在 KnowenNodeList中,如果有未知节点就向未知节点也发信.
%%                SysUnKnowenNodeList = nodes() -- KnowenNodeList,
%%                ?log(11,{"SysUnKnowenNodeList", SysUnKnowenNodeList}),
%%                {SysUnKnowenNodeRouterMapList, _BadNodes} = rpc:multicall(SysUnKnowenNodeList, gen_server, call,
%%                                [?MODULE, {collect_router_request, KnowenNodeList ++ SysUnKnowenNodeList, Ref}]),
%%                lists:foldl(
%%                    fun(RouterMap, Acc) when is_map(RouterMap)->
%%                            maps:merge(RouterMap, Acc);
%%                       (BadRouterMap, Acc) ->
%%                            ?log(9, {"BadRouterItem:",BadRouterMap}),
%%                            Acc
%%                    end,
%%                    #{node() => #router_item{connected_list = nodes(),
%%                                             timestamp = erlang:system_time(millisecond)} }, %本节点直连点.
%%                    SysUnKnowenNodeRouterMapList);
%%            _ ->
%%                #{} % ignore 已经处理过这个router请求了
%%        end,
%%    {reply, RouterMap, State};

handle_call(Request, _From, OldState) ->
    ?log(1, {"router.unhandle_call:",{Request, _From, OldState}}),
    {reply, Request, OldState}. %{reply, Reply, State1}。Reply是需要回馈给客户端的答复，同时 State1 是gen_server的状态的新值。

handle_cast({update_router_item, NewRouterMap, KnownNodeList, Ref}, State) ->
    % KnowenNodeList :: [已知节点]
    ?log(11, {update_router_item, NewRouterMap, KnownNodeList, Ref}),
    NewState =
        case get(Ref) of
            ?undef ->
                handle_ref(State, Ref),
                % 检查与自己连接的节点是不是都在 KnowenNodeList中,如果有未知节点就向未知节点也发信.
                SysUnKnowenNodeList = nodes() -- KnownNodeList,
                rpc:multicall(SysUnKnowenNodeList, gen_server, cast,
                                [router, {update_router_item, NewRouterMap, KnownNodeList ++ SysUnKnowenNodeList, Ref}]),
                % 更新本地路由信息
                update_router_info(State, NewRouterMap);
            _ ->
                State % ignore 已经处理过这个请求
        end,
    {noreply, NewState};
handle_cast({update_router_info,NewRouterMap}, State) ->
    NewState = update_router_info(State, NewRouterMap),
    {noreply, NewState};

handle_cast({log, {Level, What, Node, Pid, Module, Fun, Line, Time}}, State)->
    {ok, LevelThreshold} = get_config_from_state(?log_level, State),
    Level =< LevelThreshold andalso
        io:format("=> ~p~n"
                  "\t\t\t\t->log:~p N:~p P:~p M:~p:~p - ~p T:~s~n",[What, Level, Node, Pid, Module, Fun, Line, Time]),

    {noreply, State};

handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast(_Request, OldState) ->
    ?log(1, {"router.unhandle_cast:",{_Request, OldState}}),
    {noreply, OldState}.

handle_info({nodeup, Node},State)->
    ?log(1, {nodeup, Node}),
    cast_router_info(),
    {noreply, State};
handle_info({nodedown, Node},State)->
    ?log(1, {nodedown, Node}),
    cast_router_info(),
    {noreply, State};
handle_info({timeout, _TimerRef, {del_ref, Ref}},State)->
    erase(Ref),
    {noreply, State};
handle_info(Info,State)->
    ?log(1, {"router.unhandle_info:",Info}),
    {noreply, State}.

code_change(_OldVsn, State, _Extra)->
    ?log(1, {"router.code_change",_OldVsn, State, _Extra}),
    {ok, State}.

%% ============================================================================================
%% Internal Function
%% ============================================================================================

help_process_loop() ->
    NodesList1 = all_nodes(),
    NodesList2 = maps:keys(gen_server:call(?MODULE, get_all_router_items)),
    AllNodesList = lists:umerge(lists:usort(NodesList1), lists:usort(NodesList2)),
    get_config(?automatically_connect) == {ok, ?true} andalso
        spawn(fun() -> [net_adm:ping(N) || N <- AllNodesList] end),

    get_config(?infect_others) == {ok, ?true} andalso
        [rpc:cast(N, application, start, [?APP_NAME]) || N <- nodes()],

    cast_router_info(),

    {ok, SleepTime} = get_config(?time_to_update_router),
    timer:sleep(SleepTime),

    % for code change.动态链接至运行时代码
    ?MODULE:help_process_loop().

cast_router_info()->
    NewRouterMap = #{node() => #router_item{connected_list = nodes(),
                                            timestamp = erlang:system_time(millisecond)}},
    ?log(11, {"This Node NewRouterItem:",NewRouterMap}),
    rpc:multicall(gen_server, cast,
                    [?MODULE, {update_router_item, NewRouterMap, [node()|nodes()], erlang:make_ref()}]).

handle_ref(State, Ref)->
    put(Ref, ref),
    {ok, TimeToDelRef} = get_config_from_state(?time_to_del_ref, State),
    erlang:start_timer(TimeToDelRef, self(), {del_ref, Ref}).

update_router_info(State, NewRouterMap)-> % NewState
    #{?router_items := OldRouterMap} = State,
    NewRouterMap1 = maps:merge(OldRouterMap, NewRouterMap),% the value in Map1 is superseded by the value in Map2
    % 过滤掉太老的路由项.
    NewRouterMap2 = maps:filter(
        fun(Node, #router_item{timestamp = Timestamp})->
            {ok, TimeToDelRouter} = get_config_from_state(?time_to_del_router_item, State),
            LegalTime = erlang:system_time(millisecond) - TimeToDelRouter,
            Result = Timestamp > LegalTime,
            Result == ?false andalso ?log(1,{"Node disconnectd", Node}),
            Result
        end,
        NewRouterMap1),
    ?log(100, {"update_router_info new:",NewRouterMap2}),
    PathMap = find_path_for_all(NewRouterMap2),
    State#{?router_items := NewRouterMap2,
           ?path_to_other := PathMap}.

find_path_for_all(RouterMap)->%PathMap
    Nodes = [node()|nodes()],
    InitPathMap = lists:foldl(
        fun(N,Acc)->
            Acc#{N => [N]}
        end, #{}, Nodes),
    find_path_for_all_help(RouterMap, InitPathMap, queue:from_list(Nodes)).

find_path_for_all_help(RouterMap, PathMap, Queue)->%PathMap
    case queue:out(Queue) of
        {{value, Node}, TQueue} ->
            {NewPathMap,NewQueue} =
                case RouterMap of
                    #{Node := #router_item{connected_list = NodeListConnToThis}} ->
                        #{Node := PathToThisNode} = PathMap,
                        lists:foldl(
                            fun(N,{AccPathMap,AccQueue})->
                                case maps:is_key(N, AccPathMap) of
                                    true->
                                        {AccPathMap,AccQueue}; % N 节点已经在PathMap中了
                                    false->
                                        {AccPathMap#{N => PathToThisNode ++ [N]}, queue:in(N, AccQueue)}
                                end
                            end, {PathMap,TQueue}, NodeListConnToThis);
                    _ ->
                        % 没有该节点的直连节点列表信息,不能通过此节点向外扩展.
                        ?log(11, {"Router Item not found, it may not have router proc.node:", Node}),
                        {PathMap,TQueue}
                end,
            find_path_for_all_help(RouterMap, NewPathMap, NewQueue);
        {empty, _} ->
            PathMap
    end.

% -spec read_config()-> ConfigMap
read_config()->
    UserConfigMap =
        try
            {ok,[[HomeDir]]} = init:get_argument(home),
            FilePath = filename:join(HomeDir, "router.config"),
            {ok,[ConfigMap | _]} = file:consult(FilePath),
            ConfigMap
        catch
            T:P ->
                ?log(9, {"Unable to read config",T,P}),
                #{}
        end,
    DefaultConfigMap =
        #{
            %删除ref的时间. (ms)
            ?time_to_del_ref=> 100*1000,
            % 更新路由信息的时间 (ms)
            ?time_to_update_router => 10*1000,
            % 每条路由生存周期.
            ?time_to_del_router_item => 50*1000,
            % 要显示log的最低等级
            % log优先级,数字越小优先级越高,最高为0.
            ?log_level => 10,
            % 本节点role
            ?role => ?undef,
            % 尝试在其他节点启动本应用
            ?infect_others => ?true,
            % 尝试连接所有已知节点
            ?automatically_connect => ?true
        },
    maps:merge(DefaultConfigMap, UserConfigMap).

% -spec get_config_from_state(Key, State)-> {ok, Value} | error.
get_config_from_state(Key, #{?config := Config} = _State)->
    maps:find(Key, Config).

% doc
% 路由:
%         每个节点都会在随机时间后尝试向所有已知节点发出ping请求连接,并向周围直连节点发出自己的路由表,
%         周围节点收到路由表后开始更新自己的路由表.
%           在这个系统里我们遵循以下原则:
%               每个节点要么在自己节点直联表变化时发出路由信息,要么转发别人的变化信息,不会主动发出别人节点的路由信息.
%           每个节点启动一个进程,每隔一段时间后就尝试ping所有已知节点,
%           当本节点的nodes()表和State中的本节点路由项不同时就 1.更新自己的路由信息和path表 2.向其他节点广播自己的新变化
%           其他节点收到新路由项后做 1.更新自己的路由信息和path表 2.向未知节点广播路由变化 3.X尝试ping所有节点,检查自己是否有路由信息变化.
%           目前和此路由相关的主要有函数:
%               help_process_loop()
%               handle_cast({update_router_item, NewRouterMap, KnowenNodeList, Ref}, State)
%               *update_router_info(State, NewRouterMap)
%
% Log系统
%   带有优先级的log系统.
% 代码远程加载
% 配置文件
% 角色系统.

% todo
%   通信加密.
