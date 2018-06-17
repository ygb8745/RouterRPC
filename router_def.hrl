-define(DEBUG, true).

%% ============================================================================================
%% Macro
%% ============================================================================================

-ifdef(DEBUG).
    -define(log(What), ?log(11, What)).
    -define(log(Level, What),
                gen_server:cast(router,{log, {Level, What, node(), self(), ?MODULE,?LINE,format_timestamp()}})).

    format_timestamp() ->
        {_,_,Micro} = os:timestamp(),
        {{_Year,_Month,_Day},{Hour,Minute,Second}} =
            calendar:local_time(),
        io_lib:format("~p:~p:~p.~p",
                        [Hour,Minute,Second,Micro]).
-else.
    -define(log(_What), void).
    -define(log(_Level, _What), void).
-endif.

-define(undef, undefined).
-define(router, router).

-define(time_to_del_ref, 100*1000).%100s
-define(time_to_update_router, 2000).%2s
-define(live_period_for_router_item, 10). % 每个路由项的生存周期.

%% ============================================================================================
%% Data type
%% ============================================================================================

-record(router_state,{
    reouter_items = #{}, % #{node_name() => #router_item{}}
    path_to_other = #{}, % 每条记录是到达其他节点的路径: 节点名->[到达该节点的路径列表]
                         % 这个map下所有作为key的node()就是全部的已知节点.
    help_pid,            % help process
    log_level = 10       % log优先级数字小于等于此值得才会被处理.
                         %      log优先级,数字越小优先级越高,最高为0.
}).

% 用法 用于作为router_map的一项: node() => #router_item{}
% -type router_map() = #{node_name() => #router_item{}}.
% -type path_map() =   #{node_name() => [_Path_to_this_node = node_name()]}.
-type(node_name() :: atom()).
-record(router_item,{
    connected_list, % [直连节点列表]
    timestamp % 最后更新该项的时间戳
}).
