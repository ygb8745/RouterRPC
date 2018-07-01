-define(DEBUG, true).

%% ============================================================================================
%% Macro
%% ============================================================================================

-ifdef(DEBUG).
    -define(log(What), ?log(1, What)).
    -define(log(Level, What),
                gen_server:cast(router,{log, {Level, What, node(), self(), ?MODULE,?LINE,format_timestamp()}})).

    format_timestamp() ->
        {_,_,Micro} = os:timestamp(),
        {{_Year,_Month,_Day},{Hour,Minute,Second}} =
            calendar:local_time(),
        io_lib:format("~4w-~2.2.0w-~2.2.0w ~2.2.0w:~2.2.0w:~2.2.0w.~p",
                        [_Year,_Month,_Day,Hour,Minute,Second,Micro]).
-else.
    -define(log(_What), void).
    -define(log(_Level, _What), void).
-endif.

-define(undef,                          undefined).
-define(true,                           true).
-define(false,                          false).
-define(router,                         router).
-define(time_to_del_ref,                time_to_del_ref).
-define(time_to_update_router,          time_to_update_router).
-define(time_to_del_router_item,        time_to_del_router_item).
-define(log_level,                      log_level).
-define(role,                           role).

%% ============================================================================================
%% Data type
%% ============================================================================================

-record(router_state,{
    reouter_items = #{}, % #{node_name() => #router_item{}}
    path_to_other = #{}, % 每条记录是到达其他节点的路径: 节点名->[到达该节点的路径列表]
                         % 这个map下所有作为key的node()就是全部的已知节点.
    help_pid,            % help process
    config               % map #{}
}).

% 用法 用于作为router_map的一项: node() => #router_item{}
% -type router_map() = #{node_name() => #router_item{}}.
% -type path_map() =   #{node_name() => [_Path_to_this_node = node_name()]}.
-type(node_name() :: atom()).
-record(router_item,{
    connected_list, % [直连节点列表]
    timestamp % 最后更新该项的时间戳
}).
