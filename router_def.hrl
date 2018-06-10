 -define(DEBUG, true).

%% ============================================================================================
%% Macro
%% ============================================================================================

-ifdef(DEBUG).
%todo 带有优先级的log
-define(log(What),
            io:format(
                "=> ~p~n"
                "\t\t\t\t log: N:~p P:~p M:~p L:~p T:~s~n",[What, node(), self(), ?MODULE,?LINE,format_timestamp()])).
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

-define(time_to_del_ref, 100*1000).%100s
-define(time_to_update_router, 2000).%2s

%% ============================================================================================
%% Data type
%% ============================================================================================

-record(router_state,{
    reouter_items = #{}, % 每条记录是: 节点名-> [该节点可达的节点列表]
    path_to_other = #{}, % 每条记录是到达其他节点的路径: 节点名->[到达该节点的路径列表]
                         % 这个map下所有作为key的node()就是全部的已知节点.
    help_pid             % help process
}).

% 用法 用于作为router_map的一项: node() => #router_item{}
% -type router_map() = #{node_name() => #router_item{}}.
% -type path_map() =   #{node_name() => [_Path_to_this_node = node_name()]}.
% -type node_name() = atom().
-record(router_item,{
    connected_list, % [直连节点列表]
    timestamp % 最后更新该项的时间戳
}).
