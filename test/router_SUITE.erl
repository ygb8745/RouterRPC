%%%-------------------------------------------------------------------
%%% @author ygb8745
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 27. 十一月 2018 22:44
%%%-------------------------------------------------------------------
-module(router_SUITE).
-author("ygb8745").
-compile(export_all).
-compile(nowarn_export_all).

-include_lib("eunit/include/eunit.hrl").
-include("../include/router_def.hrl").

% fun should end with  _test()
find_path_for_all_test() ->
    ?debugFmt("t_find_path_for_all starting...", []),

    TestList = [
        {_Input1 = #{
            node() => #router_item{connected_list = [node_a]},
            node_a => #router_item{connected_list = [node_b]}},
            _Output1 = #{
                nonode@nohost => [nonode@nohost],
                node_a => [nonode@nohost, node_a],
                node_b => [nonode@nohost, node_a, node_b]}},
        {_Input2 = #{
            node() => #router_item{connected_list = [node_a,node_b]},
            node_a => #router_item{connected_list = []},
            node_b => #router_item{connected_list = []}},
            _Output2 = #{
                nonode@nohost => [nonode@nohost],
                node_a => [nonode@nohost, node_a],
                node_b => [nonode@nohost, node_b]}},
        {_Input3 = #{
            node() => #router_item{connected_list = [node_a,node_b]},
            node_a => #router_item{connected_list = [node_c]},
            node_b => #router_item{connected_list = []},
            node_c => #router_item{connected_list = []}},
            _Output3 = #{
                nonode@nohost => [nonode@nohost],
                node_a => [nonode@nohost, node_a],
                node_b => [nonode@nohost, node_b],
                node_c => [nonode@nohost, node_a, node_c]}}
    ],
    lists:foreach(
        fun({RouterMap, PathMap}) ->
            Result = router:find_path_for_all(RouterMap),
            case PathMap == Result of
                true -> ok;
                false ->
                    ?debugFmt("Result miss match~n Input:~p~n Excepted Result:~p~n Actual Result:~p~n", [RouterMap, PathMap, Result]),
                    ?assert(false)
            end
        end,
        TestList
    ).
