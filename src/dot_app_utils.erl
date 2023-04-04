%%%-------------------------------------------------------------------
%%% @author artman41
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 04. Apr 2023 20:52
%%%-------------------------------------------------------------------
-module(dot_app_utils).
-author("artman41").

%% API
-export([
    ac_tab/0,
    get_appl_size/0
]).

-define(AC_TAB, ac_tab).

ac_tab() ->
    ?AC_TAB.

get_appl_size() ->
    Key = {?MODULE, appl_size},
    case ets:lookup(?AC_TAB, Key) of
        [{Key, ApplSize}] ->
            ApplSize;
        [] ->
            App = find_first_app_loaded(),
            [{_, Appl}] = ets:lookup(?AC_TAB, {loaded, App}),
            ApplSize = tuple_size(Appl),
            ets:insert(?AC_TAB, {Key, ApplSize}),
            ApplSize
    end.

%% INTERNAL

find_first_app_loaded() ->
    Key = ets:first(?AC_TAB),
    find_first_app_loaded_(Key).

find_first_app_loaded_('$end_of_table') ->
    erlang:error(badarg);
find_first_app_loaded_({loaded, App}) ->
    App;
find_first_app_loaded_(Key) ->
    find_first_app_loaded_(ets:next(?AC_TAB, Key)).
