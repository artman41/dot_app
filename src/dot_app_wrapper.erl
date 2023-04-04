%%%-------------------------------------------------------------------
%%% @author artman41
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 04. Apr 2023 19:54
%%%-------------------------------------------------------------------
-module(dot_app_wrapper).
-author("artman41").

%% API
-export([handle_hook/3]).

handle_hook(FA = {Function, Arity}, Vars, Return) ->
    case is_called_from(application, Function, Arity) of
        true ->
            handle_hook_(FA, Vars, Return);
        false ->
            ok
    end.

handle_hook_({load, 1}, [Application], ok) ->
    load_additional_info(Application),
    ok;
handle_hook_({load, 2}, [Application, _DistNodes], ok) ->
    load_additional_info(Application),
    ok;
handle_hook_(_FA, _Vars, _Return) ->
    ok.

is_called_from(Mod, Fun, Arity) ->
    {current_stacktrace, Stack} = process_info(self(), current_stacktrace),
    case lists:nth(3, Stack) of
        {Mod, Fun, Arity, _} ->
            true;
        _ ->
            false
    end.

load_additional_info(Application) ->
    case ets:lookup(dot_app_utils:ac_tab(), {loaded, Application}) of
        [] ->
            erlang:error(badarg);
        [{{loaded, Application}, Appl}] ->
            case code:where_is_file(atom_to_list(Application) ++ ".app") of
                non_existing ->
                    ok;
                AppPath ->
                    load_additional_info_(Application, AppPath, Appl)
            end
    end.

load_additional_info_(Application, AppPath, Appl) ->
    ApplSize = dot_app_utils:get_appl_size(),
    LargerAppl0 =
        case tuple_size(Appl) of
            ApplSize ->
                erlang:make_tuple(ApplSize+1, undefined, lists:zip(lists:seq(1, ApplSize), tuple_to_list(Appl)));
            _ ->
                Appl
        end,
    {ok, [{application, dot_app, Props}]} = file:consult(AppPath),
    ExtraData = proplists:get_value(extra_data, Props, []),
    LargerAppl1 = setelement(ApplSize+1, LargerAppl0, ExtraData),
    ets:insert(dot_app_utils:ac_tab(), {{loaded, Application}, LargerAppl1}).