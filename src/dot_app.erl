-module(dot_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).
-export([get_extra_data/1]).

start(_Type, _Args) ->
    %% We want to initialise the size of the
    %%  #appl{} record before we potentially
    %%  modify it.
    _ = dot_app_utils:get_appl_size(),
    {OrigObjCode, Ret} = setup_hooks(),
    erlang:put('$application_object_code', OrigObjCode),
    case Ret of
        {module, _} ->
            {ok, self()};
        _ ->
            Ret
    end.

stop(_State) ->
    {Mod, BeamBin, FilePath} = erlang:get('$application_object_code'),
    case force_load_binary(Mod, FilePath, BeamBin) of
        {module, Mod} ->
            ok;
        Error ->
            Error
    end.

-spec get_extra_data(Application :: atom()) -> undefined | list().
get_extra_data(Application) when is_atom(Application) ->
    case ets:lookup(dot_app_utils:ac_tab(), {loaded, Application}) of
        [] ->
            undefined;
        [{{loaded, Application}, Appl}] ->
            ApplSize = dot_app_utils:get_appl_size(),
            case tuple_size(Appl) of
                ApplSize ->
                    undefined;
                _ ->
                    element(ApplSize+1, Appl)
            end
    end.

%% Internal Functions

setup_hooks() ->
    OrigObjCode = {Mod, BeamBin, FilePath} = code:get_object_code(application),
    {ok, {Mod, [{abstract_code, AbstCode}]}} = beam_lib:chunks(BeamBin, [abstract_code]),
    {raw_abstract_v1, AST} = AbstCode,
    InjectedAST = inject(AST, fun dot_app_wrapper:handle_hook/3, []),
    {ok, Mod, InjectedBeamBin} = compile:forms(InjectedAST),
    Ret = force_load_binary(Mod, FilePath, InjectedBeamBin),
    {OrigObjCode, Ret}.

inject([], _F, Acc) ->
    lists:reverse(Acc);
inject([FuncAST0 = {function, _, load, Arity, BodyAST0}|T], F, Acc) when is_function(F, 3) ->
    {type, external} = erlang:fun_info(F, type),
    VarNameStr = "Value__" ++ atom_to_list(?MODULE) ++ "__" ++ integer_to_list(os:system_time()),
    VarNameAtom = list_to_atom(VarNameStr),
    {module, FunMod} = erlang:fun_info(F, module),
    {name, FunName} = erlang:fun_info(F, name),
    BodyAST1 =
        [begin
             VarsString = string:join([atom_to_list(VarName) || {var, _, VarName} <- Vars], ","),
             ExecStr = lists:flatten(io_lib:format("catch ~p:~p({load, ~b}, [~s], ~s).", [FunMod, FunName, Arity, VarsString, VarNameStr])),
             BlockAST = {block, Location, ClauseAST},
             {ok, ExecTokens, _} = erl_scan:string(ExecStr, Location),
             {ok, [ExecAST]} = erl_parse:parse_exprs(ExecTokens),
             AST = [
                 {match, Location, {var, Location, VarNameAtom}, BlockAST},
                 ExecAST,
                 {var, Location, VarNameAtom}
             ],
             {clause, Location, Vars, Guards, AST}
        end || {clause, Location, Vars, Guards, ClauseAST} <- BodyAST0],
    FuncAST1 = setelement(5, FuncAST0, BodyAST1),
    inject(T, F, [FuncAST1|Acc]);
inject([H|T], F, Acc) when is_function(F) ->
    {type, external} = erlang:fun_info(F, type),
    inject(T, F, [H|Acc]).

force_load_binary(Mod, FilePath, BeamBin) ->
    FileDir = filename:dirname(FilePath),
    case code:unstick_dir(FileDir) of
        ok ->
            Ret = code:load_binary(Mod, FilePath, BeamBin),
            case code:stick_dir(FileDir) of
                ok ->
                    Ret;
                error ->
                    error
            end;
        error ->
            error
    end.