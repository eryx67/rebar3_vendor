-module(rebar3_prv_vendor_apply).

-export([init/1, do/1, format_error/1]).

-define(PROVIDER, apply).
-define(NAMESPACE, vendor).
-define(DEPS, []).

%% ===================================================================
%% Public API
%% ===================================================================
-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Provider = providers:create([
        {name, ?PROVIDER},
        {module, ?MODULE},
        {namespace, ?NAMESPACE},
        {bare, true},
        {deps, ?DEPS},
        {example, "rebar3 vendor apply"},
        {opts, []},
        {short_desc, ""},
        {desc, ""}
    ]),
    {ok, rebar_state:add_provider(State, Provider)}.


-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    rebar_api:info("Applying vendored dependencies...", []),
    %% init
    DepsDir = rebar_dir:deps_dir(State),
    VendorDir = filename:join(rebar_dir:root_dir(State), "deps"),
    filelib:ensure_dir(filename:join(DepsDir, "dummy.beam")),

    PluginDir = rebar_dir:plugins_dir(State),
    PluginVendorDir = filename:join(rebar_dir:root_dir(State), "plugin_deps"),
    filelib:ensure_dir(filename:join(PluginDir, "dummy.beam")),
    %% extract
    [begin
         Filename = filename:basename(Filepath, ".zip"),
         rebar_api:info("Extracting ~s", [Filename]),
         rebar_file_utils:rm_rf(filename:join(DepsDir, Filename)),
         zip:extract(Filepath, [{cwd, DepsDir}])
    end || Filepath <- filelib:wildcard(filename:join(VendorDir, "*.zip"))],

    [begin
         Filename = filename:basename(Filepath, ".zip"),
         rebar_api:info("Extracting plugin dependency ~s", [Filename]),
         rebar_file_utils:rm_rf(filename:join(PluginDir, Filename)),
         zip:extract(Filepath, [{cwd, PluginDir}])
    end || Filepath <- filelib:wildcard(filename:join(PluginVendorDir, "*.zip"))],
    {ok, State}.

-spec format_error(any()) -> iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).
