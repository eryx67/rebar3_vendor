-module(rebar3_prv_vendor_store).

-export([init/1, do/1, format_error/1]).

-define(PROVIDER, store).
-define(NAMESPACE, vendor).
-define(DEPS, [{default, install_deps}, {default, lock}]).

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
        {example, "rebar3 vendor store"},
        {opts, []},
        {short_desc, "Makes a copy of dependencies to deps/ for vendoring."},
        {desc, ""}
    ]),
    {ok, rebar_state:add_provider(State, Provider)}.


-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    %% init
    AllDeps = rebar_state:lock(State),
    DepsDir = rebar_dir:deps_dir(State),
    VendorDir = filename:join(rebar_dir:root_dir(State), "deps"),
    filelib:ensure_dir(filename:join([VendorDir, "dummy.beam"])),

    PluginDeps = rebar_state:all_plugin_deps(State),
    PluginDir = rebar_dir:plugins_dir(State),
    PluginVendorDir = filename:join(rebar_dir:root_dir(State), "plugin_deps"),
    filelib:ensure_dir(filename:join([PluginVendorDir, "dummy.beam"])),

    %% clean deps to ensure that no compile code is included
    clean_all_deps(State),
    %% zip all dependencies in the /deps directory
    rebar_api:info("Vendoring dependencies...", []),
    [begin
    %% get info
        Name = binary_to_list(rebar_app_info:name(Dep)),
        Vsn = get_vsn(Dep, State),
        %% prepare filename
        Filename = iolist_to_binary([Name, "-", Vsn, ".zip"]),
        Filepath = binary_to_list(filename:join([VendorDir, Filename])),
        %% purge other versions if they exist
        purge_other_versions(VendorDir, Filepath, Name),
        %% create zip if doesn't exist
        create_zip_if_not_exist(DepsDir, Filepath, Name)
    end || Dep <- AllDeps, not(rebar_app_info:is_checkout(Dep))],

    %% zip all plugin dependencies in the /plugin_deps directory
    rebar_api:info("Vendoring plugin dependencies...", []),
    [begin
    %% get info
        Name = binary_to_list(rebar_app_info:name(Dep)),
        Vsn = get_vsn(Dep, State),
        %% prepare filename
        Filename = iolist_to_binary([Name, "-", Vsn, ".zip"]),
        Filepath = binary_to_list(filename:join([PluginVendorDir, Filename])),
        %% purge other versions if they exist
        purge_other_versions(PluginVendorDir, Filepath, Name),
        %% create zip if doesn't exist
        create_zip_if_not_exist(PluginDir, Filepath, Name)
     end || Dep <- PluginDeps, not(rebar_app_info:is_checkout(Dep))],
    %% return
    {ok, State}.

-spec format_error(any()) -> iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

-spec clean_all_deps(rebar_state:t()) -> ok.
clean_all_deps(State) ->
    %% temporary hack: add the 'all' option to be able to clean all dependencies
    {Args, Other} = rebar_state:command_parsed_args(State),
    State1 = rebar_state:command_parsed_args(State, {Args ++ [{all, true}], Other}),
    {ok, _} = rebar_prv_clean:do(State1),
    ok.

-spec get_vsn(rebar_app_info:t() | reabar_pkg_resource:package(), rebar_state:t()) -> binary() | string().
get_vsn(Dep, State) ->
    Lock = try
               rebar_fetch:lock_source(Dep, State)
           catch
               _:E:T ->
                   rebar_api:info("get_vsn: ~p ~p", [E, T]),
                   Dep
           end,
    case Lock of
        {git, _, {ref, Ref}} -> Ref;
        {pkg, _, Vsn0} -> Vsn0;
        {pkg, _, Vsn0, _} -> Vsn0
    end.

-spec purge_other_versions(file:filename_all(), file:filename_all(), binary() | string()) -> list().
purge_other_versions(VendorDir, Filepath, Name) ->
    OtherFilepathPattern = filelib:wildcard(filename:join(VendorDir, string:concat(Name, "-*.zip"))),
    [begin
        rebar_api:info("   - ~s", [filename:basename(OtherFilepath, ".zip")]),
        ok = file:delete(OtherFilepath)
    end || OtherFilepath <- OtherFilepathPattern, OtherFilepath =/= Filepath].

create_zip_if_not_exist(DepsDir, Filepath, Name) ->
    case filelib:is_file(Filepath) of
        true ->
            rebar_api:debug("Skipping ~s: already vendored.", [filename:basename(Filepath, ".zip")]);
        false ->
            %% create zip   ===>
            rebar_api:info("   + ~s", [filename:basename(Filepath, ".zip")]),
            TmpDir = rebar_file_utils:system_tmpdir([integer_to_list(erlang:phash2(erlang:make_ref()))]),
            try
                rebar_file_utils:reset_dir(TmpDir),
                rebar_file_utils:cp_r([filename:join(DepsDir, Name)], TmpDir),
                rebar_file_utils:rm_rf(filename:join([TmpDir, Name, "ebin"])),
                rebar_file_utils:rm_rf(filename:join([TmpDir, Name, ".rebar"])),
                {ok, _} = zip:create(Filepath, [Name], [{cwd, TmpDir}])
            after
                rebar_file_utils:rm_rf(TmpDir)
            end
    end.
