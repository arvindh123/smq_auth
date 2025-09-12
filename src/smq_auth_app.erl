-module(smq_auth_app).
-doc "smq_auth Application.".

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    HostsAndPorts = [
        {auth_host, "localhost"},
        {auth_port, 7001},
        {clients_host, "localhost"},
        {clients_port, 7006},
        {channels_host, "localhost"},
        {channels_port, 7005}
    ],
    init_grpcbox(HostsAndPorts),

    smq_auth_sup:start_link().

stop(_State) ->
    ok.

%% internal functions

init_grpcbox(HostsAndPorts) ->
    %% Stop grpcbox if already running
    case application:stop(grpcbox) of
        ok -> ok;
        {error, {not_started, grpcbox}} -> ok
    end,

    application:unload(grpcbox),
    %% Make sure the app is loaded
    application:load(grpcbox),

    %% Build config dynamically
    Channels = [
        {auth,
            [
                {http, proplists:get_value(auth_host, HostsAndPorts),
                    proplists:get_value(auth_port, HostsAndPorts), []}
            ],
            #{}},
        {clients,
            [
                {http, proplists:get_value(clients_host, HostsAndPorts),
                    proplists:get_value(clients_port, HostsAndPorts), []}
            ],
            #{}},
        {channels,
            [
                {http, proplists:get_value(channels_host, HostsAndPorts),
                    proplists:get_value(channels_port, HostsAndPorts), []}
            ],
            #{}}
    ],

    application:set_env(grpcbox, client, #{channels => Channels}),

    %% Now actually start it
    case application:ensure_all_started(grpcbox) of
        {ok, Started} ->
            io:format("grpcbox started with channels: ~p~n", [Channels]),
            {ok, Started};
        {error, Reason} ->
            io:format("Failed to start grpcbox: ~p~n", [Reason]),
            {error, Reason};
        undefined ->
            io:format("Unexpected undefined return while starting grpcbox~n", []),
            {error, undefined}
    end.
