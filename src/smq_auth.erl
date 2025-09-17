%%%-------------------------------------------------------------------
%%% @doc Public API for call SuperMQ Authentication and Authorization
%%%-------------------------------------------------------------------
-module(smq_auth).
-include_lib("kernel/include/logger.hrl").
-include_lib("smq_auth/include/smq_auth.hrl").

-export([convert_service_config/1, convert_grpc_config/1, default_grpc_config/0]).
-export([client_authn/1, client_authz/1, init_smq_grpc/1, close_smq_grpc/0]).

default_grpc_config() ->
    #smq_grpc_config{
        auth = #smq_service_grpc_config{
            host = "localhost",
            port = 7001
        },
        clients = #smq_service_grpc_config{
            host = "localhost",
            port = 7006
        },
        channels = #smq_service_grpc_config{
            host = "localhost",
            port = 7005
        }
    }.
-spec convert_service_config(map() | term()) -> smq_service_grpc_config() | {error, term()}.
convert_service_config(ServiceMap) when is_map(ServiceMap) ->
    case {maps:is_key(host, ServiceMap), maps:is_key(port, ServiceMap)} of
        {true, true} ->
            HostVal = maps:get(host, ServiceMap),
            PortVal = maps:get(port, ServiceMap),
            #smq_service_grpc_config{
                host =
                    case HostVal of
                        % convert binary -> string
                        H when is_binary(H) -> binary_to_list(H);
                        % already string
                        H when is_list(H) ->
                            Hr = io_lib:format("~p", [H]),
                            lists:flatten(Hr)
                    end,
                port =
                    case PortVal of
                        P when is_integer(P) -> P;
                        P when is_binary(P) -> list_to_integer(binary_to_list(P));
                        P when is_list(P) -> list_to_integer(P)
                    end
            };
        _ ->
            error(missing_keys)
    end;
convert_service_config(_) ->
    error(invalid_service_map).

-spec convert_grpc_config(map() | term()) ->
    #smq_grpc_config{} | {error, term()}.
convert_grpc_config(ConfigMap) when is_map(ConfigMap) ->
    case convert_auth(ConfigMap) of
        {ok, Auth} ->
            case convert_channels(ConfigMap) of
                {ok, Channels} ->
                    case convert_clients(ConfigMap) of
                        {ok, Clients} ->
                            #smq_grpc_config{
                                auth = Auth,
                                channels = Channels,
                                clients = Clients
                            };
                        {error, Reason} ->
                            {error, Reason}
                    end;
                {error, Reason} ->
                    {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end;
convert_grpc_config(_) ->
    {error, invalid_config_map}.

%% Helpers
convert_auth(ConfigMap) ->
    case maps:is_key(auth, ConfigMap) of
        true ->
            case convert_service_config(maps:get(auth, ConfigMap)) of
                Rec when is_record(Rec, smq_service_grpc_config) ->
                    {ok, Rec};
                {error, Reason} ->
                    {error, Reason};
                Other ->
                    {error, {invalid_auth_config, Other}}
            end;
        false ->
            {error, missing_auth_key}
    end.

convert_channels(ConfigMap) ->
    case maps:is_key(channels, ConfigMap) of
        true ->
            case convert_service_config(maps:get(channels, ConfigMap)) of
                Rec when is_record(Rec, smq_service_grpc_config) ->
                    {ok, Rec};
                {error, Reason} ->
                    {error, Reason};
                Other ->
                    {error, {invalid_channels_config, Other}}
            end;
        false ->
            {error, missing_channels_key}
    end.

convert_clients(ConfigMap) ->
    case maps:is_key(clients, ConfigMap) of
        true ->
            case convert_service_config(maps:get(clients, ConfigMap)) of
                Rec when is_record(Rec, smq_service_grpc_config) ->
                    {ok, Rec};
                {error, Reason} ->
                    {error, Reason};
                Other ->
                    {error, {invalid_clients_config, Other}}
            end;
        false ->
            {error, missing_clients_key}
    end.

-spec init_smq_grpc(#smq_grpc_config{}) -> {ok, [atom()] | none()} | {error, term()}.
init_smq_grpc(Config = #smq_grpc_config{}) ->
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
                {http, Config#smq_grpc_config.auth#smq_service_grpc_config.host,
                    Config#smq_grpc_config.auth#smq_service_grpc_config.port, []}
            ],
            #{}},
        {clients,
            [
                {http, Config#smq_grpc_config.clients#smq_service_grpc_config.host,
                    Config#smq_grpc_config.clients#smq_service_grpc_config.port, []}
            ],
            #{}},
        {channels,
            [
                {http, Config#smq_grpc_config.channels#smq_service_grpc_config.host,
                    Config#smq_grpc_config.channels#smq_service_grpc_config.port, []}
            ],
            #{}}
    ],

    io:format("channels ~p~n", [Channels]),

    application:set_env(grpcbox, client, #{channels => Channels}),

    case application:ensure_all_started(grpcbox) of
        {ok, Started} ->
            logger:info("grpcbox started with channels: ~p~n", [Channels]),
            {ok, Started};
        {error, Reason} ->
            ?LOG_ERROR("Failed to start grpcbox: ~p~n", [Reason]),
            {error, Reason};
        undefined ->
            ?LOG_ERROR("Unexpected undefined return while starting grpcbox~n", []),
            {error, undefined}
    end.

close_smq_grpc() ->
    application:stop(grpcbox).

%% Public API: RabbitMQ plugin calls this
-spec client_authn(smq_client_authn_request()) -> smq_client_authn_result().

client_authn(#smq_client_authn_request{client_id = ClientID, client_key = ClientKey}) ->
    %% Create Basic Auth header string.
    %% BUG SMQ: After Basic there should be no space, otherwise all request will fail.
    AuthString = "Basic" ++ base64:encode_to_string(ClientID ++ ":" ++ ClientKey),

    %% Build request
    Req = #{token => AuthString},
    Opts = #{channel => clients},

    %% Match all possible outcomes
    Resp = clients_v_1_clients_service_client:authenticate(ctx:new(), Req, Opts),
    case Resp of
        {ok, ReplyData, Metadata} ->
            ?LOG_INFO("Authorization successful ~p ~p", [ReplyData, Metadata]),
            %% ReplyData :: authn_res()
            case maps:get(authenticated, ReplyData, false) of
                true ->
                    ID = maps:get(id, ReplyData, <<"">>),
                    IDBin = normalize_id(ID),
                    ?LOG_INFO("Auth OK for id=~p", [IDBin]),
                    {ok, IDBin};
                1 ->
                    ID = [maps:get(id, ReplyData, <<"">>)],
                    IDBin = normalize_id(ID),
                    ?LOG_INFO("Auth OK for id=~p", [IDBin]),
                    {ok, IDBin};
                _ ->
                    ?LOG_WARNING("Auth failed: ~p", [ReplyData]),
                    {error, unauthenticated}
            end;
        {error, {Code, Msg}, Meta} ->
            ?LOG_WARNING("RPC failed Code=~p Msg=~p Meta=~p", [Code, Msg, Meta]),
            {error, {Code, Msg}};
        {error, Reason} ->
            ?LOG_ERROR("RPC failed: ~p", [Reason]),
            {error, Reason};
        {grpc_error, Details} ->
            ?LOG_ERROR("gRPC error: ~p", [Details]),
            {grpc_error, Details};
        Other ->
            ?LOG_ERROR("Unexpected RPC response: ~p", [Other]),
            {error, {unexpected_response, Other}}
    end.

-spec normalize_id(binary() | unicode:chardata()) -> binary().
normalize_id(ID) ->
    case unicode:characters_to_binary(ID, utf8, utf8) of
        Bin when is_binary(Bin) ->
            Bin;
        {error, _Bin, _Rest} ->
            %% fallback: return empty binary
            <<>>;
        {incomplete, _Bin, _Rest} ->
            %% fallback: return empty binary
            <<>>
    end.

-spec client_authz(smq_client_authz_request()) -> smq_client_authz_result().

client_authz(#smq_client_authz_request{
    domain_id = DomainID,
    channel_id = ChannelID,
    client_id = ClientID,
    client_type = ClientType,
    client_key = _ClientKey,
    type = Type
}) ->
    %% Convert enum atom to gRPC numeric value
    TypeVal =
        case Type of
            publish -> 1;
            subscribe -> 2;
            _ -> erlang:error({invalid_type, Type})
        end,

    %% Convert ClientType atom to string for gRPC
    ClientTypeVal =
        case ClientType of
            client -> "client";
            user -> "user";
            _ -> erlang:error({invalid_client_type, ClientType})
        end,

    %% Build the request map
    Req = #{
        domain_id => DomainID,
        client_id => ClientID,
        client_type => ClientTypeVal,
        channel_id => ChannelID,
        type => TypeVal
    },

    %% Options for grpcbox client
    Opts = #{channel => channels},

    %% Call the gRPC authorize method
    Resp = channels_v_1_channels_service_client:authorize(ctx:new(), Req, Opts),

    %% Normalize all possible outcomes
    case Resp of
        {ok, ReplyData, Metadata} ->
            ?LOG_INFO("Authorization successful! Reply: ~p, Metadata: ~p", [ReplyData, Metadata]),
            case maps:get(authorized, ReplyData, false) of
                true ->
                    ?LOG_INFO("Authorized", []),
                    {ok};
                1 ->
                    ?LOG_INFO("Authorized", []),
                    {ok};
                _ ->
                    ?LOG_WARNING("Auth failed: ~p", [ReplyData]),
                    {error, {unauthorized}}
            end;
        {error, {Code, Msg, _Meta}} ->
            ?LOG_ERROR("gRPC error: Code=~p Msg=~p", [Code, Msg]),
            {error, {Code, Msg}};
        {error, Reason} ->
            ?LOG_ERROR("RPC failed: ~p", [Reason]),
            {error, Reason};
        {grpc_error, Details} ->
            ?LOG_ERROR("gRPC error: ~p", [Details]),
            {grpc_error, Details};
        Other ->
            ?LOG_ERROR("Unexpected RPC response: ~p", [Other]),
            {error, {unexpected_response, Other}}
    end.
