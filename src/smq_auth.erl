%%%-------------------------------------------------------------------
%%% @doc Public API for call SuperMQ Authentication and Authorization
%%%-------------------------------------------------------------------
-module(smq_auth).
-include_lib("kernel/include/logger.hrl").

-export([client_authn/2, client_authz/5]).

%% Public API: RabbitMQ plugin calls this
-spec client_authn(ClientID :: string(), ClientKey :: string()) ->
    {ok, clients_pb:authn_res(), grpcbox:metadata()}
    %% {Code, Msg}
    | {error, {binary(), binary()}}
    %% other reason
    | {error, term()}
    %% grpc_error details
    | {grpc_error, term()}
    | {error, {unexpected_response, term()}}.

client_authn(ClientID, ClientKey) ->
    %% Create Basic Auth header string.
    %% BUG SMQ: After Basic there should be no space, other wise all request will fail.
    AuthString = "Basic " ++ base64:encode_to_string(ClientID ++ ":" ++ ClientKey),

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
                    ?LOG_INFO("Auth OK for id=~p", [maps:get(id, ReplyData, <<"">>)]),
                    {ok, ReplyData, Metadata};
                1 ->
                    ?LOG_INFO("Auth OK for id=~p", [maps:get(id, ReplyData, <<"">>)]),
                    {ok, ReplyData, Metadata};
                _ ->
                    ?LOG_WARNING("Auth failed: ~p", [ReplyData]),
                    {error, {unauthenticated, ReplyData}}
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

-spec client_authz(
    DomainID :: binary() | string(),
    ClientID :: binary() | string(),
    ClientType :: client | user,
    ChannelID :: binary() | string(),
    Type :: publish | subscribe
) ->
    {ok}
    | {error, {unauthorized}}
    %% gRPC error with code/msg
    | {error, {binary(), binary()}}
    %% generic RPC error
    | {error, term()}
    %% grpc_error response
    | {grpc_error, term()}
    | {error, {unexpected_response, term()}}.

client_authz(DomainID, ClientID, ClientType, ChannelID, Type) ->
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
            io:format("Authorization successful! Reply: ~p, Metadata: ~p~n", [ReplyData, Metadata]),
            case maps:get(authenticated, ReplyData, false) of
                true ->
                    ?LOG_INFO("Auth OK for id=~p", [maps:get(id, ReplyData, <<"">>)]),
                    {ok};
                1 ->
                    ?LOG_INFO("Auth OK for id=~p", [maps:get(id, ReplyData, <<"">>)]),
                    {ok};
                _ ->
                    ?LOG_WARNING("Auth failed: ~p", [ReplyData]),
                    {error, {unauthorized}}
            end;
        {error, {Code, Msg, _Meta}} ->
            io:format("gRPC error: Code=~p Msg=~p~n", [Code, Msg]),
            {error, {Code, Msg}};
        {error, Reason} ->
            io:format("RPC failed: ~p~n", [Reason]),
            {error, Reason};
        {grpc_error, Details} ->
            io:format("gRPC error: ~p~n", [Details]),
            {grpc_error, Details};
        Other ->
            io:format("Unexpected RPC response: ~p~n", [Other]),
            {error, {unexpected_response, Other}}
    end.
