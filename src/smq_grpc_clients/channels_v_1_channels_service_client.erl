%%%-------------------------------------------------------------------
%% @doc Client module for grpc service channels.v1.ChannelsService.
%% @end
%%%-------------------------------------------------------------------

%% this module was generated and should not be modified manually

-module(channels_v_1_channels_service_client).

-compile(export_all).
-compile(nowarn_export_all).

-include_lib("grpcbox/include/grpcbox.hrl").

-define(is_ctx(Ctx), is_tuple(Ctx) andalso element(1, Ctx) =:= ctx).

-define(SERVICE, 'channels.v1.ChannelsService').
-define(PROTO_MODULE, 'channels_pb').
-define(MARSHAL_FUN(T), fun(I) -> ?PROTO_MODULE:encode_msg(I, T) end).
-define(UNMARSHAL_FUN(T), fun(I) -> ?PROTO_MODULE:decode_msg(I, T) end).
-define(DEF(Input, Output, MessageType), #grpcbox_def{service=?SERVICE,
                                                      message_type=MessageType,
                                                      marshal_fun=?MARSHAL_FUN(Input),
                                                      unmarshal_fun=?UNMARSHAL_FUN(Output)}).

%% @doc Unary RPC
-spec authorize(channels_pb:authz_req()) ->
    {ok, channels_pb:authz_res(), grpcbox:metadata()} | grpcbox_stream:grpc_error_response() | {error, any()}.
authorize(Input) ->
    authorize(ctx:new(), Input, #{}).

-spec authorize(ctx:t() | channels_pb:authz_req(), channels_pb:authz_req() | grpcbox_client:options()) ->
    {ok, channels_pb:authz_res(), grpcbox:metadata()} | grpcbox_stream:grpc_error_response() | {error, any()}.
authorize(Ctx, Input) when ?is_ctx(Ctx) ->
    authorize(Ctx, Input, #{});
authorize(Input, Options) ->
    authorize(ctx:new(), Input, Options).

-spec authorize(ctx:t(), channels_pb:authz_req(), grpcbox_client:options()) ->
    {ok, channels_pb:authz_res(), grpcbox:metadata()} | grpcbox_stream:grpc_error_response() | {error, any()}.
authorize(Ctx, Input, Options) ->
    grpcbox_client:unary(Ctx, <<"/channels.v1.ChannelsService/Authorize">>, Input, ?DEF(authz_req, authz_res, <<"channels.v1.AuthzReq">>), Options).

%% @doc Unary RPC
-spec remove_client_connections(channels_pb:remove_client_connections_req()) ->
    {ok, channels_pb:remove_client_connections_res(), grpcbox:metadata()} | grpcbox_stream:grpc_error_response() | {error, any()}.
remove_client_connections(Input) ->
    remove_client_connections(ctx:new(), Input, #{}).

-spec remove_client_connections(ctx:t() | channels_pb:remove_client_connections_req(), channels_pb:remove_client_connections_req() | grpcbox_client:options()) ->
    {ok, channels_pb:remove_client_connections_res(), grpcbox:metadata()} | grpcbox_stream:grpc_error_response() | {error, any()}.
remove_client_connections(Ctx, Input) when ?is_ctx(Ctx) ->
    remove_client_connections(Ctx, Input, #{});
remove_client_connections(Input, Options) ->
    remove_client_connections(ctx:new(), Input, Options).

-spec remove_client_connections(ctx:t(), channels_pb:remove_client_connections_req(), grpcbox_client:options()) ->
    {ok, channels_pb:remove_client_connections_res(), grpcbox:metadata()} | grpcbox_stream:grpc_error_response() | {error, any()}.
remove_client_connections(Ctx, Input, Options) ->
    grpcbox_client:unary(Ctx, <<"/channels.v1.ChannelsService/RemoveClientConnections">>, Input, ?DEF(remove_client_connections_req, remove_client_connections_res, <<"channels.v1.RemoveClientConnectionsReq">>), Options).

%% @doc Unary RPC
-spec unset_parent_group_from_channels(channels_pb:unset_parent_group_from_channels_req()) ->
    {ok, channels_pb:unset_parent_group_from_channels_res(), grpcbox:metadata()} | grpcbox_stream:grpc_error_response() | {error, any()}.
unset_parent_group_from_channels(Input) ->
    unset_parent_group_from_channels(ctx:new(), Input, #{}).

-spec unset_parent_group_from_channels(ctx:t() | channels_pb:unset_parent_group_from_channels_req(), channels_pb:unset_parent_group_from_channels_req() | grpcbox_client:options()) ->
    {ok, channels_pb:unset_parent_group_from_channels_res(), grpcbox:metadata()} | grpcbox_stream:grpc_error_response() | {error, any()}.
unset_parent_group_from_channels(Ctx, Input) when ?is_ctx(Ctx) ->
    unset_parent_group_from_channels(Ctx, Input, #{});
unset_parent_group_from_channels(Input, Options) ->
    unset_parent_group_from_channels(ctx:new(), Input, Options).

-spec unset_parent_group_from_channels(ctx:t(), channels_pb:unset_parent_group_from_channels_req(), grpcbox_client:options()) ->
    {ok, channels_pb:unset_parent_group_from_channels_res(), grpcbox:metadata()} | grpcbox_stream:grpc_error_response() | {error, any()}.
unset_parent_group_from_channels(Ctx, Input, Options) ->
    grpcbox_client:unary(Ctx, <<"/channels.v1.ChannelsService/UnsetParentGroupFromChannels">>, Input, ?DEF(unset_parent_group_from_channels_req, unset_parent_group_from_channels_res, <<"channels.v1.UnsetParentGroupFromChannelsReq">>), Options).

%% @doc Unary RPC
-spec retrieve_entity(channels_pb:retrieve_entity_req()) ->
    {ok, channels_pb:retrieve_entity_res(), grpcbox:metadata()} | grpcbox_stream:grpc_error_response() | {error, any()}.
retrieve_entity(Input) ->
    retrieve_entity(ctx:new(), Input, #{}).

-spec retrieve_entity(ctx:t() | channels_pb:retrieve_entity_req(), channels_pb:retrieve_entity_req() | grpcbox_client:options()) ->
    {ok, channels_pb:retrieve_entity_res(), grpcbox:metadata()} | grpcbox_stream:grpc_error_response() | {error, any()}.
retrieve_entity(Ctx, Input) when ?is_ctx(Ctx) ->
    retrieve_entity(Ctx, Input, #{});
retrieve_entity(Input, Options) ->
    retrieve_entity(ctx:new(), Input, Options).

-spec retrieve_entity(ctx:t(), channels_pb:retrieve_entity_req(), grpcbox_client:options()) ->
    {ok, channels_pb:retrieve_entity_res(), grpcbox:metadata()} | grpcbox_stream:grpc_error_response() | {error, any()}.
retrieve_entity(Ctx, Input, Options) ->
    grpcbox_client:unary(Ctx, <<"/channels.v1.ChannelsService/RetrieveEntity">>, Input, ?DEF(retrieve_entity_req, retrieve_entity_res, <<"common.v1.RetrieveEntityReq">>), Options).

%% @doc Unary RPC
-spec retrieve_id_by_route(channels_pb:retrieve_id_by_route_req()) ->
    {ok, channels_pb:retrieve_entity_res(), grpcbox:metadata()} | grpcbox_stream:grpc_error_response() | {error, any()}.
retrieve_id_by_route(Input) ->
    retrieve_id_by_route(ctx:new(), Input, #{}).

-spec retrieve_id_by_route(ctx:t() | channels_pb:retrieve_id_by_route_req(), channels_pb:retrieve_id_by_route_req() | grpcbox_client:options()) ->
    {ok, channels_pb:retrieve_entity_res(), grpcbox:metadata()} | grpcbox_stream:grpc_error_response() | {error, any()}.
retrieve_id_by_route(Ctx, Input) when ?is_ctx(Ctx) ->
    retrieve_id_by_route(Ctx, Input, #{});
retrieve_id_by_route(Input, Options) ->
    retrieve_id_by_route(ctx:new(), Input, Options).

-spec retrieve_id_by_route(ctx:t(), channels_pb:retrieve_id_by_route_req(), grpcbox_client:options()) ->
    {ok, channels_pb:retrieve_entity_res(), grpcbox:metadata()} | grpcbox_stream:grpc_error_response() | {error, any()}.
retrieve_id_by_route(Ctx, Input, Options) ->
    grpcbox_client:unary(Ctx, <<"/channels.v1.ChannelsService/RetrieveIDByRoute">>, Input, ?DEF(retrieve_id_by_route_req, retrieve_entity_res, <<"common.v1.RetrieveIDByRouteReq">>), Options).

