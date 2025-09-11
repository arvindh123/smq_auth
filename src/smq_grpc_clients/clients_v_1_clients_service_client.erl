%%%-------------------------------------------------------------------
%% @doc Client module for grpc service clients.v1.ClientsService.
%% @end
%%%-------------------------------------------------------------------

%% this module was generated and should not be modified manually

-module(clients_v_1_clients_service_client).

-compile(export_all).
-compile(nowarn_export_all).

-include_lib("grpcbox/include/grpcbox.hrl").

-define(is_ctx(Ctx), is_tuple(Ctx) andalso element(1, Ctx) =:= ctx).

-define(SERVICE, 'clients.v1.ClientsService').
-define(PROTO_MODULE, 'clients_pb').
-define(MARSHAL_FUN(T), fun(I) -> ?PROTO_MODULE:encode_msg(I, T) end).
-define(UNMARSHAL_FUN(T), fun(I) -> ?PROTO_MODULE:decode_msg(I, T) end).
-define(DEF(Input, Output, MessageType), #grpcbox_def{service=?SERVICE,
                                                      message_type=MessageType,
                                                      marshal_fun=?MARSHAL_FUN(Input),
                                                      unmarshal_fun=?UNMARSHAL_FUN(Output)}).

%% @doc Unary RPC
-spec authenticate(clients_pb:authn_req()) ->
    {ok, clients_pb:authn_res(), grpcbox:metadata()} | grpcbox_stream:grpc_error_response() | {error, any()}.
authenticate(Input) ->
    authenticate(ctx:new(), Input, #{}).

-spec authenticate(ctx:t() | clients_pb:authn_req(), clients_pb:authn_req() | grpcbox_client:options()) ->
    {ok, clients_pb:authn_res(), grpcbox:metadata()} | grpcbox_stream:grpc_error_response() | {error, any()}.
authenticate(Ctx, Input) when ?is_ctx(Ctx) ->
    authenticate(Ctx, Input, #{});
authenticate(Input, Options) ->
    authenticate(ctx:new(), Input, Options).

-spec authenticate(ctx:t(), clients_pb:authn_req(), grpcbox_client:options()) ->
    {ok, clients_pb:authn_res(), grpcbox:metadata()} | grpcbox_stream:grpc_error_response() | {error, any()}.
authenticate(Ctx, Input, Options) ->
    grpcbox_client:unary(Ctx, <<"/clients.v1.ClientsService/Authenticate">>, Input, ?DEF(authn_req, authn_res, <<"clients.v1.AuthnReq">>), Options).

%% @doc Unary RPC
-spec retrieve_entity(clients_pb:retrieve_entity_req()) ->
    {ok, clients_pb:retrieve_entity_res(), grpcbox:metadata()} | grpcbox_stream:grpc_error_response() | {error, any()}.
retrieve_entity(Input) ->
    retrieve_entity(ctx:new(), Input, #{}).

-spec retrieve_entity(ctx:t() | clients_pb:retrieve_entity_req(), clients_pb:retrieve_entity_req() | grpcbox_client:options()) ->
    {ok, clients_pb:retrieve_entity_res(), grpcbox:metadata()} | grpcbox_stream:grpc_error_response() | {error, any()}.
retrieve_entity(Ctx, Input) when ?is_ctx(Ctx) ->
    retrieve_entity(Ctx, Input, #{});
retrieve_entity(Input, Options) ->
    retrieve_entity(ctx:new(), Input, Options).

-spec retrieve_entity(ctx:t(), clients_pb:retrieve_entity_req(), grpcbox_client:options()) ->
    {ok, clients_pb:retrieve_entity_res(), grpcbox:metadata()} | grpcbox_stream:grpc_error_response() | {error, any()}.
retrieve_entity(Ctx, Input, Options) ->
    grpcbox_client:unary(Ctx, <<"/clients.v1.ClientsService/RetrieveEntity">>, Input, ?DEF(retrieve_entity_req, retrieve_entity_res, <<"common.v1.RetrieveEntityReq">>), Options).

%% @doc Unary RPC
-spec retrieve_entities(clients_pb:retrieve_entities_req()) ->
    {ok, clients_pb:retrieve_entities_res(), grpcbox:metadata()} | grpcbox_stream:grpc_error_response() | {error, any()}.
retrieve_entities(Input) ->
    retrieve_entities(ctx:new(), Input, #{}).

-spec retrieve_entities(ctx:t() | clients_pb:retrieve_entities_req(), clients_pb:retrieve_entities_req() | grpcbox_client:options()) ->
    {ok, clients_pb:retrieve_entities_res(), grpcbox:metadata()} | grpcbox_stream:grpc_error_response() | {error, any()}.
retrieve_entities(Ctx, Input) when ?is_ctx(Ctx) ->
    retrieve_entities(Ctx, Input, #{});
retrieve_entities(Input, Options) ->
    retrieve_entities(ctx:new(), Input, Options).

-spec retrieve_entities(ctx:t(), clients_pb:retrieve_entities_req(), grpcbox_client:options()) ->
    {ok, clients_pb:retrieve_entities_res(), grpcbox:metadata()} | grpcbox_stream:grpc_error_response() | {error, any()}.
retrieve_entities(Ctx, Input, Options) ->
    grpcbox_client:unary(Ctx, <<"/clients.v1.ClientsService/RetrieveEntities">>, Input, ?DEF(retrieve_entities_req, retrieve_entities_res, <<"common.v1.RetrieveEntitiesReq">>), Options).

%% @doc Unary RPC
-spec add_connections(clients_pb:add_connections_req()) ->
    {ok, clients_pb:add_connections_res(), grpcbox:metadata()} | grpcbox_stream:grpc_error_response() | {error, any()}.
add_connections(Input) ->
    add_connections(ctx:new(), Input, #{}).

-spec add_connections(ctx:t() | clients_pb:add_connections_req(), clients_pb:add_connections_req() | grpcbox_client:options()) ->
    {ok, clients_pb:add_connections_res(), grpcbox:metadata()} | grpcbox_stream:grpc_error_response() | {error, any()}.
add_connections(Ctx, Input) when ?is_ctx(Ctx) ->
    add_connections(Ctx, Input, #{});
add_connections(Input, Options) ->
    add_connections(ctx:new(), Input, Options).

-spec add_connections(ctx:t(), clients_pb:add_connections_req(), grpcbox_client:options()) ->
    {ok, clients_pb:add_connections_res(), grpcbox:metadata()} | grpcbox_stream:grpc_error_response() | {error, any()}.
add_connections(Ctx, Input, Options) ->
    grpcbox_client:unary(Ctx, <<"/clients.v1.ClientsService/AddConnections">>, Input, ?DEF(add_connections_req, add_connections_res, <<"common.v1.AddConnectionsReq">>), Options).

%% @doc Unary RPC
-spec remove_connections(clients_pb:remove_connections_req()) ->
    {ok, clients_pb:remove_connections_res(), grpcbox:metadata()} | grpcbox_stream:grpc_error_response() | {error, any()}.
remove_connections(Input) ->
    remove_connections(ctx:new(), Input, #{}).

-spec remove_connections(ctx:t() | clients_pb:remove_connections_req(), clients_pb:remove_connections_req() | grpcbox_client:options()) ->
    {ok, clients_pb:remove_connections_res(), grpcbox:metadata()} | grpcbox_stream:grpc_error_response() | {error, any()}.
remove_connections(Ctx, Input) when ?is_ctx(Ctx) ->
    remove_connections(Ctx, Input, #{});
remove_connections(Input, Options) ->
    remove_connections(ctx:new(), Input, Options).

-spec remove_connections(ctx:t(), clients_pb:remove_connections_req(), grpcbox_client:options()) ->
    {ok, clients_pb:remove_connections_res(), grpcbox:metadata()} | grpcbox_stream:grpc_error_response() | {error, any()}.
remove_connections(Ctx, Input, Options) ->
    grpcbox_client:unary(Ctx, <<"/clients.v1.ClientsService/RemoveConnections">>, Input, ?DEF(remove_connections_req, remove_connections_res, <<"common.v1.RemoveConnectionsReq">>), Options).

%% @doc Unary RPC
-spec remove_channel_connections(clients_pb:remove_channel_connections_req()) ->
    {ok, clients_pb:remove_channel_connections_res(), grpcbox:metadata()} | grpcbox_stream:grpc_error_response() | {error, any()}.
remove_channel_connections(Input) ->
    remove_channel_connections(ctx:new(), Input, #{}).

-spec remove_channel_connections(ctx:t() | clients_pb:remove_channel_connections_req(), clients_pb:remove_channel_connections_req() | grpcbox_client:options()) ->
    {ok, clients_pb:remove_channel_connections_res(), grpcbox:metadata()} | grpcbox_stream:grpc_error_response() | {error, any()}.
remove_channel_connections(Ctx, Input) when ?is_ctx(Ctx) ->
    remove_channel_connections(Ctx, Input, #{});
remove_channel_connections(Input, Options) ->
    remove_channel_connections(ctx:new(), Input, Options).

-spec remove_channel_connections(ctx:t(), clients_pb:remove_channel_connections_req(), grpcbox_client:options()) ->
    {ok, clients_pb:remove_channel_connections_res(), grpcbox:metadata()} | grpcbox_stream:grpc_error_response() | {error, any()}.
remove_channel_connections(Ctx, Input, Options) ->
    grpcbox_client:unary(Ctx, <<"/clients.v1.ClientsService/RemoveChannelConnections">>, Input, ?DEF(remove_channel_connections_req, remove_channel_connections_res, <<"clients.v1.RemoveChannelConnectionsReq">>), Options).

%% @doc Unary RPC
-spec unset_parent_group_from_client(clients_pb:unset_parent_group_from_client_req()) ->
    {ok, clients_pb:unset_parent_group_from_client_res(), grpcbox:metadata()} | grpcbox_stream:grpc_error_response() | {error, any()}.
unset_parent_group_from_client(Input) ->
    unset_parent_group_from_client(ctx:new(), Input, #{}).

-spec unset_parent_group_from_client(ctx:t() | clients_pb:unset_parent_group_from_client_req(), clients_pb:unset_parent_group_from_client_req() | grpcbox_client:options()) ->
    {ok, clients_pb:unset_parent_group_from_client_res(), grpcbox:metadata()} | grpcbox_stream:grpc_error_response() | {error, any()}.
unset_parent_group_from_client(Ctx, Input) when ?is_ctx(Ctx) ->
    unset_parent_group_from_client(Ctx, Input, #{});
unset_parent_group_from_client(Input, Options) ->
    unset_parent_group_from_client(ctx:new(), Input, Options).

-spec unset_parent_group_from_client(ctx:t(), clients_pb:unset_parent_group_from_client_req(), grpcbox_client:options()) ->
    {ok, clients_pb:unset_parent_group_from_client_res(), grpcbox:metadata()} | grpcbox_stream:grpc_error_response() | {error, any()}.
unset_parent_group_from_client(Ctx, Input, Options) ->
    grpcbox_client:unary(Ctx, <<"/clients.v1.ClientsService/UnsetParentGroupFromClient">>, Input, ?DEF(unset_parent_group_from_client_req, unset_parent_group_from_client_res, <<"clients.v1.UnsetParentGroupFromClientReq">>), Options).

