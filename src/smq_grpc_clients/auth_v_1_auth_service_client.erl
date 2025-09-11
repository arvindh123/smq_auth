%%%-------------------------------------------------------------------
%% @doc Client module for grpc service auth.v1.AuthService.
%% @end
%%%-------------------------------------------------------------------

%% this module was generated and should not be modified manually

-module(auth_v_1_auth_service_client).

-compile(export_all).
-compile(nowarn_export_all).

-include_lib("grpcbox/include/grpcbox.hrl").

-define(is_ctx(Ctx), is_tuple(Ctx) andalso element(1, Ctx) =:= ctx).

-define(SERVICE, 'auth.v1.AuthService').
-define(PROTO_MODULE, 'auth_pb').
-define(MARSHAL_FUN(T), fun(I) -> ?PROTO_MODULE:encode_msg(I, T) end).
-define(UNMARSHAL_FUN(T), fun(I) -> ?PROTO_MODULE:decode_msg(I, T) end).
-define(DEF(Input, Output, MessageType), #grpcbox_def{service=?SERVICE,
                                                      message_type=MessageType,
                                                      marshal_fun=?MARSHAL_FUN(Input),
                                                      unmarshal_fun=?UNMARSHAL_FUN(Output)}).

%% @doc Unary RPC
-spec authorize(auth_pb:auth_z_req()) ->
    {ok, auth_pb:auth_z_res(), grpcbox:metadata()} | grpcbox_stream:grpc_error_response() | {error, any()}.
authorize(Input) ->
    authorize(ctx:new(), Input, #{}).

-spec authorize(ctx:t() | auth_pb:auth_z_req(), auth_pb:auth_z_req() | grpcbox_client:options()) ->
    {ok, auth_pb:auth_z_res(), grpcbox:metadata()} | grpcbox_stream:grpc_error_response() | {error, any()}.
authorize(Ctx, Input) when ?is_ctx(Ctx) ->
    authorize(Ctx, Input, #{});
authorize(Input, Options) ->
    authorize(ctx:new(), Input, Options).

-spec authorize(ctx:t(), auth_pb:auth_z_req(), grpcbox_client:options()) ->
    {ok, auth_pb:auth_z_res(), grpcbox:metadata()} | grpcbox_stream:grpc_error_response() | {error, any()}.
authorize(Ctx, Input, Options) ->
    grpcbox_client:unary(Ctx, <<"/auth.v1.AuthService/Authorize">>, Input, ?DEF(auth_z_req, auth_z_res, <<"auth.v1.AuthZReq">>), Options).

%% @doc Unary RPC
-spec authorize_pat(auth_pb:auth_z_pat_req()) ->
    {ok, auth_pb:auth_z_res(), grpcbox:metadata()} | grpcbox_stream:grpc_error_response() | {error, any()}.
authorize_pat(Input) ->
    authorize_pat(ctx:new(), Input, #{}).

-spec authorize_pat(ctx:t() | auth_pb:auth_z_pat_req(), auth_pb:auth_z_pat_req() | grpcbox_client:options()) ->
    {ok, auth_pb:auth_z_res(), grpcbox:metadata()} | grpcbox_stream:grpc_error_response() | {error, any()}.
authorize_pat(Ctx, Input) when ?is_ctx(Ctx) ->
    authorize_pat(Ctx, Input, #{});
authorize_pat(Input, Options) ->
    authorize_pat(ctx:new(), Input, Options).

-spec authorize_pat(ctx:t(), auth_pb:auth_z_pat_req(), grpcbox_client:options()) ->
    {ok, auth_pb:auth_z_res(), grpcbox:metadata()} | grpcbox_stream:grpc_error_response() | {error, any()}.
authorize_pat(Ctx, Input, Options) ->
    grpcbox_client:unary(Ctx, <<"/auth.v1.AuthService/AuthorizePAT">>, Input, ?DEF(auth_z_pat_req, auth_z_res, <<"auth.v1.AuthZPatReq">>), Options).

%% @doc Unary RPC
-spec authenticate(auth_pb:auth_n_req()) ->
    {ok, auth_pb:auth_n_res(), grpcbox:metadata()} | grpcbox_stream:grpc_error_response() | {error, any()}.
authenticate(Input) ->
    authenticate(ctx:new(), Input, #{}).

-spec authenticate(ctx:t() | auth_pb:auth_n_req(), auth_pb:auth_n_req() | grpcbox_client:options()) ->
    {ok, auth_pb:auth_n_res(), grpcbox:metadata()} | grpcbox_stream:grpc_error_response() | {error, any()}.
authenticate(Ctx, Input) when ?is_ctx(Ctx) ->
    authenticate(Ctx, Input, #{});
authenticate(Input, Options) ->
    authenticate(ctx:new(), Input, Options).

-spec authenticate(ctx:t(), auth_pb:auth_n_req(), grpcbox_client:options()) ->
    {ok, auth_pb:auth_n_res(), grpcbox:metadata()} | grpcbox_stream:grpc_error_response() | {error, any()}.
authenticate(Ctx, Input, Options) ->
    grpcbox_client:unary(Ctx, <<"/auth.v1.AuthService/Authenticate">>, Input, ?DEF(auth_n_req, auth_n_res, <<"auth.v1.AuthNReq">>), Options).

%% @doc Unary RPC
-spec authenticate_pat(auth_pb:auth_n_req()) ->
    {ok, auth_pb:auth_n_res(), grpcbox:metadata()} | grpcbox_stream:grpc_error_response() | {error, any()}.
authenticate_pat(Input) ->
    authenticate_pat(ctx:new(), Input, #{}).

-spec authenticate_pat(ctx:t() | auth_pb:auth_n_req(), auth_pb:auth_n_req() | grpcbox_client:options()) ->
    {ok, auth_pb:auth_n_res(), grpcbox:metadata()} | grpcbox_stream:grpc_error_response() | {error, any()}.
authenticate_pat(Ctx, Input) when ?is_ctx(Ctx) ->
    authenticate_pat(Ctx, Input, #{});
authenticate_pat(Input, Options) ->
    authenticate_pat(ctx:new(), Input, Options).

-spec authenticate_pat(ctx:t(), auth_pb:auth_n_req(), grpcbox_client:options()) ->
    {ok, auth_pb:auth_n_res(), grpcbox:metadata()} | grpcbox_stream:grpc_error_response() | {error, any()}.
authenticate_pat(Ctx, Input, Options) ->
    grpcbox_client:unary(Ctx, <<"/auth.v1.AuthService/AuthenticatePAT">>, Input, ?DEF(auth_n_req, auth_n_res, <<"auth.v1.AuthNReq">>), Options).

