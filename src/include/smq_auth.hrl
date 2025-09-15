%%%-------------------------------------------------------------------
%%% @doc Header file for SuperMQ Authentication and Authorization
%%% @author SuperMQ Team
%%%-------------------------------------------------------------------

-ifndef(SMQ_AUTH_HRL).
-define(SMQ_AUTH_HRL, true).

-record(smq_client_authn_request, {
    client_id :: string(),
    client_key :: string()
}).

-opaque smq_client_authn_request() :: #smq_client_authn_request{}.

-record(smq_client_authz_request, {
    domain_id :: string(),
    channel_id :: string(),
    client_id :: string(),
    client_type :: client | user,
    client_key :: string(),
    type :: publish | subscribe
}).

%% Authorization types
-opaque smq_client_authz_request() :: #smq_client_authz_request{}.

%% Domain and client identifiers

-type smq_client_authn_result() ::
    {ok, clients_pb:authn_res()}
    | {error, {binary(), binary()}}
    | {error, term()}
    | {grpc_error, term()}
    | {error, {unexpected_response, term()}}.

%% Authorization result type
-type smq_client_authz_result() ::
    {ok}
    | {error, {unauthorized}}
    | {error, {binary(), binary()}}
    | {error, term()}
    | {grpc_error, term()}
    | {error, {unexpected_response, term()}}.

%% Authentication result type (exported for external use)
-export_type([
    smq_client_authn_request/0,
    smq_client_authn_result/0,
    smq_client_authz_request/0,
    smq_client_authz_result/0
]).

-endif.
