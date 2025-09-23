-module(smq_auth_worker).
-behaviour(gen_server).
-export([
    start_link/0, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3
]).
-include_lib("smq_auth/include/smq_auth.hrl").

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    % You can do initialization here
    {ok, #{}}.

% Shell Command to test
% gen_server:call(smq_auth_worker, {client_authn, "user", "pass"}).
handle_call({client_authn, ClientID, ClientKey}, _From, State) ->
    Reply = smq_auth:client_authn(#smq_client_authn_request{
        client_id = ClientID,
        client_key = ClientKey
    }),
    {reply, Reply, State};
% gen_server:call(smq_auth_worker,{check_client_exists,"asdf"}).
handle_call({check_client_exists, ClientID}, _From, State) ->
    Reply = smq_auth:check_client_exists(ClientID),
    {reply, Reply, State};
% gen_server:call(smq_auth_worker, {client_authz, "domainid", "clientid",client,"channelid",publish}).
handle_call({client_authz, DomainID, ClientID, ClientType, ChannelID, Type}, _From, State) ->
    Reply = smq_auth:client_authz(#smq_client_authz_request{
        domain_id = DomainID,
        client_id = ClientID,
        client_type = ClientType,
        channel_id = ChannelID,
        client_key = "",
        type = Type
    }),
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
