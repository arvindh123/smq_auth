-module(smq_auth_worker).
-behaviour(gen_server).
-export([
    start_link/0, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3
]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    % You can do initialization here
    {ok, #{}}.

% Shell Command to test
% gen_server:call(smq_auth_worker, {client_authn, "user", "pass"}).
handle_call({client_authn, ClientID, ClientKey}, _From, State) ->
    Reply = smq_auth:client_authn(ClientID, ClientKey),
    {reply, Reply, State};
% Shell Command to test
% gen_server:call(smq_auth_worker, {client_authz, "domainid", "clientid",client,"channelid",publish}).
handle_call({client_authz, DomainID, ClientID, ClientType, ChannelID, Type}, _From, State) ->
    Reply = smq_auth:client_authz(DomainID, ClientID, ClientType, ChannelID, Type),
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
