-module(smq_auth_app).
-doc "smq_auth Application.".

-include_lib("smq_auth/include/smq_auth.hrl").
-include_lib("kernel/include/logger.hrl").

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    SmqGrpcConfig = #smq_grpc_config{
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
    },
    logger:update_process_metadata(#{domain => [smq_auth]}),

    application:ensure_all_started(logger),

    smq_auth:init_smq_grpc(SmqGrpcConfig),

    smq_auth_sup:start_link().

stop(_State) ->
    smq_auth:close_smq_grpc(),
    ok.
