-module(smq_auth_app).
-doc "smq_auth Application.".

-include_lib("smq_auth/include/smq_auth.hrl").

-behaviour(application).

-export([start/2, stop/1]).

-define(DEFAULT_SMQ_GRPC_SCHEMA_PATH_IN_PRIV, "schema/smq.schema").

start(_StartType, _StartArgs) ->
    logger:update_process_metadata(#{domain => [smq_auth]}),

    application:ensure_all_started(logger),

    load_config(),

    SmqGrpcConfig =
        case application:get_env(smq_grpc, config, undefined) of
            undefined ->
                logger:warning(
                    "smq_grpc.config is not found in env, using default smq_grpc.config"
                ),
                smq_auth:default_grpc_config();
            SmqGrpcConfigMap ->
                logger:info("SmqGrpcConfigMap ~p~n", [SmqGrpcConfigMap]),
                case smq_auth:convert_grpc_config(SmqGrpcConfigMap) of
                    {error, Reason} ->
                        logger:warning(
                            "failed to smq_grpc.config to smq_grpc_config record , using default smq_grpc.config, error: ~p",
                            [Reason]
                        ),
                        smq_auth:default_grpc_config();
                    ConvertedConfig ->
                        logger:notice(
                            "loaded smq_grpc.config from conf: ~p",
                            [ConvertedConfig]
                        ),
                        ConvertedConfig
                end
        end,
    smq_auth:init_smq_grpc(SmqGrpcConfig),

    smq_auth_sup:start_link().

stop(_State) ->
    smq_auth:close_smq_grpc(),
    ok.

load_config() ->
    %% Read conf file as proplist
    ConfigFile = get_config_file_path(),

    %% Check if config file exists
    case filelib:is_regular(ConfigFile) of
        true ->
            %% Load schema
            SchemaFile = get_schema_file_path(),
            SchemaResult = cuttlefish_schema:files([SchemaFile]),

            %% Handle schema loading errors
            case SchemaResult of
                {'errorlist', ErrorList} ->
                    logger:error("Schema loading failed with errors: ~p~n", [ErrorList]),
                    ok;
                Schema ->
                    %% Schema is now known to be a valid cuttlefish_schema:schema()
                    %% Parse configuration file
                    ConfResult = cuttlefish_conf:file(ConfigFile),
                    case ConfResult of
                        {errorlist, ErrorList} ->
                            logger:error("Config loading failed with errors: ~p~nErrors: ~p~n", [
                                ErrorList
                            ]),
                            ok;
                        Conf ->
                            %% Conf is now known to be a valid cuttlefish_conf:conf()
                            MapResult = cuttlefish_generator:map(Schema, Conf),
                            case MapResult of
                                {error, Reason, ErrorList} ->
                                    logger:error("Config generation failed: ~p~nErrors: ~p~n", [
                                        Reason, ErrorList
                                    ]),
                                    ok;
                                AppEnv when is_list(AppEnv) ->
                                    %% Set application environment variables
                                    lists:foreach(
                                        fun({App, Settings}) ->
                                            lists:foreach(
                                                fun({Key, Value}) ->
                                                    logger:debug("App: ~p, Key: ~p, Value: ~p", [
                                                        App, Key, Value
                                                    ]),
                                                    application:set_env(App, Key, Value)
                                                end,
                                                Settings
                                            )
                                        end,
                                        AppEnv
                                    ),
                                    ok;
                                Other ->
                                    logger:error(
                                        "Config generation failed unexpected result: ~p~n", [Other]
                                    ),
                                    ok
                            end
                    end
            end;
        false ->
            logger:warning("Config file not found: ~s, using defaults~n", [ConfigFile]),
            ok
    end.

get_schema_file_path() ->
    %% Get path from app env
    SmqSchemaFilePathInPriv =
        case application:get_env(?APPNAME, smq_grpc_schema_file) of
            {ok, File} ->
                File;
            undefined ->
                %% fallback to default path
                logger:warning(
                    "Warning: smq_grpc_schema_file not set in app, using default ~p~n", [
                        ?DEFAULT_SMQ_GRPC_SCHEMA_PATH_IN_PRIV
                    ]
                ),
                ?DEFAULT_SMQ_GRPC_SCHEMA_PATH_IN_PRIV
        end,
    logger:info("SmqSchemaFilePathInPriv ~p", [SmqSchemaFilePathInPriv]),

    PrivDir = code:priv_dir(?APPNAME),
    logger:info("PrivDir ~p", [PrivDir]),

    SmqSchemaFilePath = filename:join([PrivDir, SmqSchemaFilePathInPriv]),
    logger:info("SmqSchemaFilePath ~p", [SmqSchemaFilePath]),

    SmqSchemaFilePath.

%% Get configuration file path
get_config_file_path() ->
    case application:get_env(?APPNAME, config_file) of
        {ok, Path} ->
            Path;
        undefined ->
            %% Default paths to check
            Candidates = [
                "smq.conf",
                "priv/smq.conf",
                "priv/config/smq.conf",
                "/etc/smq/smq.conf",
                filename:join([code:priv_dir(?APPNAME), "smq.conf"])
            ],
            find_config_file(Candidates)
    end.

%% Find the first existing config file
find_config_file([]) ->
    % fallback
    "smq.conf";
find_config_file([Path | Rest]) ->
    case filelib:is_regular(Path) of
        true -> Path;
        false -> find_config_file(Rest)
    end.
