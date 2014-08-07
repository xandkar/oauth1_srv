-module(oauth1_srv_app).

-behaviour(application).

-export(
    [ start/2
    , stop/1

    , start_dev/0
    ]).


-define(cfg, oauth1_srv_cfg).


%% ============================================================================
%% Callbacks
%% ============================================================================

start(_StartType, _StartArgs) ->
    ok = start_server(),
    oauth1_srv_sup:start_link().

stop(_State) ->
    ok.


%% ============================================================================
%% API
%% ============================================================================

start_dev() ->
    App = oauth1_srv,
    ok = app_start_deps(App),
    ok = app_start(App).


%% ============================================================================
%% Helpers (Cowboy setup)
%% ============================================================================

-spec start_server() ->
    ok.
start_server() ->
    Routes   = routes(),
    Dispatch = cowboy_router:compile([{'_', Routes}]),
    Middlewares =
        [ cowboy_router
        , cowboy_handler
        | ?cfg:get(middlewares)
        ],
    ListenerName = ?cfg:get(listener_name),
    ListenPort   = ?cfg:get(listen_port),
    ListenIP     = ?cfg:get(listen_ip),
    TransOpts =
        [ {port , ListenPort}
        , {ip   , ListenIP}
        | opts_trans_cert()
        ],
    NumberOfAcceptors = ?cfg:get(number_of_acceptors),
    ProtoOpts =
        [ {env         , [{dispatch, Dispatch}]}
        , {middlewares , Middlewares}
        ],
    Res = cowboy:start_https( ListenerName
                                , NumberOfAcceptors
                                , TransOpts
                                , ProtoOpts
                                ),
    io:format("cowboy:start_https(...) -> ~p~n", [Res]),
    {ok, _} = Res,
    ok.

-spec opts_trans_cert() ->
    [{atom(), binary() | string()}].
opts_trans_cert() ->
    DirSSL     = ?cfg:get_ssl_dir(),
    FileCertCA  = filename:join(DirSSL, ?cfg:get(file_cert_ca)),
    FileCert    = filename:join(DirSSL, ?cfg:get(file_cert)),
    FileKey     = filename:join(DirSSL, ?cfg:get(file_key)),
    Password    = ?cfg:get(password),
    [ {cacertfile , FileCertCA}
    , {certfile   , FileCert}
    , {keyfile    , FileKey}
    , {password   , Password}
    ].

-spec routes() ->
    [ { Path          :: binary()
      , HandlerModule :: atom()
      , Options       :: [term()]
      }
    ].
routes() ->
    [].


%% ============================================================================
%% Helpers (starting app deps during dev)
%% ============================================================================

app_start_deps(App) ->
    Deps    = app_find_deps(App),
    DepsRev = lists:reverse(Deps),
    ok = lists:foreach(fun app_start/1, DepsRev).

app_start(App) ->
    case application:start(App)
    of  ok                            -> ok
    ;   {error, {already_started, _}} -> ok
    end.

-spec app_find_deps(atom()) ->
    [atom()].
app_find_deps(App) ->
    ok = app_load(App),
    {ok, Deps1} = application:get_key(App, applications),
    Deps2 = Deps1 ++ [app_find_deps(D) || D <- Deps1],
    Deps3 = lists:flatten(Deps2),
    Deps4 = hope_list:unique_preserve_order(Deps3),
    Deps4.

app_load(App) ->
    Ok = ok,
    case application:load(App)
    of  ok                             -> Ok
    ;   {error, {already_loaded, App}} -> Ok
    ;   {error, _}=Error               -> Error
    end.
