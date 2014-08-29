-module(oauth1_srv_SUITE).

-include_lib("oauth1_core/include/oauth1_parameter_names.hrl").
-include_lib("oauth1_core/include/oauth1_signature.hrl").

%% Callbacks
-export(
    [ all/0
    , groups/0
    , init_per_suite/1
    , end_per_suite/1
    ]).

%% Tests
-export(
    [ t_register_ok/1
    , t_initiate_ok/1
    , t_authorize/1
    , t_token_ok/1
    ]).


-define(GROUP, oauth1_srv).


-record(state,
    { tmp_token_id     = none :: hope_option:t(binary())
    , tmp_token_secret = none :: hope_option:t(binary())
    , verifier         = none :: hope_option:t(binary())
    , token_id         = none :: hope_option:t(binary())
    , token_secret     = none :: hope_option:t(binary())
    , client_id        = none :: hope_option:t(binary())
    , client_secret    = none :: hope_option:t(binary())
    }).


%%=============================================================================
%% Callbacks
%%=============================================================================

all() ->
    [{group, ?GROUP}].

groups() ->
    Tests =
        [ t_register_ok
        , t_initiate_ok
        , t_authorize
        , t_token_ok
        ],
    Properties = [],
    [ {?GROUP, Properties, Tests}
    ].

init_per_suite(Cfg) ->
    ok = oauth1_srv_app:start_dev(),
    ok = application:start(inets),
    Cfg.

end_per_suite(_Cfg) ->
    application:stop(oauth1_srv).


%%=============================================================================
%% Tests
%%=============================================================================

t_register_ok(_Cfg) ->
    ReqURL = "https://localhost:8443/register",
    ReqHeaders = [],
    ReqMethod = get,
    Request = {ReqURL, ReqHeaders},
    ReqOptions = [],
    ReqHTTPOptions = [{ssl, [{verify, verify_none}]}],
    {ok, Response} = httpc:request(ReqMethod, Request, ReqHTTPOptions, ReqOptions),
    {Status, Headers, BodyRaw} = Response,
    {_HttpVsn, StatusCode, _StatusMsg} = Status,
    ct:log("Status: ~p", [Status]),
    ct:log("Headers: ~p", [Headers]),
    ct:log("BodyRaw: ~s", [BodyRaw]),
    ParseJSON = hope_result:lift_exn(fun jsx:decode/1),
    BodyParseResult = ParseJSON(list_to_binary(BodyRaw)),
    ct:log("BodyParseResult: ~p", [BodyParseResult]),
    {ok, BodyParsed} = BodyParseResult,
    {some, _} = hope_kv_list:get(BodyParsed, <<"id">>),
    {some, _} = hope_kv_list:get(BodyParsed, <<"secret">>),
    200 = StatusCode,
    ok.

t_initiate_ok(_Cfg) ->
    {ClientID, ClientSecret} = register_client(),
    Realm           = <<"oblivion">>,
    ReqURL          = "https://localhost:8443/initiate",
    Callback        = <<"https://client/ready">>,
    Timestamp       = <<"123456789">>,
    TimestampInt    =    123456789,
    {ok, CallbackURI} = oauth1_uri:of_bin(Callback),
    {ok, ResourceURI} = oauth1_uri:of_bin(list_to_binary(ReqURL)),
    Resource        = oauth1_resource:cons(Realm, ResourceURI),
    Nonce           = <<"foofoobahbah">>,
    SigMeth         = 'HMAC_SHA1',
    SigMethBin      = oauth1_signature:method_to_bin(SigMeth),
    SigArgs = #oauth1_signature_args_cons
        { method               = SigMeth
        , http_req_method      = <<"POST">>
        , http_req_host        = <<"localhost">>
        , resource             = Resource
        , consumer_key         = {client, ClientID}
        , timestamp            = TimestampInt
        , nonce                = Nonce
        , client_shared_secret = {client, ClientSecret}
        , token                = none
        , verifier             = none
        , callback             = {some, CallbackURI}
        , version              = none
        },
    Sig       = oauth1_signature:cons(SigArgs),
    SigDigest = oauth1_signature:get_digest(Sig),
    Params =
        [ {?PARAM_REALM            , Realm}
        , {?PARAM_CONSUMER_KEY     , ClientID}
        , {?PARAM_SIGNATURE        , SigDigest}
        , {?PARAM_SIGNATURE_METHOD , SigMethBin}
        , {?PARAM_TIMESTAMP        , Timestamp}
        , {?PARAM_NONCE            , Nonce}
        , {?PARAM_CALLBACK         , Callback}
        ],
    AuthorizationBin = oauth1_parameters:to_http_header_authorization(Params),
    Authorization    = binary_to_list(AuthorizationBin),
    ReqURL = "https://localhost:8443/initiate",
    ReqHeaders =
        [ {"Authorization", Authorization}
        ],
    ReqMethod = post,
    ReqContentType = "application/x-www-form-urlencoded",
    ReqBody = "",
    Request = {ReqURL, ReqHeaders, ReqContentType, ReqBody},
    ReqOptions = [],
    ReqHTTPOptions = [{ssl, [{verify, verify_none}]}],
    {ok, Response} = httpc:request(ReqMethod, Request, ReqHTTPOptions, ReqOptions),
    {Status, Headers, BodyRaw} = Response,
    {_HttpVsn, StatusCode, _StatusMsg} = Status,
    ct:log("Status: ~p", [Status]),
    ct:log("Headers: ~p", [Headers]),
    ct:log("BodyRaw: ~s", [BodyRaw]),
    ParseQS = hope_result:lift_exn(fun cow_qs:parse_qs/1),
    BodyParseResult = ParseQS(list_to_binary(BodyRaw)),
    ct:log("BodyParseResult: ~p", [BodyParseResult]),
    {ok, BodyParsed} = BodyParseResult,
    BP = BodyParsed,
    {some, TmpTokenID}     = hope_kv_list:get(BP, <<"oauth_token">>),
    {some, TmpTokenSecret} = hope_kv_list:get(BP, <<"oauth_token_secret">>),
    200 = StatusCode,
    State = #state
        { tmp_token_id     = {some, TmpTokenID}
        , tmp_token_secret = {some, TmpTokenSecret}
        , client_id        = {some, ClientID}
        , client_secret    = {some, ClientSecret}
        },
    state_return(State).

t_authorize(Cfg) ->
    State1 = state_get(Cfg, t_initiate_ok),
    #state
        { tmp_token_id     = {some, TmpTokenID}
        , tmp_token_secret = {some, _TmpTokenSecret}
        } = State1,
    TmpTokenIDStr = binary_to_list(TmpTokenID),
    ReqURL = "https://localhost:8443/authorize?oauth_token=" ++ TmpTokenIDStr,
    ReqHeaders = [],
    ReqMethod = get,
    Request = {ReqURL, ReqHeaders},
    ReqOptions = [],
    ReqHTTPOptions =
        [ {ssl          , [{verify, verify_none}]}
        , {autoredirect , false}
        ],
    {ok, Response} = httpc:request(ReqMethod, Request, ReqHTTPOptions, ReqOptions),
    {Status, Headers, BodyRaw} = Response,
    {_HttpVsn, StatusCode, _StatusMsg} = Status,
    ct:log("Status: ~p", [Status]),
    ct:log("Headers: ~p", [Headers]),
    ct:log("BodyRaw: ~s", [BodyRaw]),
    {some, CallbackStr} = hope_kv_list:get(Headers, "location"),
    CallbackBin = list_to_binary(CallbackStr),
    {ok, CallbackURI} = oauth1_uri:of_bin(CallbackBin),
    CallbackParams = oauth1_uri:get_query(CallbackURI),
    ct:log("CallbackParams: ~p", [CallbackParams]),
    {some, TmpTokenID} = hope_kv_list:get(CallbackParams, <<"oauth_token">>),
    {some, Verifier}   = hope_kv_list:get(CallbackParams, <<"oauth_verifier">>),
    State2 = State1#state{verifier={some, Verifier}},
    303 = StatusCode,
    state_return(State2).

t_token_ok(Cfg) ->
    State1 = state_get(Cfg, t_authorize),
    #state
        { tmp_token_id     = {some, TmpTokenID}
        , tmp_token_secret = {some, TmpTokenSecret}
        , verifier         = {some, VerifierBin}
        , client_id        = {some, ClientID}
        , client_secret    = {some, ClientSecret}
        } = State1,
    Verifier     = oauth1_verifier:cons({tmp, TmpTokenID}, VerifierBin),
    Realm        = <<"oblivion">>,
    ReqURL       = "https://localhost:8443/token",
    {ok, URI}    = oauth1_uri:of_bin(list_to_binary(ReqURL)),
    Resource     = oauth1_resource:cons(Realm, URI),
    TimestampBin = <<"123456789">>,
    TimestampInt =    123456789,
    Nonce        = <<"foofoobahbah">>,
    Token        = oauth1_credentials:cons(tmp, TmpTokenID, TmpTokenSecret),
    SigMeth      = 'HMAC_SHA1',
    SigMethBin   = oauth1_signature:method_to_bin(SigMeth),
    SigArgs      = #oauth1_signature_args_cons
        { method               = SigMeth
        , http_req_method      = <<"POST">>
        , http_req_host        = <<"localhost">>
        , resource             = Resource
        , consumer_key         = {client, ClientID}
        , timestamp            = TimestampInt
        , nonce                = Nonce
        , client_shared_secret = {client, ClientSecret}
        , token                = {some, Token}
        , verifier             = {some, Verifier}
        , callback             = none
        , version              = none
        },
    Sig             = oauth1_signature:cons(SigArgs),
    Signature       = oauth1_signature:get_digest(Sig),
    SignatureMethod = SigMethBin,
    Params =
        [ {?PARAM_REALM            , Realm}
        , {?PARAM_CONSUMER_KEY     , ClientID}
        , {?PARAM_TOKEN            , TmpTokenID}
        , {?PARAM_SIGNATURE        , Signature}
        , {?PARAM_SIGNATURE_METHOD , SignatureMethod}
        , {?PARAM_TIMESTAMP        , TimestampBin}
        , {?PARAM_NONCE            , Nonce}
        , {?PARAM_VERIFIER         , VerifierBin}
        ],
    AuthorizationBin = oauth1_parameters:to_http_header_authorization(Params),
    Authorization    = binary_to_list(AuthorizationBin),
    ReqHeaders =
        [ {"Authorization", Authorization}
        ],
    ReqMethod = post,
    ReqContentType = "application/x-www-form-urlencoded",
    ReqBody = "",
    Request = {ReqURL, ReqHeaders, ReqContentType, ReqBody},
    ReqOptions = [],
    ReqHTTPOptions = [{ssl, [{verify, verify_none}]}],
    {ok, Response} = httpc:request(ReqMethod, Request, ReqHTTPOptions, ReqOptions),
    {Status, Headers, BodyRaw} = Response,
    {_HttpVsn, StatusCode, _StatusMsg} = Status,
    ct:log("Status: ~p", [Status]),
    ct:log("Headers: ~p", [Headers]),
    ct:log("BodyRaw: ~s", [BodyRaw]),
    200 = StatusCode,
    ParseQS = hope_result:lift_exn(fun cow_qs:parse_qs/1),
    BodyParseResult = ParseQS(list_to_binary(BodyRaw)),
    ct:log("BodyParseResult: ~p", [BodyParseResult]),
    {ok, BodyParsed} = BodyParseResult,
    BP1 = BodyParsed,
    {{some, TokenID}    , BP2} = hope_kv_list:pop(BP1, ?PARAM_TOKEN),
    {{some, TokenSecret}, BP3} = hope_kv_list:pop(BP2, ?PARAM_TOKEN_SECRET),
    [] = BP3,
    _State2 = State1#state
        { token_id     = {some, TokenID}
        , token_secret = {some, TokenSecret}
        }.


%%=============================================================================
%% Helpers
%%=============================================================================

register_client() ->
    ClientID     = <<"hero-of-kvatch">>,
    ClientSecret = <<"thereisnosword">>,
    ClientCreds  = oauth1_credentials:cons(client, ClientID, ClientSecret),
    {ok, ok}     = oauth1_credentials:store(ClientCreds),
    {ClientID, ClientSecret}.

state_return(State) ->
    {save_config, State}.

state_get(Cfg, NameOfSetterTestCase) ->
    {some, {NameOfSetterTestCase, State}} = hope_kv_list:get(Cfg, saved_config),
    State.
