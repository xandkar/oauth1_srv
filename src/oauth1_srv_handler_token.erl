-module(oauth1_srv_handler_token).

-export(
    % Cowboy
    [ init/3

    % Cowboy REST
    , allowed_methods/2
    , content_types_accepted/2
    , is_authorized/2
    , rest_terminate/2
    , rest_init/2

    % Content handlers
    , content_handler/2
    ]).


-record(state,
    {
    }).


%% ============================================================================
%% Cowboy
%% ============================================================================

init(_Transport, _R, _Opts) ->
    {upgrade, protocol, cowboy_rest}.


%% ============================================================================
%% Cowboy REST
%% ============================================================================

rest_init(R1, _Opts) ->
    S1 = #state{},
    {ok, R1, S1}.

rest_terminate(_R, #state{}) ->
    ok.

allowed_methods(R, #state{}=S) ->
    AllowedMethods = [<<"POST">>],
    lager:info("AllowedMethods: ~p", [AllowedMethods]),
    {AllowedMethods, R, S}.

content_types_accepted(R, #state{}=S) ->
    ContentType = {<<"application">>, <<"x-www-form-urlencoded">>, '*'},
    Handler = content_handler,
    ContentTypesAccepted = [{ContentType, Handler}],
    lager:info("ContentTypesAccepted: ~p", [ContentTypesAccepted]),
    {ContentTypesAccepted, R, S}.

is_authorized(R1, #state{}=S) ->
    {ok, {IsAuthorized, R2}} = oauth1_srv_authentication:is_client(R1),
    lager:info("IsAuthorized: ~p", [IsAuthorized]),
    {IsAuthorized, R2, S}.


%% ============================================================================
%% Content handlers
%% ============================================================================

content_handler(R1, #state{}=S) ->
    lager:info("Begin content_handler."),
    lager:info("Set resourse URI."),
    % TODO: Grab host and endpoint from request. Duh!
    {ok, URI} = oauth1_uri:of_bin(<<"https://localhost:8443/token">>),
    lager:info("Get authorization header."),
    case cowboy_req:header(<<"authorization">>, R1)
    of  {undefined, R2} ->
            lager:info("Error: header missing: \"authorization\"."),
            ErrorData =
               [{<<"error">>, [{<<"header_missing">>, <<"authorization">>}]}],
            Body = jsx:encode(ErrorData),
            R3 = cowboy_req:set_resp_body(Body, R2),
            {false, R3, S}
    ;   {<<Authorization/binary>>, R2} ->
            lager:info("OK: got \"authorization\" header: ~p.", [Authorization]),
            Steps =
                [ fun oauth1_parameters:of_http_header_authorization/1
                , fun (P) -> oauth1_server:token_args_of_params(URI, P) end
                , fun oauth1_server:token/1
                ],
            lager:info("Begin processing pipe."),
            % TODO: Handle IO Errors
            case hope_result:pipe(Steps, Authorization)
            of  {error, {invalid_format, _}} ->
                    lager:info("Request error: header malformed: \"authorization\"."),
                    ErrorData =
                        [ { <<"error">>
                          , [{<<"header_malformed">>, <<"authorization">>}]
                          }
                        ],
                    % TODO: Resolve this violation of declared content type:
                    Body = jsx:encode(ErrorData),
                    R3 = cowboy_req:set_resp_body(Body, R2),
                    {false, R3, S}
            ;   {error, {bad_request, _}=Error} ->
                    lager:info("Request error: ~p", [Error]),
                    Body = oauth1_server:error_to_bin(Error),
                    R3 = cowboy_req:set_resp_body(Body, R2),
                    {false, R3, S}
            ;   {error, {unauthorized, _}=Error} ->
                    lager:info("Request error: ~p", [Error]),
                    Body = oauth1_server:error_to_bin(Error),
                    R3 = cowboy_req:set_resp_body(Body, R2),
                    {ok, R4} = cowboy_req:reply(401, R3),
                    {halt , R4, S}
            ;   {ok, Token} ->
                    lager:info("Request OK: ~p", [Token]),
                    TokenID     = oauth1_credentials:get_id(Token),
                    TokenSecret = oauth1_credentials:get_secret(Token),
                    {token, <<TokenIDBin/binary>>    } = TokenID,
                    {token, <<TokenSecretBin/binary>>} = TokenSecret,
                    Body =
                        % TODO: Use cow_qs:qs/1
                        << "oauth_token"             , "=" , TokenIDBin/binary
                         , "&"
                         , "oauth_token_secret"      , "=" , TokenSecretBin/binary
                        >>,
                    R3 = cowboy_req:set_resp_body(Body, R2),
                    {true, R3, S}
            end
    end.
