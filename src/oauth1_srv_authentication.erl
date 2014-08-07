-module(oauth1_srv_authentication).

-export(
    [ is_client/1
    , is_owner/1
    ]).


-callback is_client(cowboy_req:req()) ->
    hope_result:t({boolean(), cowboy_req:req()}, any()).

-callback is_owner(cowboy_req:req()) ->
    hope_result:t({boolean(), cowboy_req:req()}, any()).


is_client(Req) ->
    Module = lookup_implementation_module(),
    Module:is_client(Req).

is_owner(Req) ->
    Module = lookup_implementation_module(),
    Module:is_owner(Req).


lookup_implementation_module() ->
    oauth1_srv_cfg:get(authentication_module).
