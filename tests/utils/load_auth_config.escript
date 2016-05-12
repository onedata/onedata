#!/usr/bin/env escript
%%! -name gettoken@test


main([OpenIdServerIP, OutputFile]) ->

    AuthContent = {{dropbox, [
        {auth_module, auth_dropbox},
        {app_id, <<"mock_id">>},
        {app_secret, <<"mock_secret">>},
        % Provider specific config
        {authorize_endpoint, <<"https://{open_id_ip}/1/oauth2/authorize">>},
        {access_token_endpoint, <<"https://{open_id_ip}/1/oauth2/token">>},
        {user_info_endpoint, <<"https://{open_id_ip}/1/account/info">>}
    ]}}