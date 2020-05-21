-module(encrypt).

-export([encrypt/1, decrypt/1]).

-define(SECRET_KEY_FILE, "secret.key").
-define(MODE, aes_gcm).
-define(AAD, <<"AES256GCM">>).
-define(IV, <<"ab">>).

encrypt(Msg) when is_binary(Msg) ->
    {CipherText, CipherTag} = crypto:block_encrypt(?MODE, get_secret_key(), ?IV, {?AAD, Msg, 16}),
    list_to_binary([CipherTag, CipherText]).

decrypt(<<Tag:16/binary, CipherText/binary>>) ->
    crypto:block_decrypt(?MODE, get_secret_key(), ?IV, {?AAD, CipherText, Tag}).

get_secret_key() ->
    % Check if file exists
    EncodedKey = case filelib:is_regular(?SECRET_KEY_FILE) of
                     true ->
                         {ok, Contents} = file:read_file(?SECRET_KEY_FILE),
                         Contents;
                     _ ->
                         % If not generate key
                         Contents = generate_secret_key(),
                         file:write_file(?SECRET_KEY_FILE, Contents),
                         Contents
                 end,
    % decode
    base64:decode(EncodedKey).



generate_secret_key() ->
    Key = crypto:strong_rand_bytes(16),
    base64:encode(Key).
