-module(pomex_utils).
-export([get_device_id/0]).

-spec get_device_id() -> binary().
get_device_id() ->
    {ok, Name} = inet:gethostname(),
    erlang:list_to_binary(Name).
