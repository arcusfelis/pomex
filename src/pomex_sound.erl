-module(pomex_sound).
-export([play_start/0,
         play_finish/0,
         play_error/0]).

play_start() ->
    play_file(find_file("start.wav")).

play_finish() ->
    play_file(find_file("finish.wav")).

play_error() ->
    play_file(find_file("error.wav")).

find_file(Name) ->
    filename:absname(Name, priv_dir()).

priv_dir() ->
    case code:priv_dir(pomex) of
        [_|_] = PrivDir ->
            PrivDir;  
        _ ->
            "priv"
    end.

play_file(File) ->
    stop_playing(),
    erlang:open_port({spawn_executable, "/usr/bin/env"}, [{args, ["aplay", File]}]).

stop_playing() ->
    os:cmd("killall aplay").
