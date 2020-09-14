-module(tdlib_nif).

%% API exports
-export([new/0, send/2, execute/2, recv/2]).
-export([set_log_verbosity_level/1, set_log_file_path/1, set_log_max_file_size/1]).

-on_load(init/0).

init() ->
    %% TODO use some module argument
    PrivDir = code:priv_dir(tdlib),
    File = filename:join([PrivDir, "crates", "tdlib-nif", "libtdlib_nif.so"]),

    erlang:load_nif(filename:rootname(File), 0).

%%====================================================================
%% API functions
%%====================================================================

new() ->
    exit(nif_lirary_not_loaded).

send(_, _) ->
    exit(nif_lirary_not_loaded).

execute(_, _) ->
    exit(nif_lirary_not_loaded).

recv(_, _) ->
    exit(nif_lirary_not_loaded).

set_log_verbosity_level(_) ->
    exit(nif_lirary_not_loaded).

set_log_file_path(_) ->
    exit(nif_lirary_not_loaded).

set_log_max_file_size(_) ->
    exit(nif_lirary_not_loaded).

%%====================================================================
%% Internal functions
%%====================================================================
