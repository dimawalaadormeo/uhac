-module(uhac_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
   
    %%PrivDir = code:priv_dir(eigenfaces),
    PD = case code:priv_dir(uhac) of
        {error, bad_name} ->
            % This occurs when not running as a release; e.g., erl -pa ebin
            % Of course, this will not work for all cases, but should account 
            % for most
            PrivDir = "priv";
        PrivDir ->
            % In this case, we are running in a release and the VM knows
            % where the application (and thus the priv directory) resides
            % on the file system
            PrivDir
    end,
    {ok, ExtProg} = application:get_env(eigenfaces, extprog),
    io:fwrite("~s   ~s ~n",[PrivDir,ExtProg]),
    inets:start(),
    uhac_sup:start_link(filename:join([PD, ExtProg])).
 
stop(_State) ->
    ok.
