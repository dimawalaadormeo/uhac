-module(uhac_sup).

-behaviour(supervisor).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link(ExtProg) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [ExtProg]).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([ExtProg]) ->
    UserSpec = [ {uhac_port, {uhac_port, start_link, [ExtProg]},
                            permanent, 2000, worker, [uhac_port]},{tcp_receiver_sup, {tcp_receiver_sup, start_link, []},
                            permanent, 2000, worker, [tcp_receiver_sup]},{tcp_receiver_enroll_sup, {tcp_receiver_enroll_sup, start_link, []},
                            permanent, 2000, worker, [tcp_receiver_enroll_sup]} ],
       StartSpecs = {{one_for_one, 5, 10}, UserSpec},
       {ok, StartSpecs}.

