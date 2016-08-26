-module(pomex_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).
-define(CHILDW(I), {I, {I, start_link, []}, permanent, 5000, worker, [I]}).
-define(CHILDWA(I, A), {I, {I, start_link, A}, permanent, 5000, worker, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    {ok, { {one_for_all, 5, 10}, [?CHILDWA(gen_event, [{local, erlnfc_event}]),
                                  ?CHILDW(erlnfc_reader),
                                  ?CHILDW(pomex)
                                 ]} }.

