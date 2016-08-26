-module(pomex).

%% API
-export([start_link/0,
         fire_trigger/1]).

%% Debugging
-export([start_task/1,
         start_task/2,
         stop_task/1]).

%% Private API
-export([expire_task/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(pomex_task, {trigger_id, pomid, start_time, expire_tref}).
-record(state, {}).


%%====================================================================
%% API
%%====================================================================

start_link() ->
    gen_server:start_link({local, pomex_server}, ?MODULE, [], []).

fire_trigger(TriggerId) when is_binary(TriggerId) ->
    case is_trigger_running(TriggerId) of
        true ->
            stop_task(TriggerId);
        false ->
            start_task(TriggerId)
    end.

stop_task(TriggerId) ->
    lager:info("issue=stop_task, trigger_id=~p", [TriggerId]),
    case ets:lookup(pomex_task, TriggerId) of
        [#pomex_task{pomid=PomId, start_time=StartTime, expire_tref=ExpireTRef}] ->
            DurationSeconds = task_duration(StartTime),
            lager:info("issue=stop_task, trigger_id=~p, pomid=~p, start_time=~p, "
                        "duration=~p seconds",
                       [TriggerId, PomId, StartTime, DurationSeconds]),
            timer:cancel(ExpireTRef),
            ets:delete(pomex_task, TriggerId),
            push_stop(TriggerId, DurationSeconds),
            pomex_sound:play_finish(),
            ok;
        _ ->
            lager:error("issue=stop_task:not_found, trigger_id=~p", [TriggerId]),
            ok
    end.

expire_task(TriggerId, PomId) ->
    lager:info("issue=expire_task, trigger_id=~p", [TriggerId]),
    case ets:lookup(pomex_task, TriggerId) of
        [#pomex_task{pomid=PomId2, expire_tref=ExpireTRef}] when PomId =:= PomId2 ->
            timer:cancel(ExpireTRef),
            ets:delete(pomex_task, TriggerId),
            pomex_sound:play_finish(),
            ok;
        [#pomex_task{pomid=PomId2}] ->
            lager:error("issue=stop_task:wrong_pomid, trigger_id=~p, "
                        "stored_pomid=~p, expired_pomid=~p",
                        [TriggerId, PomId, PomId2]),
            ok;
        _ ->
            lager:error("issue=stop_task:not_found, trigger_id=~p", [TriggerId]),
            ok
    end.

start_task(TriggerId) ->
    start_task(TriggerId, duration()).

start_task(TriggerId, DurationSeconds) ->
    lager:info("issue=start_task, trigger_id=~p", [TriggerId]),
    stop_task(TriggerId), %% to be sure
    PomId = ask_for_pomid(TriggerId),
    {ok, ExpireTRef} = timer:apply_after(DurationSeconds * 1000, ?MODULE, expire_task,
                                         [TriggerId, PomId]),
    Task = #pomex_task{trigger_id=TriggerId, pomid=PomId,
                       start_time=os:timestamp(), expire_tref=ExpireTRef},
    ets:insert_new(pomex_task, Task),
    pomex_sound:play_start(),
    ok.

is_trigger_running(TriggerId) ->
    ets:member(pomex_task, TriggerId).

duration() -> %% in seconds
    25*60.

%%====================================================================
%% remote server functions
%%====================================================================

ask_for_pomid(TriggerId) ->
    DeviceId = pomex_utils:get_device_id(),
    FullUrl = ask_for_pomid_url(),
    Headers = json_set_headers(),
    Payload = jsx:encode([{device_id, DeviceId}, {trigger_id, TriggerId}]),
    lager:info("issue=ask_for_pomid, trigger_id=~p, device_id=~p, url=~p",
               [TriggerId, DeviceId, FullUrl]),
    Res = lhttpc:request(FullUrl, 'POST', Headers, Payload, 5000),
    case Res of
        {ok, {{200,"OK"}, _Hdrs, Resp}}  ->
            lager:info("issue=server_returns, response=~p", [Resp]),
            response_to_pomid(Resp);
        Other ->
            lager:error("issue=server_returns, response=~p", [Other]),
            erlang:error(ask_for_pomid_failed)
    end.

push_stop(TriggerId, DurationSeconds) ->
    ok.

json_set_headers() ->
    [
        {"User-Agent", "Pomex on PI"},
        {"Accept", "application/json"},
        {"Content-type", "application/json"}
    ].

ask_for_pomid_url() ->
    "http://10.152.1.12:4000/users/2/pomodoros".

response_to_pomid(Resp) ->
    RespJSON = jsx:decode(Resp),
    {<<"pid">>, PomId} = lists:keyfind(<<"pid">>, 1, RespJSON),
    PomId.

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([]) ->
    ets:new(pomex_task, [set, named_table, public, {keypos, 2}]),
    {ok, #state{}}.

handle_call(_, _From, State) ->
    {reply, ok, State}.

handle_cast(Msg, State) ->
    lager:warning("Strange message ~p.", [Msg]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------

handle_info(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% internal
%%====================================================================

task_duration(StartTime) ->
    Microseconds = timer:now_diff(os:timestamp(), StartTime),
    microseconds_to_seconds(Microseconds).

microseconds_to_seconds(Microseconds) ->
    Microseconds div 1000000.
