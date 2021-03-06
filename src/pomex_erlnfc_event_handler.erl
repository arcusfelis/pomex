%%%-------------------------------------------------------------------
%%% @author Michal Slaski
%%% @doc
%%%
%%% @end
%%% Created : 26 Aug 2016 by Michal Slaski
%%%-------------------------------------------------------------------
-module(pomex_erlnfc_event_handler).

-behaviour(gen_event).

%% gen_event callbacks
-export([init/1, handle_event/2, handle_call/2, 
         handle_info/2, terminate/2, code_change/3]).

-record(state, {last_seen}).


%%%===================================================================
%%% gen_event callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a new event handler is added to an event manager,
%% this function is called to initialize the event handler.
%%
%% @spec init(Args) -> {ok, State}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    {ok, #state{last_seen=dict:new()}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever an event manager receives an event sent using
%% gen_event:notify/2 or gen_event:sync_notify/2, this function is
%% called for each installed event handler to handle the event.
%%
%% @spec handle_event(Event, State) ->
%%                          {ok, State} |
%%                          {swap_handler, Args1, State1, Mod2, Args2} |
%%                          remove_handler
%% @end
%%--------------------------------------------------------------------

handle_event({nfctag,Nfctag}, State=#state{last_seen=LastSeen}) ->
    LastSeen2 = handle_nfc_tag(Nfctag, LastSeen),
    {ok, State#state{last_seen=LastSeen2}};
handle_event(Event, State) ->
    pomex_sound:play_error(),
    lager:error("issue=unknown_event, event=~1000p", [Event]),
    {ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever an event manager receives a request sent using
%% gen_event:call/3,4, this function is called for the specified
%% event handler to handle the request.
%%
%% @spec handle_call(Request, State) ->
%%                   {ok, Reply, State} |
%%                   {swap_handler, Reply, Args1, State1, Mod2, Args2} |
%%                   {remove_handler, Reply}
%% @end
%%--------------------------------------------------------------------
handle_call(_Request, State) ->
    Reply = ok,
    {ok, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called for each installed event handler when
%% an event manager receives any other message than an event or a
%% synchronous request (or a system message).
%%
%% @spec handle_info(Info, State) ->
%%                         {ok, State} |
%%                         {swap_handler, Args1, State1, Mod2, Args2} |
%%                         remove_handler
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever an event handler is deleted from an event manager, this
%% function is called. It should be the opposite of Module:init/1 and
%% do any necessary cleaning up.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%%===================================================================
%%% Internals
%%%===================================================================

nfctag_to_binary(Nfctag) when is_integer(Nfctag) ->
    list_to_binary(integer_to_list(Nfctag));
nfctag_to_binary(Nfctag) when is_binary(Nfctag) ->
    Nfctag.

handle_nfc_tag(Nfctag, LastSeen) ->
    lager:info("issue=nfctag_received, event=~p", [Nfctag]),
    Now = os:timestamp(),
    %% Detect duplicates
    case dict:find(Nfctag, LastSeen) of
        {ok, Timestamp} ->
            Milliseconds = timer:now_diff(Now, Timestamp) div 1000,
            case is_duplicate(Milliseconds) of
                true ->
                    lager:info("issue=ignore_duplicate, event=~p, milliseconds=~p", [Nfctag, Milliseconds]),
                    ok;
                false ->
                    pass_nfc_tag(Nfctag)
            end;
        _ ->
            %% First time seen
            pass_nfc_tag(Nfctag)
    end,
    dict:store(Nfctag, Now, LastSeen).

pass_nfc_tag(Nfctag) ->
    TriggerId = nfctag_to_binary(Nfctag),
    spawn(fun() -> catch pomex:fire_trigger(TriggerId) end).

%% Less than three seconds
is_duplicate(Milliseconds) -> Milliseconds < 3000.
