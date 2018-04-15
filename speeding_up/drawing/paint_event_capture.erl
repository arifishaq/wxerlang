-module(paint_event_capture).

-behaviour(wx_object).

-include_lib("wx/include/wx.hrl").

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 handle_event/2, terminate/2, code_change/3]).

start_link() ->
    wx_object:start_link(?MODULE, [], []).

init([]) ->
    wx:new(),
    Frame = wxFrame:new(wx:null(), ?wxID_ANY, "paint event capture"),
    wxFrame:connect(Frame, paint),  %% subscribing to the paint event
    wxFrame:show(Frame),
    {Frame, #{frame => Frame}}.

handle_event(#wx{} = Event, State) ->  
    io:format("got ~p~n", [Event]),	
    {noreply, State}.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    wx:destroy().

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

