-module(resize_no_skip).

-behaviour(wx_object).

-include_lib("wx/include/wx.hrl").

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 handle_event/2, terminate/2, code_change/3]).

start_link() ->
    wx_object:start_link(?MODULE, [], []).

init([]) ->
    wx:new(),
    Frame = wxFrame:new(wx:null(), ?wxID_ANY, "resize, no skip"),
    Sizer = wxBoxSizer:new(?wxVERTICAL),
    MkButton = 
	fun(TheFrame, TheSizer, TheLabel) ->
		B = wxButton:new(TheFrame, ?wxID_ANY, [{label, TheLabel}]),
		wxSizer:add(TheSizer, B, [{flag, ?wxEXPAND}])
	end,
    [MkButton(Frame, Sizer, L) || L <- ["One", "Two", "Three"]],
    wxFrame:setSizer(Frame, Sizer),
    wxSizer:setSizeHints(Sizer, Frame),
    wxFrame:connect(Frame, size),  %% subscribing to the size event
    wxFrame:show(Frame),
    {Frame, #{frame => Frame}}.

handle_event(#wx{event = #wxSize{}}, State = #{frame := F}) ->
    wxWindow:refresh(F),
    {noreply, State};

handle_event(#wx{}, State) ->  
    {noreply, State}.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(resize, State = #{frame := Frame}) ->
    {W, _} = wxFrame:getSize(Frame),
    wxFrame:setSize(Frame, W + 100, -1),
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _) ->
    wx:destroy().

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

