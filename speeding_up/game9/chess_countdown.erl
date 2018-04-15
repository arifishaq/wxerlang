-module(chess_countdown).

-behaviour(wx_object).
-include_lib("wx/include/wx.hrl").

-export([start_link/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 handle_event/2, terminate/2, code_change/3]).

start_link(Frame, Seconds) ->
    wx_object:start_link(?MODULE, [Frame, Seconds], []).

init([Frame, Seconds]) ->
    D = wxDialog:new(Frame, ?wxID_ANY, "Count down to start", [{style, ?wxSTAY_ON_TOP bor ?wxCAPTION bor ?wxCLOSE_BOX} %%, {size, {200,200}}
    ]),
    Label = integer_to_list(Seconds),
    Sz = wxBoxSizer:new(?wxVERTICAL),
    Count = wxStaticText:new(D, ?wxID_ANY, Label, [{style, ?wxALIGN_CENTRE}]),
    Font = wxFont:new(64, ?wxFONTFAMILY_TELETYPE, ?wxFONTSTYLE_NORMAL, ?wxFONTWEIGHT_NORMAL),
    wxStaticText:setFont(Count, Font),
    wxSizer:addStretchSpacer(Sz),
    wxSizer:add(Sz,Count,[{flag, ?wxALIGN_CENTRE}]),
    wxSizer:addStretchSpacer(Sz),
    wxDialog:setSizer(D, Sz),

    erlang:send_after(1000, self(), update),
    wxDialog:show(D),
    {D, #{dialog => D, label => Count, secs => Seconds-1}}.

handle_event(#wx{}, State) ->
    {noreply, State}.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(update, State = #{secs := 0}) ->
    {stop, normal, State};
handle_info(update, State = #{secs := Secs, label := Label}) ->    
    wxStaticText:setLabel(Label, integer_to_list(Secs)),
    erlang:send_after(1000, self(), update),
    {noreply, State#{secs => Secs-1}};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, #{dialog := Dialog}) ->
    wxDialog:destroy(Dialog),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
