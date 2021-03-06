-module(chess_clock).

-export([start_link/1, start_link/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 handle_event/2, terminate/2, code_change/3]).

-behaviour(wx_object).
-include_lib("wx/include/wx.hrl").

-define(MY_CLOCK, my_clock).
-define(OTHER_CLOCK, other_clock).

start_link(Parent) ->
    start_link(Parent, none).
start_link(Parent, PlayerPid) ->
    wx_object:start_link(?MODULE, [Parent, PlayerPid], []).

init([Parent, PlayerPid]) ->
    Panel = wxPanel:new(Parent),
    BGPanel = wxPanel:new(Panel),
    Sz = wxBoxSizer:new(?wxVERTICAL),
    wxSizer:add(Sz, BGPanel, [{flag, ?wxLEFT bor ?wxTOP bor ?wxRIGHT  bor ?wxEXPAND}, {border, 5}]),

    {ZeroLabel, _} = time_label(0, hide),
    T = wxStaticText:new(BGPanel, -1, ZeroLabel),
    Font = wxFont:new(24, ?wxFONTFAMILY_TELETYPE, ?wxFONTSTYLE_NORMAL, ?wxFONTWEIGHT_NORMAL),
    wxStaticText:setFont(T, Font),
    TSz = wxBoxSizer:new(?wxVERTICAL),
    wxSizer:add(TSz, T, [{flag, ?wxALIGN_CENTRE}]),
    wxPanel:setSizer(BGPanel, TSz),

    Gauge = wxGauge:new(Panel, -1, 60, [{style, ?wxGA_HORIZONTAL bor ?wxGA_SMOOTH}, {size, {-1, 10}}]),
    wxSizer:add(Sz, Gauge, [{flag, ?wxLEFT bor ?wxBOTTOM bor ?wxRIGHT bor ?wxEXPAND}, {border, 5}]),

    wxPanel:setSizer(Panel, Sz),
    {Panel, #{parent => Parent, 
	      player_pid => PlayerPid, 
	      panel => Panel, 
	      static_text => T,
	      ticking => false,
	      seconds_left => 0,
	      colon_showing_policy => show,
	      gauge => Gauge}}.

handle_event(#wx{}, State) ->
    {noreply, State}.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({set_colour, Colour}, State = #{panel := Panel}) ->
    wxPanel:setOwnBackgroundColour(Panel, 
				case Colour of 
				    white -> {255,255,255};
				    black -> {0,0,0}
				end),
    wxWindow:refresh(Panel),
    {noreply, State};

handle_info({ticking, TrueOrFalse}, 
	    State = #{seconds_left := SecondsLeft,
		      static_text := T}) ->
    {Label, _} = time_label(SecondsLeft, hide),
    wxStaticText:setLabel(T, Label),
    [erlang:send_after(1000, self(), tick) || TrueOrFalse =:= true],
    {noreply, State#{ticking => TrueOrFalse, colon_showing => true}};


handle_info({reset, Seconds}, State = #{gauge := Gauge, static_text := T}) ->
    {Label, _} = time_label(Seconds, hide),
    wxStaticText:setLabel(T, Label),
    wxGauge:setValue(Gauge, 0),
    {noreply, State#{seconds_left => Seconds, colon_showing_policy := show}};

handle_info(tick, State = #{ticking := false, colon_showing_policy := show}) ->
    {noreply, State};
handle_info(tick, State = #{ticking := false, 
			    colon_showing_policy := hide,
			    seconds_left := SecondsLeft,
			    static_text := T}) ->
    {Label, _} = time_label(SecondsLeft, hide),
    wxStaticText:setLabel(T, Label),
    {noreply, State#{colon_showing_policy => show}};
handle_info(tick, State = #{player_pid := PlayerPid, 
			    ticking := true, 
			    seconds_left := 0, 
			    gauge := Gauge}) ->
    wxGauge:setValue(Gauge, 60),
    [PlayerPid ! {we_end, time_up} || PlayerPid =/= none],
    {noreply, State#{ticking => false}};
handle_info(tick, State = #{ticking := true, 
			    colon_showing_policy := ColonShowingPolicy, 
			    static_text := T, 
			    seconds_left := SecondsLeft, 
			    gauge := Gauge}) ->
    erlang:send_after(1000, self(), tick),
    {Label, ColonShowing} = time_label(SecondsLeft, ColonShowingPolicy),
    wxStaticText:setLabel(T, Label),
    [wxGauge:setValue(Gauge, 60 - SecondsLeft) || SecondsLeft < 60],
    {noreply, State#{colon_showing_policy => ColonShowing, 
		     seconds_left => SecondsLeft - 1}};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, #{panel := Panel}) ->
    catch wxPanel:destroy(Panel),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


time_label(SecondsLeft, ColonShowingPolicy) 
  when ColonShowingPolicy =:= show; 
       ColonShowingPolicy =:= hide ->
    HoursLeft = integer_to_list(SecondsLeft div 3600),
    MinsLeft = case (SecondsLeft rem 3600) div 60 of 
	       M when M < 10 -> "0" ++ integer_to_list(M);
	       M -> integer_to_list(M)
	   end,
    case ColonShowingPolicy of
	show ->  {HoursLeft ++ " " ++ MinsLeft, hide};
	hide -> {HoursLeft ++ ":" ++ MinsLeft, show}
    end.
