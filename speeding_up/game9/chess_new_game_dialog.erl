-module(chess_new_game_dialog).

-behaviour(wx_object).
-include_lib("wx/include/wx.hrl").

-export([start_link/1, get_choice/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 handle_event/2, terminate/2, code_change/3]).


get_choice(Dialog) ->
    gen_server:call(Dialog, get_choice, infinity).

start_link(Frame) ->
    wx_object:start_link(?MODULE, [Frame], []).

init([Frame]) ->
    D = wxDialog:new(Frame, ?wxID_ANY, "New Game"),
    Sz = wxGridBagSizer:new(),

    MinDurationCtrl = wxSpinCtrl:new(D, [{style, ?wxSP_ARROW_KEYS}, {min, 1}, {max, 180}, {initial, 5}]),
    MaxDurationCtrl = wxSpinCtrl:new(D, [{style, ?wxSP_ARROW_KEYS}, {min, 5}, {max, 180}, {initial, 15}]),
    WaitCtrl = wxSpinCtrl:new(D, [{style, ?wxSP_ARROW_KEYS}, {min, 0}, {max, 15}, {initial, 1}]),
    
    wxGridBagSizer:add(
      Sz, 
      wxStaticText:new(D, ?wxID_ANY, "Min duration"), 
      {0,0},
      [{flag, ?wxALIGN_CENTER_VERTICAL bor ?wxALIGN_RIGHT bor ?wxALL}, {border, 5}]),
    wxGridBagSizer:add(
      Sz, 
      MinDurationCtrl,
      {0,1}, 
      [{flag, ?wxALL}, {border, 5}]),
    wxGridBagSizer:add(
      Sz, 
      wxStaticText:new(D, ?wxID_ANY, "minutes"), 
      {0,2},
      [{flag, ?wxALIGN_CENTER_VERTICAL bor ?wxALIGN_LEFT bor ?wxALL}, {border, 5}]),

    wxGridBagSizer:add(
      Sz, 
      wxStaticText:new(D, ?wxID_ANY, "Max duration"), 
      {1,0},
      [{flag, ?wxALIGN_CENTER_VERTICAL bor ?wxALIGN_RIGHT bor ?wxALL}, {border, 5}]),
    wxGridBagSizer:add(
      Sz, 
      MaxDurationCtrl,
      {1,1}, 
      [{flag, ?wxALL}, {border, 5}]),
    wxGridBagSizer:add(
      Sz, 
      wxStaticText:new(D, ?wxID_ANY, "minutes"), 
      {1,2},
      [{flag, ?wxALIGN_CENTER_VERTICAL bor ?wxALIGN_LEFT bor ?wxALL}, {border, 5}]),
    

    wxGridBagSizer:add(
      Sz, 
      wxStaticText:new(D, ?wxID_ANY, "Wait for at most"), 
      {2,0},
      [{flag, ?wxALIGN_CENTER_VERTICAL bor ?wxALIGN_RIGHT bor ?wxALL}, {border, 5}]),
    
    wxGridBagSizer:add(
      Sz, 
      WaitCtrl,
      {2,1}, 
      [{flag, ?wxALL}, {border, 5}]),
    wxGridBagSizer:add(
      Sz, 
      wxStaticText:new(D, ?wxID_ANY, "minutes"), 
      {2,2},
      [{flag, ?wxALIGN_CENTER_VERTICAL bor ?wxALIGN_LEFT bor ?wxALL}, {border, 5}]),

    wxGridBagSizer:add(
      Sz, 
      wxButton:new(D, ?wxID_CANCEL, [{label, "Cancel"}]), 
      {3,0},
      [{flag, ?wxALL}, {border, 5}]),

    OK = wxButton:new(D, ?wxID_OK, [{label, "OK"}]), 
    wxButton:setDefault(OK),

    wxGridBagSizer:add(
      Sz, 
      OK,
      {3,1},
      [{span, {1,2}}, {flag, ?wxALL bor ?wxEXPAND}, {border, 5}]),
  
    wxDialog:setSizer(D,Sz),
    wxSizer:setSizeHints(Sz, D),
    
    wxDialog:showModal(D),

    {D, #{dialog => D, 
	  min_ctrl => MinDurationCtrl, 
	  max_ctrl => MaxDurationCtrl, 
	  ok_button => OK, 
	  wait => WaitCtrl}}.


handle_event(#wx{}, State) ->
    {noreply, State}.

handle_call(get_choice, _From, 
	    State = #{dialog := D, 
		      min_ctrl := MinDurationCtrl, 
		      max_ctrl := MaxDurationCtrl, 
		      wait := WaitCtrl}) ->
    Choice = case wxDialog:getReturnCode(D) of
		 ?wxID_OK -> ok;
		 ?wxID_CANCEL -> cance
	     end,
    MinDuration = wxSpinCtrl:getValue(MinDurationCtrl),
    MaxDuration = wxSpinCtrl:getValue(MaxDurationCtrl),
    Wait = wxSpinCtrl:getValue(WaitCtrl),
    Reply = {Choice, #{min_duration => MinDuration, 
		       max_duration => MaxDuration, 
		       max_wait => Wait}},
    {stop, normal, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, #{dialog := D}) ->
    wxDialog:destroy(D),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
