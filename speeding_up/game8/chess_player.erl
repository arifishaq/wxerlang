-module(chess_player).

-behaviour(wx_object).
-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 handle_event/2, %% handle_sync_event/3, 
	 terminate/2, code_change/3]).

-include_lib("wx/include/wx.hrl").

start_link(PlayerName) ->
    wx_object:start_link(?MODULE, [PlayerName], []).

init([PlayerName]) ->
    wx:new(),
    Frame = wxFrame:new(wx:null(), ?wxID_ANY, PlayerName),

    MB = wxMenuBar:new(),

    GameMenu = wxMenu:new([]),
    wxMenu:append(GameMenu, ?wxID_NEW, "&New Game"),
    wxMenu:appendSeparator(GameMenu),
    wxMenu:append(GameMenu, ?wxID_EXIT, "&Quit"),

    BoardMenu = wxMenu:new([]),
    wxMenu:append(BoardMenu, ?wxID_ZOOM_IN, "&Bigger\tCtrl++"),
    wxMenu:append(BoardMenu, ?wxID_ZOOM_OUT, "&Smaller\tCtrl+-"),

    HelpMenu    = wxMenu:new([]), 
    wxMenu:append(HelpMenu, ?wxID_ABOUT, "&About"),
    wxMenu:append(HelpMenu, ?wxID_HELP, "&Help"), 

    wxMenuBar:append(MB, GameMenu, "&Game"),
    wxMenuBar:append(MB, BoardMenu, "&Board"),
    wxMenuBar:append(MB, HelpMenu, "&Help"),

    wxFrame:setMenuBar(Frame,MB),

    Sz = wxBoxSizer:new(?wxVERTICAL),

    CkSz = wxBoxSizer:new(?wxHORIZONTAL),
    Ck1 = chess_clock:start_link(Frame, self()),
    Ck2 = chess_clock:start_link(Frame),
    wxSizer:add(CkSz, Ck1, [{proportion, 1}, {flag, ?wxEXPAND}]),
    wxSizer:add(CkSz, Ck2, [{proportion, 1}, {flag, ?wxEXPAND}]),
    wxSizer:add(Sz, CkSz, [{flag, ?wxEXPAND}]),

    Board = chess_board:start_link(Frame, self()),
    wxSizer:add(Sz, Board, [{proportion, 1}, {flag, ?wxEXPAND}]),

    Button = wxButton:new(Frame, -1, [{label, "Moved"}]),
    wxButton:disable(Button),

    wxSizer:add(Sz, Button, [{flag, ?wxEXPAND}]),

    wxFrame:setSizer(Frame,Sz),
    wxFrame:show(Frame),
    wxFrame:refresh(Frame),

    wxPanel:setFocus(Board),

    {FW, FH} = wxFrame:getSize(Frame),
    {BW,BH} = wxPanel:getSize(Board),
    Dw = max(BW,BH) - BW,
    Dh = max(BW,BH) - BH,
    wxFrame:setSize(Frame, FW + Dw, FH + Dh),

    wxFrame:connect(Frame, command_menu_selected),

    {Frame, #{name => PlayerName, 
	      role => undefined,
	      frame => Frame, 
	      my_clock_pid => wx_object:get_pid(Ck1), 
	      other_clock_pid => wx_object:get_pid(Ck2), 
	      board_pid => wx_object:get_pid(Board), 
	      board => Board,
	      button => Button}}.


handle_event(#wx{id=?wxID_ZOOM_IN, 
		 event=#wxCommand{type = command_menu_selected}}, 
	     State = #{frame := Frame}) -> 
    {W,H} = wxFrame:getSize(Frame),
    wxFrame:setSize(Frame,W+8,H+8),
    {noreply, State};
    
handle_event(#wx{id=?wxID_ZOOM_OUT, 
		 event=#wxCommand{type = command_menu_selected}}, 
	     State = #{frame := Frame}) -> 
    {W,H} = wxFrame:getSize(Frame),
    wxFrame:setSize(Frame,W-8,H-8),
    {noreply, State};

handle_event(#wx{id=?wxID_ABOUT, 
		 event=#wxCommand{type = command_menu_selected}}, 
	     State = #{frame := Frame}) -> 
    M = wxMessageDialog:new(Frame, "A demonstration of wxErlang\n(c) Arif Ishaq, 2018\nThis work is licensed under the\nCreative Commons Attribution-ShareAlike 4.0\nInternational License."),
    wxMessageDialog:showModal(M),
    wxMessageDialog:destroy(M),
    {noreply, State};

handle_event(#wx{} = _Wx, State) ->
    io:format("chess_player got event ~p~n", [_Wx]),
    {noreply, State}.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    wx:destroy(),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

