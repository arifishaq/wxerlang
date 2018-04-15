-module(chess_player).

-behaviour(wx_object).

-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 handle_event/2, 
	 terminate/2, code_change/3]).

-include_lib("wx/include/wx.hrl").

-define(ARBITER, chess_arbiter).

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

    timer:sleep(20), %% ensure the size message arrives when the frame is visible
    wxPanel:setFocus(Board),

    {FW, FH} = wxFrame:getSize(Frame),
    {BW,BH} = wxPanel:getSize(Board),
    Dw = max(BW,BH) - BW,
    Dh = max(BW,BH) - BH,
    wxFrame:setSize(Frame, FW + Dw, FH + Dh),

    wxFrame:connect(Frame, command_menu_selected),
    wxFrame:connect(Frame, command_button_clicked, []),
    
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

handle_event(#wx{id = ?wxID_NEW, event = #wxCommand{type = command_menu_selected}}, State = #{name := Name, frame := Frame}) ->
    %% chess_arbiter:start_link(),
    D = chess_new_game_dialog:start_link(Frame),
    Choice = chess_new_game_dialog:get_choice(wx_object:get_pid(D)),
    %% wx_object:stop(D),
    case Choice of 
	{ok, #{min_duration := MinDuration,
	       max_duration := MaxDuration,
	       max_wait := MaxWait}}  -> 
    	    chess_arbiter:want_to_play(Name, MinDuration, MaxDuration, MaxWait);
    	_ -> ok
    end,    
    {noreply, State};
   
handle_event(#wx{id = ?wxID_EXIT, 
		 event = #wxCommand{type = command_menu_selected}},
	     State = #{role := MyColour,
		       frame := Frame}) ->
    D = wxMessageDialog:new(Frame, "Do you really want to quit?", [{style, ?wxICON_QUESTION bor ?wxYES_NO}]),
    Decision = wxMessageDialog:showModal(D),
    wxMessageDialog:destroy(D),
    case Decision of 
	?wxID_YES -> (catch global:send(?ARBITER, {quit, MyColour})),
		     {stop, normal, State};
	?wxID_NO -> {noreply, State}
    end;

handle_event(#wx{event = #wxCommand{type = command_button_clicked}}, 
	     State = #{button := Button,
		       role := Colour}) ->
    global:send(?ARBITER, {done, Colour}),
    wxButton:disable(Button),
    {noreply, State};

handle_event(#wx{} = _Wx, State) ->
    {noreply, State}.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(nomatch, State = #{name := Name, frame := Frame}) ->
    M = wxMessageDialog:new(Frame,
			    "Sorry, " ++ Name ++ ",\n"
			    "no one matched your request",
			    [{style, ?wxOK}]),
    wxMessageDialog:showModal(M),
    wxMessageDialog:destroy(M),
    {noreply, State};

handle_info({prepare_to_play, _OpponentName, Duration, When}, 
	    State = #{frame := Frame, 
		      my_clock_pid := MyCk, 
		      other_clock_pid := OtherCk}) ->
    chess_countdown:start_link(Frame, When),
    MyCk ! {reset, Duration*60},
    OtherCk ! {reset, Duration*60},
    {noreply, State};

handle_info({role, MyColour, Layout}, 
	    State = #{board_pid := Board,
		      my_clock_pid := MyCk,
		      other_clock_pid := OtherCk}) ->
    Board ! {role, MyColour, Layout},
    MyCk ! {set_colour, MyColour},
    OtherCk ! {set_colour, chess_utils:opponent(MyColour)},
    {noreply, State#{role => MyColour}};

%% my move
handle_info({play, Colour}, State = #{board_pid := Board,
				      role := Colour,
				      my_clock_pid := MyCk,
				      other_clock_pid := OtherCk}) ->
    Board ! prepare_to_select,
    MyCk ! {ticking, true},
    OtherCk ! {ticking, false},
    {noreply, State};

%% opponent's move
handle_info({play, _Colour}, State = #{my_clock_pid := MyCk,
				       other_clock_pid := OtherCk}) ->
    MyCk ! {ticking, false},
    OtherCk ! {ticking, true},
    {noreply, State};

handle_info({we_selected, SquareLocation}, State = #{role := Colour}) ->
    global:send(?ARBITER, {we_selected, Colour, SquareLocation}), 
    {noreply, State};
            
handle_info({they_selected, SquareLocation}, State = #{board_pid := Board}) ->
    Board ! {they_selected, SquareLocation}, 
    {noreply, State};

handle_info({we_moved, FromLocation, ToLocation}, 
	    State = #{role := Colour, button := Button}) ->
    global:send(?ARBITER, {we_moved, Colour, FromLocation, ToLocation}), 
    wxButton:enable(Button),
    {noreply, State};

handle_info({we_moved_to_castle, FromLocation, ToLocation}, 
	    State = #{role := Colour, board_pid := Board}) ->
    global:send(?ARBITER, {we_moved, Colour, FromLocation, ToLocation}), 
    Board ! prepare_to_select,
    {noreply, State};

handle_info({{we_promoted_a_pawn, PromotedRole}, FromLocation, ToLocation}, 
	    State = #{role := Colour, button := Button}) ->
    global:send(?ARBITER, {{we_promoted_a_pawn, PromotedRole}, 
			   Colour, FromLocation, ToLocation}), 
    wxButton:enable(Button),
    {noreply, State};

handle_info({they_moved, FromLocation, ToLocation}, 
	    State = #{board_pid := Board}) ->
    Board ! {they_moved, FromLocation, ToLocation}, 
    {noreply, State};

handle_info({{they_promoted_a_pawn, PromotedRole}, FromLocation, ToLocation}, 
	    State = #{board_pid := Board}) ->
    Board ! {{they_promoted_a_pawn,PromotedRole}, FromLocation, ToLocation}, 
    {noreply, State};


% note, this comes from our board or clock that detected the game end
handle_info({we_end, Why}, State = #{role := MyColour, 
				     board_pid := Board, 
				     my_clock_pid := MyClock,
				     other_clock_pid := OtherClock}) ->
    global:send(?ARBITER, {end_game, MyColour, Why}), 
    Board ! {end_game, MyColour, Why}, 
    MyClock ! {ticking, false},
    OtherClock ! {ticking, false},    
    {noreply, State};

%% note the Colour parameter; this comes from the arbiter
handle_info({end_game, Colour, Why}, 
	    State = #{board_pid := Board, 
		      role := MyColour, 
		      my_clock_pid := MyClock,
		      other_clock_pid := OtherClock}) ->
    Colour = chess_utils:opponent(MyColour), %% assertion
    Board ! {end_game, Colour, Why}, 
    MyClock ! {ticking, false},
    OtherClock ! {ticking, false},    
    {noreply, State};

handle_info(quit, State = #{frame := Frame,
			    board_pid := Board, 
			    my_clock_pid := MyClock,
			    other_clock_pid := OtherClock}) ->
    D = wxMessageDialog:new(Frame, "Opponent quit", [{style, ?wxOK}]),
    wxMessageDialog:showModal(D),
    wxMessageDialog:destroy(D),
    Board ! {end_game, opponent_quit}, 
    MyClock ! {ticking, false},
    OtherClock ! {ticking, false},    
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    wx:destroy(),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

