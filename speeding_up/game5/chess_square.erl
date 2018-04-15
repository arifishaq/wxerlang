-module(chess_square).
-behaviour(wx_object).
-export([start_link/5]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, 
	 handle_event/2, handle_sync_event/3,
	 terminate/2, code_change/3]).

-include_lib("wx/include/wx.hrl").

start_link(Location, BoardPid, Parent, Brush, SelectedBrush) ->
    wx_object:start_link(
      ?MODULE, [Location, BoardPid, Parent, Brush, SelectedBrush], []).

init([Location, BoardPid, Parent, Brush, SelectedBrush]) ->
    Panel = wxPanel:new(Parent, [{style, ?wxFULL_REPAINT_ON_RESIZE}]),
    wxPanel:setBackgroundStyle(Panel, ?wxBG_STYLE_CUSTOM),

    wxPanel:connect(Panel, erase_background, [callback]),
    wxPanel:connect(Panel, paint, [callback]),
    wxPanel:connect(Panel, left_down),
    wxPanel:connect(Panel, key_up),

    State = #{
      location => Location,
      board_pid => BoardPid,
      square_panel => Panel,
      image => none,
      brush => Brush,
      selected_brush => SelectedBrush,
      selectable => false,
      selected => false},
    {Panel, State}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({Property, Value}, State = #{square_panel := Panel})
  when Property =:= selectable; Property =:= landable ->
    case Value of 
	true -> 
	    wxWindow:setCursor(Panel, wxCursor:new(?wxCURSOR_HAND));
	false -> 
	    wxWindow:setCursor(Panel, ?wxNullCursor)
    end,
    {noreply, maps:put(Property, Value, State)};

handle_info({Property, Value}, State = #{square_panel := Panel}) ->
    wxWindow:refresh(Panel),
    {noreply, maps:put(Property, Value, State)};

handle_info(_Info, State) ->
    {noreply, State}.

%% mouse click to select a piece
handle_event(#wx{event=#wxMouse{type = left_down}}, 
	     State = #{square_panel := Panel, 
		       board_pid := BoardPid,
		       selectable := true,
		       selected := false,
		       location := Location}) ->
    BoardPid ! {we_selected, Location},
    wxPanel:refresh(Panel),    
    {noreply, State#{selected => true}};

%% %% mouse click to move piece to another location
handle_event(#wx{event=#wxMouse{type = left_down}}, 
	     State = #{board_pid := BoardPid,
		       landable := true,
		       location := Location}) ->
    BoardPid ! {we_moved, Location},
    {noreply, State};

handle_event(#wx{event=#wxMouse{type = left_down}}, State) ->
    {noreply, State};

handle_event(#wx{event = #wxKey{} = Event}, State = #{board_pid := BoardPid}) ->
    %% io:format("square ~p propagating ~p~n", [Loc, Event]),
    BoardPid ! {propagate_event, Event},
    {noreply, State};

handle_event(#wx{} = _Wx, State) ->
    %% io:format("square got ~p~n", [_Wx]),
    {noreply, State}.


handle_sync_event(#wx{event=#wxPaint{}}, _, State = #{location := _Loc}) ->
    %% io:format("painting square ~p~n", [_Loc]),
    paint_square(State);
handle_sync_event(#wx{event=#wxErase{}}, _, _) ->
    ok.

terminate(_Reason, #{square_panel := Panel}) ->
    wxPanel:destroy(Panel),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

paint_square(#{square_panel := Panel,
	       image := PieceImage,
	       brush := Brush,
	       selected_brush := SelectedBrush,
	       selected := Selected}) ->
    Paint = fun(_DC, none) -> ok;
	       (DC, Image) -> 
		    {W,H} = wxPanel:getSize(Panel),
		    ScaledImage = wxImage:scale(Image,W,H),
		    PieceBitmap = wxBitmap:new(ScaledImage),
		    wxDC:drawBitmap(DC, PieceBitmap, {0,0}),
		    wxImage:destroy(ScaledImage),
		    wxBitmap:destroy(PieceBitmap)
	    end,
    
    DC = wxBufferedPaintDC:new(Panel),
    wxDC:setPen(DC, ?wxTRANSPARENT_PEN),
    wxDC:setBackground(DC, case Selected of 
			       true -> SelectedBrush; 
			       false -> Brush
			   end),
    wxDC:clear(DC),
    Paint(DC, PieceImage),
    wxBufferedPaintDC:destroy(DC).

