-module(buffered_dc).

-behaviour(wx_object).
-include_lib("wx/include/wx.hrl").

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, 
	 handle_event/2, handle_sync_event/3,
	 terminate/2, code_change/3]).

start_link() ->
    wx_object:start_link(?MODULE, [], []).

init([]) ->
    wx:new(),
    Frame = wxFrame:new(wx:null(), ?wxID_ANY, "chessboard"),
    Sizer = wxBoxSizer:new(?wxVERTICAL),
    Panel = wxPanel:new(Frame,[{size, {320,320}},
			       {style, ?wxFULL_REPAINT_ON_RESIZE}]),
    wxPanel:setBackgroundStyle(Panel, ?wxBG_STYLE_CUSTOM),
    wxSizer:add(Sizer, Panel, [{proportion, 1}, 
			       {flag, ?wxEXPAND bor ?wxALL}, 
			       {border, 5}]), 
    wxFrame:setSizer(Frame, Sizer),
    wxSizer:setSizeHints(Sizer, Frame),
    wxPanel:connect(Panel, paint, [callback]),
    wxPanel:connect(Panel, erase_background, [callback]),
    wxPanel:connect(Panel, left_down),

    White = {140,220,120},
    Black = {80,160,60},
    State = #{frame => Frame,
	      panel => Panel,
	      layout => init_board(),
	      image_map => load_images(),
	      white_brush => wxBrush:new(White),
	      black_brush => wxBrush:new(Black),
	      selected_brush => wxBrush:new({238,232,170}),
	      selected => none},

    wxFrame:show(Frame),
    wxFrame:refresh(Frame),
    
    {Panel, State}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

handle_event(#wx{event=#wxClose{}}, State) -> 
    {stop, normal, State};

handle_event(#wx{event=#wxMouse{leftDown=true,x=X,y=Y}}, 
	     State = #{panel := Panel, 
		       layout := Layout,
		       selected := none}) ->
    {C,R} = where(X,Y,Panel),
    case maps:get({C,R}, Layout, none) of 
	none -> 
		{noreply, State};
	_ -> 
		wxPanel:refresh(Panel),
	     	{noreply, State#{selected => {C,R}}}
    end;

handle_event(#wx{event=#wxMouse{leftDown=true,x=X,y=Y}}, 
	     State = #{panel := Panel, 
		       layout := Layout,
		       selected := Selected}) ->
    {C,R} = where(X,Y,Panel),
    Piece = maps:get(Selected, Layout),
    NewLayout = maps:put({C,R}, Piece, maps:remove(Selected, Layout)),
    wxPanel:refresh(Panel),
    {noreply, State#{layout => NewLayout,
		     selected => none}};

handle_event(#wx{}, State) ->
    {noreply, State}.

handle_sync_event(#wx{event=#wxErase{}}, _, _State) -> 
    ok;
handle_sync_event(#wx{event=#wxPaint{}}, _, State) ->
    paint_board(State).

terminate(_Reason, #{black_brush := BlackBrush,
		     white_brush := WhiteBrush,
		     image_map := Images}) ->
    wxBrush:destroy(BlackBrush), 
    wxBrush:destroy(WhiteBrush),
    [wxImage:destroy(I) || I <- maps:values(Images)],
    wx:destroy().

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

square_size(W,H) -> 
    ((min(W,H) div 8) div 2) * 2.

%% to_internal([File,Rank]) -> 
%%     {File - $A, $8 - Rank}.

%% from_internal({Column, Row}) -> 
%%     [Column + $A, $8 - Row].

rectangle(Column,Row,SquareSize) -> 
    {Column * SquareSize, Row * SquareSize, SquareSize, SquareSize}.

init_board() ->
    Columns = lists:seq(0,7),
    BlackPieces = [{black,rook}, {black,knight}, {black,bishop}, {black,queen}, 
		   {black,king}, {black,bishop}, {black,knight}, {black,rook}],
    WhitePieces = [{white,rook}, {white,knight}, {white,bishop}, {white,queen}, 
		   {white,king}, {white,bishop}, {white,knight}, {white,rook}],
    Row6 = [{{C,6}, {white,pawn}} || C <- Columns],	
    Row1 = [{{C,1}, {black,pawn}} || C <- Columns],	
    Row7 = [{{C,7}, lists:nth(C+1, WhitePieces)} || C <- Columns],
    Row0 = [{{C,0}, lists:nth(C+1, BlackPieces)} || C <- Columns],

    maps:from_list(Row0 ++ Row1 ++ Row6 ++ Row7).

square_colour(Col, Row) ->
    case ((Col + Row) rem 2) of
	0 -> white;
	1 -> black
    end.
	     
load_images() ->
    ImageFileNames = #{
      {black, rook} 	=> "black_rook.png",
      {black, knight} 	=> "black_knight.png",
      {black, bishop} 	=> "black_bishop.png",
      {black, queen} 	=> "black_queen.png",
      {black, king} 	=> "black_king.png",
      {black, pawn} 	=> "black_pawn.png",
      {white, rook} 	=> "white_rook.png",
      {white, knight}	=> "white_knight.png",
      {white, bishop} 	=> "white_bishop.png",
      {white, queen}	=> "white_queen.png",
      {white, king}	=> "white_king.png",
      {white, pawn}	=> "white_pawn.png"},
    maps:map(fun(_K,V) -> wxImage:new(
			    filename:join("../images", V), 
			    [{type, ?wxBITMAP_TYPE_PNG}]) end,
	     ImageFileNames).

paint_board(#{panel := Panel,
	      layout := Layout,
	      image_map := ImageMap,
	      white_brush := WhiteBrush,
	      black_brush := BlackBrush,
	      selected_brush := SelectedBrush,
	      selected := Selected}) ->
    {W,H} = wxPanel:getSize(Panel),
    SquareSize = square_size(W,H),

    PaintSquare = 
	fun(DC,C,R) ->
		Brush = 
		    case Selected of 
			{C,R} -> 
			    SelectedBrush;
			_ -> 
			    case square_colour(C,R) of
				black -> BlackBrush;
				white -> WhiteBrush
			    end
		    end,
		Rectangle = rectangle(C,R,SquareSize),		
		wxDC:setBrush(DC,Brush),
		wxDC:drawRectangle(DC, Rectangle),
		
		case maps:get({C,R}, Layout, none) of
		    none -> ok;
		    Piece ->
			{X,Y,SW,SH} = Rectangle,
			Image = wxImage:scale(maps:get(Piece, ImageMap),SW,SH),
			PieceBitmap = wxBitmap:new(Image),
			wxDC:drawBitmap(DC, PieceBitmap, {X,Y}),
			wxImage:destroy(Image),
			wxBitmap:destroy(PieceBitmap)
		end		    
	end,
    
    DC = wxBufferedPaintDC:new(Panel),
    wxDC:setPen(DC, ?wxTRANSPARENT_PEN),
    Seq0to7 = lists:seq(0,7),
    [PaintSquare(DC,C,R) || R <- Seq0to7, C <- Seq0to7], 
    wxBufferedPaintDC:destroy(DC).

where(X,Y,Panel) -> 
	{W,H} = wxPanel:getSize(Panel), 
	SquareSize = square_size(W,H),
	{X div SquareSize, Y div SquareSize}.
