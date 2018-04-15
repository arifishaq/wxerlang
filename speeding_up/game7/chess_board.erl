-module(chess_board).

-behaviour(wx_object).
-export([start_link/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, 
	 handle_event/2, 
	 terminate/2, code_change/3]).

-include_lib("wx/include/wx.hrl").

-define(SQUARE, chess_square).
-define(UTILS, chess_utils).
-define(WHITE, {140,220,120}).
-define(BLACK, {80,160,60}).
-define(SELECTED_COLOUR, {238,232,170}).

start_link(PlayerFrame, PlayerPid) -> 
    wx_object:start_link(?MODULE, [PlayerFrame, PlayerPid], []).

init([PlayerFrame, PlayerPid]) ->
    Board = wxPanel:new(PlayerFrame, [{style, ?wxFULL_REPAINT_ON_RESIZE}]), 
    wxPanel:setBackgroundStyle(Board, ?wxBG_STYLE_CUSTOM),

    Grid = wxGridSizer:new(8,8,0,0),
    wxPanel:setSizer(Board, Grid),

    ActiveWhiteBrush = wxBrush:new(?WHITE),
    ActiveBlackBrush = wxBrush:new(?BLACK),
    InactiveWhiteBrush = wxBrush:new({150,150,150}),
    InactiveBlackBrush = wxBrush:new({100,100,100}),
    SelectedBrush = wxBrush:new(?SELECTED_COLOUR),
    BackgroundBrush = wxBrush:new(wxPanel:getBackgroundColour(Board)),

    Layout = ?UTILS:init_board(),
    ImageMap = ?UTILS:load_images(),

    MkSquare = 
	fun(C,R) ->
		SquareColour = ?UTILS:square_colour(C,R),
		ActiveBrush = case SquareColour of 
				  white -> ActiveWhiteBrush;
				  black -> ActiveBlackBrush
			      end,
		InactiveBrush = case SquareColour of
				    white -> InactiveWhiteBrush;
				    black -> InactiveBlackBrush
				end,
		Square = ?SQUARE:start_link(
			    {C,R},
			    self(), 
			    Board, 
			    ActiveBrush, 
			    InactiveBrush,
			    SelectedBrush), 
		{{C,R}, Square}
	end,

    MkBoard = 
	fun(SquareMap, Seq) ->		
		wxSizer:clear(Grid),
		[wx_object:stop(Sq) || Sq <- maps:values(SquareMap)],
		SquareList = [MkSquare(C,R) || R <- Seq, C <- Seq],
		[wxSizer:add(Grid, Square, [{flag, ?wxEXPAND}])
		 || {_, Square} <- SquareList],
		SquareList
	end,

    MkWhiteBoard = fun(ChessBoard, SquareMap) -> 
			   Squares = MkBoard(SquareMap, lists:seq(0,7)),
			   wxPanel:layout(ChessBoard),
			   Squares
		   end,
    MkBlackBoard = fun(ChessBoard, SquareMap) -> 
			   Squares = MkBoard(SquareMap, lists:seq(7,0,-1)),
			   wxPanel:layout(ChessBoard),
			   Squares
		   end,
 
    SquareMap = maps:from_list(MkWhiteBoard(Board, #{})),
    SquarePidMap = maps:map(fun(_,V) -> wx_object:get_pid(V) end, SquareMap),
    layout_pieces(Layout, ImageMap, SquarePidMap),

    State = #{player_frame => PlayerFrame,
	      player_pid => PlayerPid,
	      board => Board, 
	      layout => Layout,
	      make_white_board => MkWhiteBoard,
	      make_black_board => MkBlackBoard,
	      image_map => ImageMap,
	      {white, active_brush} => ActiveWhiteBrush,
	      {white, inactive_brush} => InactiveWhiteBrush,
	      {black, active_brush} => ActiveBlackBrush,
	      {black, inactive_brush} => InactiveBlackBrush,
	      selected_brush => SelectedBrush,
	      background_brush => BackgroundBrush,
	      square_map => SquareMap,
	      square_pid_map => SquarePidMap,
	      selected => none},

    {Board, State}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({propagate_event, Event}, State = #{player_pid := PlayerPid}) ->
    %% io:format("board propagating ~p~n", [Event]),
    PlayerPid ! {propagate_event, Event},
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

handle_event(#wx{} = _Wx, State) ->
    {noreply, State}.


terminate(_Reason, #{board := Board,
		     {white, active_brush} := ActiveWhiteBrush,
		     {white, inactive_brush} := InactiveWhiteBrush,
		     {black, active_brush} := ActiveBlackBrush,
		     {black, inactive_brush} := InactiveBlackBrush,		
		     selected_brush := SelectedBrush,
		     background_brush := BackgroundBrush,
		     image_map := ImageMap}) ->
    [wxBrush:destroy(B) 
     || B <- [ActiveWhiteBrush, ActiveBlackBrush, InactiveWhiteBrush, InactiveBlackBrush, BackgroundBrush, SelectedBrush]],
    [wxImage:destroy(I) || I <- maps:values(ImageMap)],
    wxPanel:destroy(Board),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


layout_pieces(Layout, ImageMap, SquarePidMap) ->
    lists:foreach( 
      fun({Location,Piece}) ->
	      Image = maps:get(Piece, ImageMap),
	      SquarePid = maps:get(Location, SquarePidMap),
	      SquarePid ! {image, Image}
      end,
      maps:to_list(Layout)).



