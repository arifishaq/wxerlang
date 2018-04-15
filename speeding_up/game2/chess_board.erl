-module(chess_board).

-behaviour(wx_object).
-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, 
	 handle_event/2, 
	 terminate/2, code_change/3]).

-include_lib("wx/include/wx.hrl").

-define(SQUARE, chess_square).
-define(UTILS, chess_utils).
-define(WHITE, {140,220,120}).
-define(BLACK, {80,160,60}).
-define(SELECTED_COLOUR, {238,232,170}).

start_link() -> 
    wx_object:start_link(?MODULE, [], []).

init([]) ->
    wx:new(),
    Frame = wxFrame:new(wx:null(), ?wxID_ANY, "chess_board"),
    MainSizer = wxBoxSizer:new(?wxVERTICAL),

    Board = wxPanel:new(Frame, [{style, ?wxFULL_REPAINT_ON_RESIZE}]), 
    wxPanel:setBackgroundStyle(Board, ?wxBG_STYLE_CUSTOM),

    Grid = wxGridSizer:new(8,8,0,0),
    wxPanel:setSizer(Board, Grid),

    WhiteBrush = wxBrush:new(?WHITE),
    BlackBrush = wxBrush:new(?BLACK),
    SelectedBrush = wxBrush:new(?SELECTED_COLOUR),
    BackgroundBrush = wxBrush:new(wxPanel:getBackgroundColour(Board)),

    Layout = ?UTILS:init_board(),
    ImageMap = ?UTILS:load_images(),

    MkSquare = 
	fun(C,R) ->
		SquareColour = ?UTILS:square_colour(C,R),
		Brush = case SquareColour of 
			    white -> WhiteBrush;
			    black -> BlackBrush
			end,
		Square = ?SQUARE:start_link(
			    {C,R},
			    self(), 
			    Board, 
			    Brush, 
			    SelectedBrush), 
		{{C,R}, Square}
	end,

    SquaresList = [MkSquare(C,R) || R <- lists:seq(0,7), C <- lists:seq(0,7)],
    [wxSizer:add(Grid, Square, [{flag, ?wxEXPAND}]) 
     || {_,Square} <- SquaresList],

    SquareMap = maps:from_list(SquaresList), 
    SquarePidMap = maps:map(fun(_,V) -> wx_object:get_pid(V) end, SquareMap),
    layout_pieces(Layout, ImageMap, SquarePidMap),

    wxSizer:add(MainSizer, Board, [{flag, ?wxEXPAND}, {proportion,1}]),    
    wxFrame:setSizer(Frame, MainSizer),

    wxFrame:show(Frame),
    {W,H} = wxFrame:getClientSize(Frame),
    wxPanel:setSize(Board, W, H),
    wxWindow:refresh(Frame),

    State = #{frame => Frame,
	      board => Board, 
	      layout => Layout,
	      image_map => ImageMap,
	      {white, brush} => WhiteBrush,
	      {black, brush} => BlackBrush,
	      selected_brush => SelectedBrush,
	      background_brush => BackgroundBrush,
	      square_map => SquareMap,
	      square_pid_map => SquarePidMap,
	      selected => none},

    {Frame, State}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

%% %% message from square to say a piece was selected
handle_info({we_selected, SquareLocation}, 
	    State = #{square_pid_map := SquarePidMap,
		      layout := Layout}) ->
    [Sq ! {landable, true} || Sq <- maps:values(SquarePidMap)],
    {Colour, _} = maps:get(SquareLocation, Layout),
    maps:fold(
      fun(Location, {C, _}, _AccIn) when C =:= Colour -> 
	      Pid = maps:get(Location, SquarePidMap),
	      Pid ! {selectable, false},
	      Pid ! {landable, false};
	 (_,_,_) ->
	      ok
      end,
      [], Layout),
    {noreply, State#{selected =>  SquareLocation}};

handle_info({we_moved, MovedToLocation},
	    State = #{selected := FromLocation,
		      square_pid_map := SquarePidMap,
		      image_map := ImageMap,
		      layout := Layout}) ->

    [SqPid ! {landable, false} || SqPid <- maps:values(SquarePidMap)],

    Piece = {Colour, _} = maps:get(FromLocation, Layout),
    Opponent = ?UTILS:opponent(Colour),

    maps:fold(
      fun(Location, {C, _}, _AccIn) when C =:= Opponent -> 
	      maps:get(Location, SquarePidMap) ! {selectable, true};
	 (_,_,_) ->
	      ok
      end,
      [], Layout),

    FromPid = maps:get(FromLocation, SquarePidMap),
    FromPid ! {selected, false},
    FromPid ! {image, none},
    PieceImage = maps:get(Piece, ImageMap),
    TargetPid = maps:get(MovedToLocation, SquarePidMap),
    TargetPid ! {image, PieceImage},

    {noreply, State#{selected => none,
		     layout => (maps:remove(FromLocation, Layout))#{
				 MovedToLocation => Piece}}};


handle_info({play, Colour}, State = #{layout := Layout, 
				      square_pid_map := SquarePidMap}) ->
    maps:fold(
      fun(Location, {C, _}, _AccIn) when C =:= Colour -> 
	      maps:get(Location, SquarePidMap) ! {selectable, true};
	 (_,_,_) ->
	      ok
      end,
      [], Layout),
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

handle_event(#wx{} = _Wx, State) ->
    {noreply, State}.

terminate(_Reason, #{{white, brush} := WhiteBrush,
		     {black, brush} := BlackBrush,
		     selected_brush := SelectedBrush,
		     background_brush := BackgroundBrush,
		     image_map := ImageMap}) ->
    [wxBrush:destroy(B) 
     || B <- [WhiteBrush, BlackBrush, BackgroundBrush, SelectedBrush]],
    [wxImage:destroy(I) || I <- maps:values(ImageMap)],
    wx:destroy().

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


layout_pieces(Layout, ImageMap, SquarePidMap) ->
    maps:fold(
      fun(Location,Piece,_) ->
	      Image = maps:get(Piece, ImageMap),
	      Pid = maps:get(Location, SquarePidMap),
	      Pid ! {image, Image}
      end,
      [],
      Layout).

