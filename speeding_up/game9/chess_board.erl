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
-define(ARBITER, chess_arbiter).

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
	      role => none,
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
	      selected => none,
	      under_check => false,
	      can_castle_king_side => true,
	      can_castle_queen_side => true,
	      castling => false,
	      en_passant => false},

    {Board, State}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(
  {role, Role, Layout}, 
  State = #{board := Board,
	    make_white_board := MkWhiteBoard,
	    make_black_board := MkBlackBoard,
	    square_map := OldSquareMap,
	    image_map := ImageMap}) ->
    SquaresMap = maps:from_list(case Role of 
				   white -> MkWhiteBoard(Board,OldSquareMap);
				   black -> MkBlackBoard(Board,OldSquareMap)
			       end),
    SquarePidMap = maps:map(fun(_,V) -> wx_object:get_pid(V) end, SquaresMap),
    layout_pieces(Layout, ImageMap, SquarePidMap),
    chess_utils:mark(active, true, maps:keys(SquaresMap), SquaresMap),    
    {noreply, State#{role => Role, 
		     layout => Layout,
		     square_map => SquaresMap,
		     square_pid_map => SquarePidMap}};


handle_info(prepare_to_select, 
	    State = #{role := Colour, 
		      player_pid := PlayerPid,
		      layout := Layout,
		      square_map := SquareMap,
		      square_pid_map := SquarePidMap,
		      under_check := UnderCheck,
		      castling := Castling,
		      en_passant := EnPassant}) -> 
    SelectableSquares = 
	case Castling of 
	    {true, RookLocation} -> [RookLocation];
	    false -> 
		SameColoured = chess_utils:get_squares(Colour, Layout),
		chess_rules:get_movable(SameColoured, Layout, EnPassant)
	end,
    
    case SelectableSquares of 
	[] ->
	    %% if we can't select any square, it's either a checkmate or a draw
	    case UnderCheck of 
		false -> 
		    PlayerPid ! {we_end, draw};
		{true, KingLocation} -> 
		    %% not intuitive, but we need to remove the "under check" highlighting
		    maps:get(KingLocation, SquarePidMap) ! {under_check, false},
		    PlayerPid ! {we_end, checkmate}
	    end;
	_ ->
	    chess_utils:mark(selectable, true, SelectableSquares, SquareMap)
    end,
    {noreply, State};

handle_info(
  {we_selected, SquareLocation}, 
  State = #{player_pid := PlayerPid,
	    square_map := SquareMap,
	    layout := Layout, 
	    castling := Castling,
	    under_check := UnderCheck,
	    en_passant := EnPassant,
	    can_castle_king_side := KingSideCastle,
	    can_castle_queen_side := QueenSideCastle}) ->

    PlayerPid ! {we_selected, SquareLocation},

    chess_utils:mark(selectable, false, maps:keys(SquareMap), SquareMap),
    TargetSquares = 
	case Castling of 
	    {true, {0,R}} -> [{3,R}];
	    {true, {7,R}} -> [{5,R}];
	    false -> 
		case UnderCheck of 
		    false -> 
			chess_rules:eligible_squares(SquareLocation, Layout, EnPassant, KingSideCastle, QueenSideCastle);
		    _ ->
			chess_rules:eligible_squares(SquareLocation, Layout, EnPassant, false, false)
		end
	end,
    chess_utils:mark(landable, true, TargetSquares, SquareMap),    
    {noreply, State#{selected =>  SquareLocation}};

%% be informed of the opponent's action
handle_info({they_selected, SquareLocation}, State = #{square_pid_map := SquarePidMap}) -> 
    maps:get(SquareLocation, SquarePidMap) ! {selected, true},
    {noreply, State};

handle_info({we_moved, MovedToLocation},
	    State = #{player_pid := PlayerPid,
		      under_check := UnderCheck,
		      %% border_panel := BorderPanel,
		      board := Board,
		      selected := FromLocation,
		      square_map := SquareMap,
		      square_pid_map := SquarePidMap}) ->

    chess_utils:mark(landable, false, maps:keys(SquareMap), SquareMap),
    case UnderCheck of 
	{true, KingLocation} -> 
	    maps:get(KingLocation, SquarePidMap) ! 
		{under_check, false};
	false -> ok
    end,
 
    wxWindow:refresh(Board),
    
    SelectedSquarePid = maps:get(FromLocation, SquarePidMap),
    MovedToSquarePid = maps:get(MovedToLocation, SquarePidMap),

    SelectedSquarePid ! {selected, false},

    {MessageForPlayer,
     TargetImage,
     RemovedImageLocations,
     UpdatedState} = update_board_config(MovedToLocation, State),
    
    PlayerPid ! {MessageForPlayer, FromLocation, MovedToLocation},
    MovedToSquarePid ! {image, TargetImage},

    [maps:get(SqLoc, SquarePidMap) ! {image,none} || 
	SqLoc <- RemovedImageLocations],

    {noreply, UpdatedState#{under_check => false}};


%% receive opponent's move
handle_info({they_moved, FromLocation = {FC,FR}, ToLocation = {TC,TR}}, 
	    State = #{role := MyColour,
		      square_pid_map := SquarePidMap,
		      image_map := ImageMap,
		      layout := Layout}) ->
    SelectedPiece = {_, PieceRole} = maps:get(FromLocation, Layout),
    FromSquarePid = maps:get(FromLocation, SquarePidMap),
    SelectedPieceImage = maps:get(SelectedPiece,ImageMap),

    ToSquarePid = maps:get(ToLocation, SquarePidMap),

    %% if they moved a pawn obliquely and the target location was vacant, 
    %% they captured en passant, and the piece to remove was that captured pawn
    ModifiedLayout = maps:remove(
		      FromLocation, 
		      maps:put(ToLocation, SelectedPiece, Layout)), 
    TookEnPassant = 
	PieceRole =:= pawn andalso 
	TC =/= FC andalso 
	maps:get(ToLocation, Layout, none) =:= none,
    UpdatedLayout = case TookEnPassant of 
			true -> maps:remove({TC, FR}, ModifiedLayout);
			false -> ModifiedLayout
		    end,

    FromSquarePid ! {image, none},
    [maps:get({TC,FR}, SquarePidMap) ! {image,none} || 
	TookEnPassant],
    FromSquarePid ! {selected, false},
    ToSquarePid ! {image, SelectedPieceImage},

    EnPassant = 
	if (PieceRole =:= pawn) andalso (abs(FR - TR) =:= 2) -> {true, FC};
	   true -> false
	end,

    IsUnderCheck = chess_rules:is_under_check(MyColour, UpdatedLayout),
    case IsUnderCheck of
	{true, KingLocation} -> 
	    maps:get(KingLocation, SquarePidMap) ! 
		{under_check, true};
	_ -> ok
    end,

    {noreply, State#{selected => none,
		     under_check => IsUnderCheck,
		     layout => UpdatedLayout,
		     en_passant => EnPassant}};

%% opponent promoted a pawn
handle_info({{they_promoted_a_pawn,PromotedRole}, FromLocation, ToLocation}, 
	    State = #{role := MyColour,
		      square_pid_map := SquarePidMap,
		      image_map := ImageMap,
		      layout := Layout}) ->
    PromotedPiece = {chess_utils:opponent(MyColour), PromotedRole},
    PromotedPieceImage = maps:get(PromotedPiece, ImageMap),
    FromSquarePid = maps:get(FromLocation, SquarePidMap),
    ToSquarePid = maps:get(ToLocation, SquarePidMap),

    FromSquarePid ! {image, none},
    FromSquarePid ! {selected, false},
    ToSquarePid ! {image, PromotedPieceImage},

    ModifiedLayout = maps:remove(
		      FromLocation, 
		      maps:put(ToLocation, PromotedPiece, Layout)), 
    {noreply, State#{selected => none,
		     layout => ModifiedLayout}};


handle_info({end_game, Colour, Why}, 
	    State = #{role := MyColour, 
		      board := Board,
		      square_map := SquareMap}) ->
    Message = 
	if Colour =:= MyColour -> 
		case Why of 
		    time_up -> "Time up\nSorry, you lost";
		    draw -> "It's a draw";
		    checkmate -> 
			lists:flatten(io_lib:format("Checkmate\nSorry, you lost",[]))
		end;
	   Colour =/= MyColour ->
		case Why of 
		    time_up -> "Opponent's time up\nYou win";
		    draw -> "It's a draw";
		    checkmate -> 
			lists:flatten(io_lib:format("Checkmate\nYou win",[]))
		end
	end,

    MD = wxMessageDialog:new(Board, Message, [{style, ?wxOK bor ?wxICON_EXCLAMATION}]),
    wxMessageDialog:showModal(MD),
    wxMessageDialog:destroy(MD),

    chess_utils:mark(active, false, maps:keys(SquareMap), SquareMap),    
    wxWindow:refresh(Board),	
    {noreply, State};
			  

handle_info({end_game, opponent_quit}, State = #{square_map := SquareMap}) ->
    chess_utils:mark(active, false, maps:keys(SquareMap), SquareMap),    
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
    catch [wxBrush:destroy(B) 
     || B <- [ActiveWhiteBrush, ActiveBlackBrush, InactiveWhiteBrush, InactiveBlackBrush, BackgroundBrush, SelectedBrush]],
    catch [wxImage:destroy(I) || I <- maps:values(ImageMap)],
    catch wxPanel:destroy(Board),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

update_board_config(MovedToLocation, 
		    Config = #{selected := FromLocation, layout := Layout}) ->
    SelectedPiece = maps:get(FromLocation, Layout),
    update_board_config(SelectedPiece, MovedToLocation, Config).

%% rook, that may be castling king side
update_board_config({_,rook} = SelectedPiece, MovedToLocation = {7,_}, 
		    Config = #{selected := FromLocation, 
			       image_map := ImageMap,			       
			       layout := Layout}) ->
    {we_moved,
     maps:get(SelectedPiece,ImageMap),
     [FromLocation],
     Config#{selected => none, 
	     castling => false,
	     can_castle_king_side => false,
	     layout => chess_utils:update_map(
			 [{FromLocation, none},
			  {MovedToLocation, SelectedPiece}],
			 Layout)}};

%% rook that may be castling queen side
update_board_config({_,rook} = SelectedPiece, MovedToLocation = {0,_}, 
		    Config = #{selected := FromLocation, 
			       image_map := ImageMap,			       
			       layout := Layout}) ->
    {we_moved,
     maps:get(SelectedPiece,ImageMap),
     [FromLocation],
     Config#{selected => none, 
	     castling => false,
	     can_castle_queen_side => false,
	     layout => chess_utils:update_map(
			 [{FromLocation, none},
			  {MovedToLocation, SelectedPiece}],
			 Layout)}};

%% king
update_board_config({_,king} = SelectedPiece, MovedToLocation = {TC,TR}, 
		    Config = #{selected := FromLocation = {FC,_FR}, 
			       image_map:= ImageMap,			       
			       layout := Layout}) ->
    {NowCastling, Message} = case {FC,TC} of 
				 {4,6} -> {{true, {7,TR}}, we_moved_to_castle};
				 {4,2} -> {{true, {0,TR}}, we_moved_to_castle};
				 _ -> {false, we_moved}
		  end,
    {Message,
     maps:get(SelectedPiece,ImageMap),
     [FromLocation],
     Config#{selected => none, 
	     castling => NowCastling,
	     can_castle_king_side => false,
	     can_castle_queen_side => false,
	     layout => chess_utils:update_map(
			 [{FromLocation, none},
			  {MovedToLocation, SelectedPiece}],
			 Layout)}};

%% pawn promotion
update_board_config({Colour,pawn}, MovedToLocation = {_, R}, 
		    Config = #{selected := FromLocation, 
			       image_map := ImageMap,			       
			       layout := Layout}) when R =:= 0; R =:= 7->
    %% select promoted piece
    Promoter = chess_pawn_promoter:start_link(Colour, ImageMap),
    PromotedRole = wx_object:call(wx_object:get_pid(Promoter), get_role),
    PromotedPiece = {Colour, PromotedRole},

    {{we_promoted_a_pawn, PromotedRole},
     maps:get(PromotedPiece,ImageMap),
     [FromLocation],
     Config#{selected => none, 
	     castling => false,
	     layout => chess_utils:update_map(
			 [{FromLocation, none},
			  {MovedToLocation, PromotedPiece}],
			 Layout)}};


%% pawn, that may have captured en_passant
update_board_config({_,pawn} = SelectedPiece, MovedToLocation = {TC,_}, 
		    Config = #{selected := FromLocation = {FC,FR}, 
			       image_map := ImageMap,			       
			       layout := Layout}) ->
    TookEnPassant = 
	TC =/= FC andalso 
	maps:get(MovedToLocation, Layout, none) =:= none,

    {we_moved,
     maps:get(SelectedPiece,ImageMap),
     [{TC,FR} || TookEnPassant] ++ [FromLocation],
     Config#{selected => none, 
	     castling => false,
	     layout => chess_utils:update_map(
			 [{FromLocation, none},
			  {MovedToLocation, SelectedPiece}] 
			 ++ [{{TC,FR}, none} || TookEnPassant],
			 Layout)}};


update_board_config(SelectedPiece, MovedToLocation, 
		    Config = #{selected := FromLocation, 
			       image_map := ImageMap,			       
			       layout := Layout}) ->
    {we_moved,
     maps:get(SelectedPiece,ImageMap),
     [FromLocation],
     Config#{selected => none, 
	     castling => false,
	     layout => chess_utils:update_map(
			 [{FromLocation, none},
			  {MovedToLocation, SelectedPiece}],
			 Layout)}}.

layout_pieces(Layout, ImageMap, SquarePidMap) ->
    lists:foreach( 
      fun({Location,Piece}) ->
	      Image = maps:get(Piece, ImageMap),
	      SquarePid = maps:get(Location, SquarePidMap),
	      SquarePid ! {image, Image}
      end,
      maps:to_list(Layout)).



