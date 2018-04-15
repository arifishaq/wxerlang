-module(chess_rules).

-export([get_movable/3, eligible_squares/5, is_under_check/2,
	is_under_check_from_direction/4]).

    
eligible_squares(Square, Layout, EnPassant, CanCastleKingSide, CanCastleQueenSide) ->
    case maps:get(Square, Layout, none) of
	%% none -> []; %% this should not occur
	{white, Role} -> eligible_squares(Square, Role, black, Layout, EnPassant, CanCastleKingSide, CanCastleQueenSide);
	{black, Role} -> eligible_squares(Square, Role, white, Layout, EnPassant, CanCastleKingSide, CanCastleQueenSide)
    end.
	     
%% BLACK PAWN
eligible_squares({C,1}, pawn, white, Layout, _EnPassant, _CanCastleKingSide, _CanCastleQueenSide) ->
    SouthSquares = case chess_utils:is_vacant({C,2}, Layout) of
		       true -> 
			   Row3 = case chess_utils:is_vacant({C,3}, Layout) of
				      true -> [{C,3}];
				      false -> []
				  end,
			   [{C,2} | Row3];		       
		       _ -> []
		   end,
    SESquares = case chess_utils:is_white({C+1,2}, Layout) of
		    true -> [{C+1,2}];
		    _ -> []
		end,
    SWSquares = case chess_utils:is_white({C-1,2}, Layout) of
		    true -> [{C-1,2}];
		    _ -> []
		end,
    ToLocations = SouthSquares ++ SESquares ++ SWSquares,
    get_non_checking_moves({black,pawn}, {C,1}, ToLocations, Layout);


eligible_squares({C,R}, pawn, white, Layout, EnPassant, _CanCastleKingSide, _CanCastleQueenSide) ->
    SouthSquares = case chess_utils:is_vacant({C,R+1}, Layout) of
		       true -> [{C,R+1}];
		       _ -> []
		   end,
    EnPassantSquares =  case EnPassant of 
			    {true, File} when R =:= 4 ->  
				[{File,5} || abs(C - File) =:= 1];
			    _ -> []
			end,
    
    SESquares = case chess_utils:is_white({C+1,R+1}, Layout) of
		    true -> [{C+1,R+1}];
		    false -> []
		end,
    SWSquares = case chess_utils:is_white({C-1,R+1}, Layout) of
		    true -> [{C-1,R+1}];
		    false -> []
		end,
    ToLocations = SouthSquares ++ SESquares ++ SWSquares,
    get_non_checking_moves({black,pawn}, {C,1}, ToLocations, Layout) ++ 
	case EnPassantSquares of 
	    [{EPC,EPR}] -> get_non_checking_moves(
			      {black,pawn}, {C,R}, [{EPC,EPR}], 
			      maps:remove({EPC,R}, Layout));
	    [] -> []
	end;

%% WHITE PAWN
eligible_squares({C,6}, pawn, black, Layout, _EnPassant, _CanCastleKingSide, _CanCastleQueenSide) ->
    NorthSquares = case chess_utils:is_vacant({C,5}, Layout) of
		       true -> 
			   Row4 = case chess_utils:is_vacant({C,4}, Layout) of
				      true -> [{C,4}];
				      false -> []
				  end,
			   [{C,5} | Row4];		       
		       false -> []
		   end,
    NESquares = case chess_utils:is_black({C+1,5}, Layout) of
		    true -> [{C+1,5}];
		    false -> []
		end,
    NWSquares = case chess_utils:is_black({C-1,5}, Layout) of
		    true -> [{C-1,5}];
		    false -> []
		end,
    ToLocations = NorthSquares ++ NESquares ++ NWSquares,
    get_non_checking_moves({white,pawn}, {C,6}, ToLocations, Layout);

eligible_squares({C,R}, pawn, black, Layout, EnPassant, _CanCastleKingSide, _CanCastleQueenSide) ->
    NorthSquares = case chess_utils:is_vacant({C,R-1}, Layout) of
		       true -> [{C,R-1}];
		       false -> []
		   end,
    EnPassantSquares = case EnPassant of
			   {true, File} when R =:= 3 ->  
			       [{File,2} || abs(C - File) =:= 1];
			   _ -> []
		       end,

    NESquares = case chess_utils:is_black({C+1,R-1}, Layout) of
		    true -> [{C+1,R-1}];
		    false -> []
		end,
    NWSquares = case chess_utils:is_black({C-1,R-1}, Layout) of
		    true -> [{C-1,R-1}];
		    false -> []
		end,
    ToLocations = NorthSquares ++ NESquares ++ NWSquares,
    get_non_checking_moves({white,pawn}, {C,R}, ToLocations, Layout) ++ 
	case EnPassantSquares of 
	    [{EPC,EPR}] -> get_non_checking_moves(
			      {white,pawn}, {C,R}, [{EPC,EPR}], 
			      maps:remove({EPC,R}, Layout));
	    [] -> []
	end;

%% ROOK
eligible_squares(Square, rook, Opponent, Layout, _EnPassant, _CanCastleKingSide, _CanCastleQueenSide) ->
    ToLocations = 
	lists:merge(
	  [chess_utils:vacant_or_opponent(Square, Dir, Opponent, Layout, []) 
	   || Dir <- [north, south, east, west]]),
    get_non_checking_moves(
      {chess_utils:opponent(Opponent),rook}, Square, ToLocations, Layout);

%% BISHOP
eligible_squares(Square, bishop, Opponent, Layout, _EnPassant, _CanCastleKingSide, _CanCastleQueenSide) ->
    ToLocations = 
	lists:merge(
	  [chess_utils:vacant_or_opponent(Square, Dir, Opponent, Layout, []) 
	   || Dir <- [ne, nw, se, sw]]),
    get_non_checking_moves(
      {chess_utils:opponent(Opponent),bishop}, Square, ToLocations, Layout);

%% QUEEN
eligible_squares(Square, queen, Opponent, Layout, _EnPassant, _CanCastleKingSide, _CanCastleQueenSide) ->
    ToLocations = 
	lists:merge(
	  [chess_utils:vacant_or_opponent(Square, Dir, Opponent, Layout, []) 
	   || Dir <- [north, south, east, west, ne, nw, se, sw]]),
    get_non_checking_moves(
      {chess_utils:opponent(Opponent),queen}, Square, ToLocations, Layout);

%% KNIGHT
eligible_squares({C,R}, knight, Opponent, Layout, _EnPassant, _CanCastleKingSide, _CanCastleQueenSide) ->
    CandidateSquares = [{C+1,R+2},{C+2,R+1},{C+2,R-1},{C+1,R-2},
			{C-1,R+2},{C-2,R+1},{C-2,R-1},{C-1,R-2}],
    Eligible = fun({Col,Row}) when Col >= 0, Col < 8, Row >= 0, Row < 8 -> 
		       case chess_utils:is_vacant({Col,Row}, Layout) orelse 
			   chess_utils:has_colour({Col,Row}, Opponent, Layout) of 
			   true -> {true, {Col,Row}};
			   false -> false
		       end;
		  (_) -> false
	       end,
    ToLocations = lists:filtermap(Eligible, CandidateSquares),
    get_non_checking_moves(
      {chess_utils:opponent(Opponent),knight}, {C,R}, ToLocations, Layout);


%% KING
eligible_squares({C,R}, king, Opponent, Layout, _EnPassant, CanCastleKingSide, CanCastleQueenSide) ->
    Eligible = fun({Col,Row}) when Col >= 0, Col < 8, Row >= 0, Row < 8 -> 
		       case chess_utils:is_vacant({Col,Row}, Layout) orelse 
			   chess_utils:has_colour({Col,Row}, Opponent, Layout) of
			   true -> {true, {Col,Row}};
			   false -> false
		       end;
		  (_) -> false
	       end,

    CandidateSquares = [{C,R+1}, {C,R-1}, {C+1,R+1}, {C+1,R}, {C+1,R-1},
			{C-1,R+1}, {C-1,R}, {C-1,R-1}],
    EligibleLocations = lists:filtermap(Eligible, CandidateSquares),
    PossibleMoves = get_non_checking_moves(
		      {chess_utils:opponent(Opponent),king}, {C,R}, 
		      EligibleLocations, Layout),
    
    %% verify king side castling
    KingSideLocations = 
	case CanCastleKingSide andalso 
	    chess_utils:is_vacant({C+1,R}, Layout) andalso
	    chess_utils:is_vacant({C+2,R}, Layout) andalso
	    lists:member({C+1,R}, PossibleMoves) of
	    true -> [{C+2,R}];
	    false -> []
	end,
    QueenSideLocations = 
	case CanCastleQueenSide andalso 
	    chess_utils:is_vacant({C-1,R}, Layout) andalso
	    chess_utils:is_vacant({C-2,R}, Layout) andalso
	    lists:member({C-1,R}, PossibleMoves) of 
	    true -> [{C-2,R}];
	    false -> []
	end,

    PossibleMoves ++ get_non_checking_moves(
		      {chess_utils:opponent(Opponent),king}, {C,R}, 
		      KingSideLocations ++ QueenSideLocations, Layout).



is_under_check(Colour, Layout) ->
    [KingLocation] = chess_utils:find_pieces({Colour, king}, Layout),
    Opponent = chess_utils:opponent(Colour),

    case is_under_check_from_a_knight(Opponent, KingLocation, Layout) of
	true -> {true, KingLocation};
	false -> 
	    case is_under_directional_check(Opponent, KingLocation, Layout) of
		true -> {true, KingLocation};
		false -> false
	    end
    end.
	     

is_under_check_from_a_knight(OpponentColour, _KingLocation = {C,R}, Layout) -> 
    CandidateSquares = [{C+1,R+2},{C+2,R+1},{C+2,R-1},{C+1,R-2},
			{C-1,R+2},{C-2,R+1},{C-2,R-1},{C-1,R-2}],
    IsUnderCheck = 
	fun(Location) ->
		chess_utils:is_piece({OpponentColour, knight}, Location, Layout)
	end,			 
    chess_utils:first_match(IsUnderCheck, CandidateSquares).	    

is_under_directional_check(OpponentColour, KingLocation, Layout) ->
    IsUnderCheck = 
	fun(Direction) ->
		is_under_check_from_direction(Direction, OpponentColour, KingLocation, Layout)
	end,
    chess_utils:first_match(IsUnderCheck, [north, ne, east, se, south, sw, west, nw]).

is_under_check_from_direction(Direction, OpponentColour, KingLocation={C,R}, Layout) ->
    case chess_utils:find_neighbour(KingLocation, Direction, OpponentColour, Layout) of 
	none -> 
	    false;
	{black,pawn} when OpponentColour =:= black ->
	    case maps:get({C-1,R-1}, Layout, none) of 
		{black, pawn} -> true;
		_ -> case maps:get({C+1,R-1}, Layout, none) of 
			 {black, pawn} -> true;
			 _ -> false
		     end
	    end;
	{white,pawn} when OpponentColour =:= white ->
	    case maps:get({C-1,R+1}, Layout, none) of 
		{white, pawn} -> true;
		_ -> case maps:get({C+1,R+1}, Layout, none) of 
			 {white, pawn} -> true;
			 _ -> false
		     end
	    end;
	{_Piece,Role} -> 
	    CheckingRoles = 
		#{north => [queen, rook],
		  ne => [queen, bishop], 
		  east => [queen, rook],
		  se => [queen, bishop], 
		  south => [queen, rook],
		  sw => [queen, bishop], 
		  west => [queen, rook],
		  nw => [queen, bishop]},
	    lists:member(Role, maps:get(Direction, CheckingRoles))
    end.
    

%% EnPassant :: {true, FC} | false.
get_movable(Which, Layout, EnPassant) ->
	lists:filter(
	  fun(FromLocation) -> 
		  case eligible_squares(FromLocation, Layout, EnPassant, false, false) of 
		      [] -> false; 
		      _ ->  true 
		  end
	  end, 
	  Which).



get_non_checking_moves(Piece, FromLocation, ToLocations, Layout) ->
    get_non_checking_moves(Piece, FromLocation, ToLocations, Layout, []).

get_non_checking_moves(Piece, FromLocation, [Target | ToLocations], Layout, Moves) ->
    case is_non_checking_move(Piece, FromLocation, Target, Layout) of
	true ->
	    get_non_checking_moves(Piece, FromLocation, ToLocations, Layout, [Target | Moves]); 
	false ->
	    get_non_checking_moves(Piece, FromLocation, ToLocations, Layout, Moves)
    end;
get_non_checking_moves(_Piece, _FromLocation, [], _Layout, Moves) ->
    Moves.

is_non_checking_move({Colour,Role}, FromLocation, ToLocation, Layout) ->
    UpdatedLayout = chess_utils:update_map(
		      [{FromLocation, none}, {ToLocation, {Colour,Role}}], 
		      Layout),
    case is_under_check(Colour, UpdatedLayout) of
	false -> true;
	{true, _} -> false
    end.
	     
