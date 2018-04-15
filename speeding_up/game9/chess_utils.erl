-module(chess_utils).

-compile(export_all).

-include_lib("wx/include/wx.hrl").

opponent(white) -> black;
opponent(black) -> white.

square_size(W,H) -> 
    ((min(W,H) div 8) div 2) * 2.

to_internal([File,Rank]) -> 
    {File - $a, $8 - Rank}.

from_internal({Column, Row}) -> 
    [Column + $a, $8 - Row].

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
    load_images("../images/").

load_images(ImagesDir) ->
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
    maps:map(
      fun(_K,V) -> 
	      wxImage:new(filename:join(ImagesDir, V), 
			  [{type, ?wxBITMAP_TYPE_PNG}]) 
      end,
      ImageFileNames).


mark(What, How, Which, SquaresMap) 
  when What =:= selectable; What =:= landable ->
    Set = fun({C,R}) -> 
		  Sq = maps:get({C,R}, SquaresMap),
		  wx_object:get_pid(Sq) ! {What, How},
		  Cursor = case How of 
			       false -> ?wxNullCursor;
			       true -> 
				   wxCursor:new(?wxCURSOR_HAND)
			   end,
		  wxWindow:setCursor(Sq, Cursor)
	  end,
    [Set({C,R}) || {C,R} <- Which];

mark(What, How, Which, SquaresMap) ->
    Set = fun({C,R}) -> 
		  Sq = maps:get({C,R}, SquaresMap),
		  wx_object:get_pid(Sq) ! {What, How}
	  end, 
    [Set({C,R}) || {C,R} <- Which].

get_squares(Colour, Layout) -> 
    maps:fold(fun({C,R}, {Col, _}, Acc) 
		    when Col =:= Colour -> 
		      [{C,R} | Acc];
		 (_,_, Acc) -> 
		      Acc
	      end, [], Layout).

update_map([], Map) -> Map;
update_map([{Key, none} | Changes], Map) ->     
    update_map(Changes, maps:remove(Key, Map));
update_map([{Key, Value} | Changes], Map) -> 
    update_map(Changes, Map#{Key => Value}).


occupancy({C,R}, none, Layout) when C >= 0, C < 8, R >= 0, R < 8 ->
    not maps:is_key({C,R}, Layout);
occupancy({C,R}, Kind, Layout) when C >= 0, C < 8, R >= 0, R < 8 ->
    case maps:get({C,R}, Layout, none) of
	{Kind, _} -> true;
	_ -> false
    end;
occupancy(_,_,_) -> no_such_square.
	     

vacant_or_opponent(Square, Dir, Opponent, Layout, FreeSquares) ->
    case neighbour(Square, Dir) of
	none -> FreeSquares;
	Neighbour -> 
	    case is_vacant(Neighbour, Layout) of
		true -> vacant_or_opponent(Neighbour, Dir, Opponent, Layout, 
					     [Neighbour | FreeSquares]);
		false -> 
		    case has_colour(Neighbour, Opponent, Layout) of
			true -> [Neighbour | FreeSquares];
			_ -> FreeSquares
		    end
	    end
    end.
    
	    
neighbour({_,0}, Dir) 
  when Dir =:= north; Dir =:= ne; Dir =:= nw -> none;
neighbour({_,7}, Dir) 
  when Dir =:= south; Dir =:= se; Dir =:= sw -> none;
neighbour({0,_}, Dir)
  when Dir =:= west; Dir =:= sw; Dir =:= nw -> none;
neighbour({7,_}, Dir) 
  when Dir =:= east; Dir =:= se; Dir =:= ne -> none;
neighbour({C,R}, north) -> {C,R-1};
neighbour({C,R}, south) -> {C,R+1};
neighbour({C,R}, east) -> {C+1,R};
neighbour({C,R}, west) -> {C-1,R};
neighbour({C,R}, ne) -> {C+1,R-1};
neighbour({C,R}, nw) -> {C-1,R-1};
neighbour({C,R}, se) -> {C+1,R+1};
neighbour({C,R}, sw) -> {C-1,R+1}.


is_vacant(Location, Layout) ->
     case occupancy(Location, none, Layout) of
	 no_such_square -> false;
	 IsVacant -> IsVacant
     end.
	      
is_white(Location, Layout) ->
     case occupancy(Location, white, Layout) of
	 no_such_square -> false;
	 IsWhite -> IsWhite
     end.
is_black(Location, Layout) ->
     case occupancy(Location, black, Layout) of
	 no_such_square -> false;
	 IsBlack -> IsBlack
     end.
has_colour(Location, Colour, Layout) ->
    case occupancy(Location, Colour, Layout) of
	no_such_square -> false;
	HasColour -> HasColour
    end.

is_piece(Piece, AtLocation, Layout) ->
    Piece =:= maps:get(AtLocation, Layout, none).


find_pieces(Piece, Layout = #{}) -> 
    find_pieces(Piece, maps:to_list(Layout), []);
find_pieces(Piece, LayoutList) when is_list(LayoutList) ->
    find_pieces(Piece, LayoutList, []).

find_pieces(Piece, [{Location, Piece} | Layout], Locations) ->
    find_pieces(Piece, Layout, [Location | Locations]);
find_pieces(Piece, [_ | Layout], Locations) ->
    find_pieces(Piece, Layout, Locations);
find_pieces(_Piece, [], Pieces) ->
    Pieces.


find_neighbour(Location, Dir, OfColour, Layout = #{}) ->
    case neighbour(Location, Dir) of
	none -> none;
	Neighbour -> 
	    case maps:get(Neighbour, Layout, none) of 
		{OfColour, Role} ->
		    {OfColour, Role};
		none ->
		    find_neighbour(Neighbour, Dir, OfColour, Layout);
		_ ->
		    none
	    end
    end.


first_match(MatchFun, [Candidate | OtherCandidates]) ->
    case MatchFun(Candidate) of 
	true -> true;
	false -> first_match(MatchFun, OtherCandidates)
    end;
first_match(_, []) ->
    false.


