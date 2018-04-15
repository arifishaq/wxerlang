-module(chess_utils).
-compile(export_all).

-include_lib("wx/include/wx.hrl").

square_size(W,H) -> 
    ((min(W,H) div 8) div 2) * 2.

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


opponent(white) -> black;
opponent(black) -> white.


mark(What, How, Which, Squares) 
  when What =:= selectable; What =:= landable ->
    Set = fun({C,R}) -> 
		  Sq = maps:get({C,R}, Squares),
		  wx_object:get_pid(Sq) ! {What, How},
		  Cursor = case How of 
			       false -> ?wxNullCursor;
			       true -> wxCursor:new(?wxCURSOR_HAND)
			   end,
		  wxWindow:setCursor(Sq, Cursor)
	  end,
    [Set({C,R}) || {C,R} <- Which].
