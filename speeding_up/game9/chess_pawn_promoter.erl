-module(chess_pawn_promoter).

-behaviour(wx_object).
-include_lib("wx/include/wx.hrl").

-export([start_link/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 handle_event/2, terminate/2, code_change/3]).

start_link(Colour, Images) ->
    wx_object:start_link(?MODULE, [Colour, Images], []).

init([Colour, Images]) ->
    Dialog = wxDialog:new(wx:null(), ?wxID_ANY, "Pawn Promotion", [{style, ?wxCAPTION bor ?wxCLOSE_BOX}]),
    Sz = wxBoxSizer:new(?wxHORIZONTAL),
    MkBitmap = fun(I) ->
		       ScaledImage = wxImage:scale(I, 128, 128),
		       Bitmap = wxBitmap:new(ScaledImage),
		       wxImage:destroy(ScaledImage),
		       Bitmap
	       end,
    Bitmaps = [{Id, MkBitmap(Image)} || 
		  {Id, Image} <- 
		      [{1001, maps:get({Colour, rook}, Images)},
		       {1002, maps:get({Colour, knight}, Images)},
		       {1003, maps:get({Colour, bishop}, Images)},
		       {1004, maps:get({Colour, queen}, Images)}]],
    
    [wxSizer:add(Sz, wxBitmapButton:new(Dialog, Id, Bitmap), 
		 [{flag, ?wxALL}, {border, 5}]) || 
	{Id, Bitmap} <- Bitmaps],

    wxDialog:setSizer(Dialog, Sz),
    wxSizer:setSizeHints(Sz, Dialog),
    wxDialog:connect(Dialog, command_button_clicked),
    wxDialog:show(Dialog),

    {Dialog, #{dialog => Dialog, 
	       choice => undefined, 
	       bitmaps => Bitmaps}}.

handle_event(#wx{id = Id, event = #wxCommand{type = command_button_clicked}}, 
	     State = #{dialog := Dialog, from := From}) ->
    Choice = case Id of 
		 1001 -> rook;
		 1002 -> knight;
		 1003 -> bishop;
		 1004 -> queen
	     end,
    wxDialog:hide(Dialog),
    case From of 
	undefined -> 
	    {noreply, State#{choice => Choice}};
	_ -> 
	    wx_object:reply(From, Choice),
	    {stop, normal, State}
    end;

handle_event(#wx{} = _Wx, State) ->
    {noreply, State}.

handle_call(get_role, From, State = #{choice := Choice}) ->
    case Choice of 
	undefined -> 
	    {noreply, State#{from => From}};
	_ -> 
	    {reply, Choice, State}
    end;

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, #{dialog := Dialog, bitmaps := Bitmaps}) ->
    [wxBitmap:destroy(Bitmap) || {_, Bitmap} <- Bitmaps],
    wxDialog:destroy(Dialog),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
