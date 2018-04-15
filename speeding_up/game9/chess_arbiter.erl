-module(chess_arbiter).
-behaviour(gen_server).

-export([start_link/0, want_to_play/4]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(START_DELAY, 3).

want_to_play(Name, MinDuration, MaxDuration, MaxWait) ->
    gen_server:call({global, ?MODULE}, 
		    {want_to_play, Name, MinDuration, MaxDuration, MaxWait}).
start_link() ->
    gen_server:start_link({global, ?SERVER}, ?MODULE, [], []).

init([]) ->
    {ok, #{p1_map => undefined, 
	   p2_map => undefined, 
	   white_player_pid => undefined, 
	   black_player_pid => undefined, 
	   game_state => no_game}}.

handle_call({want_to_play, Name, MinDuration, MaxDuration, MaxWait}, 
	    {Pid1, _Tag}, 
	    State = #{p1_map := undefined, p2_map := undefined}) ->
    erlang:send_after(MaxWait * 60000, self(), wait_end1),
    erlang:monitor(process, Pid1),
    {reply, nomatch, State#{p1_map => #{name => Name,
					pid => Pid1, 
					min_duration => MinDuration,
					max_duration => MaxDuration}}};

handle_call({want_to_play, Name2, MinDuration2, MaxDuration2, MaxWait}, 
	    {Pid2, _Tag}, 
	    State = #{p1_map := #{pid := Pid1,
				  name := Name1, 
				  min_duration := MinDuration1,
				  max_duration := MaxDuration1}}) ->
    case (MinDuration1 =< MaxDuration2) andalso
	(MinDuration1 >= MinDuration2) of
	true -> 
	    {White, Black} = 
		start_game(MinDuration1, {Pid1,Name1}, {Pid2,Name2}),
	    {reply, ok, State#{game_state => started, 
			       p1_map => undefined,
			       p2_map => undefined,
			       white_player_pid => White, 
			       black_player_pid => Black}};
	false ->
	    case (MinDuration2 =< MaxDuration1) andalso
		(MinDuration2 >= MinDuration1) of
		true -> 
		    {White, Black} = 
			start_game(MinDuration2, {Pid1,Name1}, {Pid2,Name2}),
		    {reply, ok, State#{game_state => started,
				       p1_map => undefined,
				       p2_map => undefined,
				       white_player_pid => White, 
				       black_player_pid => Black}};
		false ->
		    erlang:send_after(MaxWait * 60000, self(), wait_end2),
		    erlang:monitor(process, Pid2),
		    {reply, nomatch, 
		     State#{p2_map => #{name => Name2, 
					pid => Pid2, 
					min_duration => MinDuration2,
					max_duration => MaxDuration2}}}
	    end
    end;

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(wait_end1, State = #{game_state := no_game, 
				 p1_map := #{pid := P1}, 
				 p2_map := P2Map}) ->
    P1 ! nomatch,
    {noreply, State#{p1_map => P2Map, p2_map => undefined}};
handle_info(wait_end1, State) ->
    {noreply, State};

handle_info(wait_end2, State = #{game_state := no_game, 
				 p2_map := #{pid := P2}}) ->
    P2 ! nomatch,
    {noreply, State#{p2_map => undefined}};
handle_info(wait_end2, State) ->
    {noreply, State};

handle_info({play, PlayerPid, Colour, Layout}, State) ->
    PlayerPid ! {role, Colour, Layout},
    PlayerPid ! {play, white},
    {noreply, State};

handle_info({we_selected, white, SquareLocation}, 
	    State = #{black_player_pid := Black}) ->
    Black ! {they_selected, SquareLocation},
    {noreply, State};
handle_info({we_selected, black, SquareLocation}, 
	    State = #{white_player_pid := White}) ->
    White ! {they_selected, SquareLocation},
    {noreply, State};

handle_info({we_moved, white, FromLocation, ToLocation}, 
	    State = #{black_player_pid := Black}) ->
    Black ! {they_moved, FromLocation, ToLocation},
    {noreply, State};
handle_info({we_moved, black, FromLocation, ToLocation}, 
	    State = #{white_player_pid := White}) ->
    White ! {they_moved, FromLocation, ToLocation},
    {noreply, State};

handle_info({{we_promoted_a_pawn, PromotedRole}, white, FromLocation, ToLocation}, State = #{black_player_pid := Black}) ->
    Black ! {{they_promoted_a_pawn, PromotedRole}, FromLocation, ToLocation},
    {noreply, State};
handle_info({{we_promoted_a_pawn, PromotedRole}, black, FromLocation, ToLocation}, State = #{white_player_pid := White}) ->
    White ! {{they_promoted_a_pawn, PromotedRole}, FromLocation, ToLocation},
    {noreply, State};

handle_info({done, Colour}, State = #{black_player_pid := Black, white_player_pid := White}) ->
    Opponent = chess_utils:opponent(Colour),
    Black ! {play, Opponent},
    White ! {play, Opponent},
    {noreply, State};

handle_info({end_game, white, Why}, State = #{black_player_pid := Black}) -> 
    Black ! {end_game, white, Why},
    {noreply, State#{p1_map => undefined, p2_map => undefined}};
handle_info({end_game, black, Why}, State = #{white_player_pid := White}) -> 
    White ! {end_game, black, Why},
    {noreply, State#{p1_map => undefined, p2_map => undefined}};

handle_info({quit, white}, State = #{black_player_pid := Black}) -> 
    Black ! quit,
    {stop, normal, State};
handle_info({quit, black}, State = #{white_player_pid := White}) -> 
    White ! quit,
    {stop, normal, State};

handle_info({'DOWN', _, process, Pid, _}, 
	    State = #{p1_map := #{pid := Pid}, p2_map := P2Map}) ->
    {noreply, State#{p1_map => P2Map, p2_map => undefined}};
handle_info({'DOWN', _, process, Pid, _}, 
	    State = #{p2_map := #{pid := Pid}}) ->
    {noreply, State#{p2_map => undefined}};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


start_game(Duration, {Pid1,Name1}, {Pid2,Name2}) ->
    {Colour1, Colour2} = case rand:uniform(2) of
			     1 -> {white, black};
			     2 -> {black, white}
			 end,
    Pid1 ! {prepare_to_play, Name2, Duration, ?START_DELAY}, 
    Pid2 ! {prepare_to_play, Name1, Duration, ?START_DELAY}, 

%%    erlang:send_after(?START_DELAY * 1000, Pid1, {play, Colour1}),
%% won't work if Pid1 is on a remote node!! 
%% If Dest is a pid(), it must be a pid() of a process created on the current runtime system instance. This process has either terminated or not. If Dest is an atom(), it is interpreted as the name of a locally registered process. The process referred to by the name is looked up at the time of timer expiration. No error is returned if the name does not refer to a process.

    Layout = chess_utils:init_board(),
    erlang:send_after(?START_DELAY * 1000, self(), {play, Pid1, Colour1, Layout}),
    erlang:send_after(?START_DELAY * 1000, self(), {play, Pid2, Colour2, Layout}),

    case Colour1 of 
	white -> {Pid1, Pid2};
	black -> {Pid2, Pid1}
    end.
    
