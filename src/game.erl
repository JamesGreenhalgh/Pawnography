-module(game).
-behaviour(gen_server).
-export([init/1, start_link/0, handle_call/3, code_change/3, handle_cast/2, handle_info/2, terminate/2, start_game/3]).
-export([take_turn/2, stop_game/1, get_board/1]).
-record(game, {board, white, black, next_player, turn}).

start_link() -> gen_server:start_link(?MODULE, [], []).
init([]) -> 
	io:format("game.erl:Game server started. Pid:~p", [self()]),
	{ok, []}.

terminate(Reason, State) -> 
	io:format("game.erl:Terminate!!~p", [Reason]),
	{ok, State}.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

%% Synchronous call
start_game(Pid, Player1, Player2) -> gen_server:call(Pid, {start, Player1, Player2}).
stop_game(Pid) -> gen_server:call(Pid, terminate).
%% This call is asynchronous
take_turn(Pid, Move) -> gen_server:call(Pid, {turn, Move}).
get_board(Pid) -> gen_server:cast(Pid, {board}).

handle_call({start, Player1, Player2}, _From, []) -> {reply, {ok, started}, make_game(Player1, Player2)};
handle_call(terminate, _From, State) -> {stop, normal, ok, State};

handle_call({turn, Move}, {Player, _}, State) ->
	%% We need to notify both players, right?
	%% The guy who made the move needs to know it was a valid move
	io:format("game.erl:PLAYER:~p~nMove:~p",[Player, Move]),
	{From, To} = Move,
	case board:make_move(State#game.board, colour_atom(State), From, To) of
		{ok, Board} -> 
			board:pretty_print_board(Board),
			%io:format("game.erl:Board:~p~nMove:~p",[Board, Move]),
			[player:update_player(X, Move, Board, opposite_colour(State)) || X <- [State#game.white, State#game.black]],
			%io:format("game.erl:After update_player:~p~nMove:~p~n",[Board, Move]),
			{reply,{ok,Board},State#game{
					board=Board, 
					next_player=opposite_colour(State),
					turn=State#game.turn
					}};
		{badmove, Description} ->
			io:format("game.erl:BAD MOVE"),
			{reply, {badmove, Description}, State}
	end;
handle_call({turn, _Move}, _Player, State) ->
	{reply, {badmove, not_players_turn}, State};
handle_call(BLA, From, State) ->
	io:format("game.erl:Bad handle call in game~p~nFrom:~p",[BLA, From]),
	{reply, ok, State}.


opposite_colour(State) when State#game.next_player =:= State#game.white -> State#game.black;
opposite_colour(State) when State#game.next_player =:= State#game.black -> State#game.white.
colour_atom(State) when State#game.next_player =:= State#game.black -> black;
colour_atom(State) when State#game.next_player =:= State#game.white -> white.
%player_pid(white, State) -> State#game.white;
%player_pid(black, State) -> State#game.black.

handle_cast(_Msg, State) ->
	{noreply, State}.
handle_info(Msg, State) ->
	io:format("game.erl:Unexpected message: ~p~n",[Msg]),
	{noreply, State}.


%%% Private functions
make_game(Player1, Player2) ->
	Board = board:get_board(),
	%{PPid1, _} = Player1,
	%{PPid2, _} = Player2,
	{ok, started} = player:start_player(Player1, white, Board),
	%{ok, started} = player:start_player(PPid1, white, Board),
	{ok, started} = player:start_player(Player2, black, Board),
	%{ok, started} = player:start_player(PPid2, black, Board),
	#game{board=Board, white=Player1, black=Player2, next_player=Player1, turn=0}.
	%#game{board=Board, white=PPid1, black=PPid2, next_player=PPid1, turn=0}.
