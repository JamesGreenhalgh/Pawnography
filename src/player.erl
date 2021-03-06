-module(player).
-behaviour(gen_server).
-export([init/1, start_link/0, handle_call/3, code_change/3, handle_cast/2, handle_info/2, terminate/2]).
-export([start_player/3, test_mode/2, make_move/2, stop_player/1, update_player/4]).
-record(player, {colour, board, game_pid, turn, moves_to_make}).

% Creates the process
start_link() -> gen_server:start_link(?MODULE, [], []).
% When does this get called??
init([]) ->
	io:format("player.erl:Player started. Pid:~p~n", [self()]),
	{ok, []}.
terminate(normal, State) -> {ok, State};
terminate(WTF, State) ->
	io:format("player.erl:Player terminated!!!!~p", [WTF]),
	{ok, State}.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

%% Synchronous call
%% Calls from server
start_player(Pid, Colour, Board) -> gen_server:call(Pid, {start, Colour, Board}).
% Tell player who moved and the position of the board
update_player(Pid, Move, Board, NextColour) ->  gen_server:cast(Pid, {update, Move, Board, NextColour}).
test_mode(Pid, ListOfMoves) -> gen_server:cast(Pid, {test, ListOfMoves}).


%% Calls from client (player)
make_move(Pid, Move) -> gen_server:call(Pid, {move, Move}).
stop_player(Pid) -> gen_server:call(Pid, terminate).
%% This call is asynchronous
% get_board(Pid) -> gen_server:cast(Pid, {board}).

handle_call({start, Colour, Board}, {From,_}, []) -> {reply, {ok, started}, make_player(Colour, Board, From)};
handle_call(terminate, _From, State) -> {stop, normal, ok, State};
handle_call({move, Move}, _From, State) ->
	{ok, NewBoard} = game:take_turn(State#player.game_pid, Move),
	{reply, valid_move, State#player{board=NewBoard,turn=State#player.turn+1}};
handle_call(WTF, From, State) ->
	io:format("player.erl:Received handle call ~p~nFrom:~p", [WTF,From]),
	{reply, ok, State}.

%% This is for test mode...
handle_cast({update, Move, Board, NextPlayer}, State) when State#player.moves_to_make /= [] ->
	%% OK. If I'm white. I will be updated of my own move, right?
	%board:pretty_print_board(Board),
	% Is it my turn?
	case NextPlayer =:= self() of
		true ->
			{ok,NewBoard} = game:take_turn(State#player.game_pid, lists:nth(State#player.turn+1, State#player.moves_to_make)),
			{noreply, State#player{board=NewBoard,turn=State#player.turn+1}};
		false ->
			% Wait for my opponent
			{noreply, State#player{board=Board,turn=State#player.turn+1}}
	end;
handle_cast({update, Move, Board, NextPlayer}, State) when NextPlayer =:= self() ->
	%board:pretty_print_board(Board),
	% Check previous move against movelist
	OpponentMove = lists:nth(State#player.turn-1, State#player.moves_to_make),
	io:format("player.erl:Moves:~nOne in list~p~nMade by player~p~nTurn:~p~n",[OpponentMove, Move, State#player.turn-1]),
	OpponentMove = Move,
	% Make move from list
	{ok, Board} = game:take_turn(State#player.game_pid, lists:nth(State#player.turn, State#player.moves_to_make)),
	{noreply, State#player{board=Board,turn=State#player.turn+1}};

handle_cast({test, ListOfMoves}, State) when State =/= [] ->
	case State#player.colour of
		white ->
			io:format("player.erl: White is taking first turn~n", []),
			{ok,Board} = game:take_turn(State#player.game_pid, hd(ListOfMoves)),
			{noreply, State#player{board=Board, moves_to_make=ListOfMoves}};
		black -> 
			{noreply, State#player{moves_to_make=ListOfMoves}};
		_ ->
			io:format("BAD COLOUR!!!!",[]),
			{noreply, State#player{moves_to_make=ListOfMoves}}
	end.

handle_info(Msg, State) ->
	io:format("player.erl:Unexpected message: ~p~n",[Msg]),
	{noreply, State}.

%%% Private functions
make_player(Colour, Board, GamePid) -> #player{colour=Colour, board=Board, game_pid=GamePid, turn=1, moves_to_make=[]}.
