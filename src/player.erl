-module(player).
-behaviour(gen_server).
-export([init/1, start_link/0, handle_call/3, code_change/3, handle_cast/2, handle_info/2, terminate/2]).
-export([start_player/3, test_mode/2, make_move/2, stop_player/1, update_player/4]).
-record(player, {colour, board, game_pid, turn, moves_to_make}).

% Creates the process
start_link() -> gen_server:start_link(?MODULE, [], []).
% When does this get called??
init([]) ->
	io:format("Player started. Pid:~p~n", [self()]),
	{ok, []}.
terminate(normal, State) -> {ok, State};
terminate(WTF, State) ->
	io:format("Player terminated!!!!~p", [WTF]),
	{ok, State}.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

%% Synchronous call
%% Calls from server
start_player(Pid, Colour, Board) -> gen_server:call(Pid, {start, Colour, Board}).
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
	io:format("Received handle call ~p~nFrom:~p", [WTF,From]),
	{reply, ok, State}.

handle_cast({update, Move, Board, NextPlayer}, State) when NextPlayer =:= self(); length(State#player.moves_to_make) > State#player.turn ->
	board:pretty_print_board(Board),
	% Check previous move against movelist
	OpponentMove = lists:nth(State#player.turn-1, State#player.moves_to_make),
	%io:format("Moves:~nOne in list~pMade by player~p~n",[OpponentMove, Move]),
	OpponentMove = Move,
	% Make move from list
	{ok, NewBoard} = game:take_turn(State#player.game_pid, lists:nth(State#player.turn, State#player.moves_to_make)),
	{noreply, State#player{board=NewBoard,turn=State#player.turn+2}};

handle_cast({test, ListOfMoves}, State) when State =/= [] ->
	case State#player.colour of
		white -> 
			{ok,Board} = game:take_turn(State#player.game_pid, hd(ListOfMoves)),
			{noreply, State#player{board=Board, moves_to_make=ListOfMoves, turn=3}};
		black -> 
			{noreply, State#player{moves_to_make=ListOfMoves, turn=2}}
	end.	

handle_info(Msg, State) ->
	io:format("Unexpected message: ~p~n",[Msg]),
	{noreply, State}.

%%% Private functions
make_player(Colour, Board, GamePid) -> #player{colour=Colour, board=Board, game_pid=GamePid, turn=1, moves_to_make=[]}.
