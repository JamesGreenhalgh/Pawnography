-module(chess_test).
-export([run_test/0]).

-define(TEST_MOVES, [ {{a,2},{a,4}},{{a,7},{a,6}}, {{b,2},{b,4}},{{b,7},{b,6}} ]).

run_test() ->
	{ok, GamePid} = game:start_link(),
	{ok, Player1} = player:start_link(),
	{ok, Player2} = player:start_link(),
	io:format("GAMEPID IS~p~n", [GamePid]),
	% Players would enter a 'room' beforehand, and then the room would call 
	% new_game and their procs would start receiving messages from the chess 
	% gen_server.
	{ok, started} = game:start_game(GamePid, Player1, Player2),
	% In test mode, players get a list 
	% of moves and start making them
	io:format("GAMEPID IS~p~n", [GamePid]),
	player:test_mode(Player1, ?TEST_MOVES),
	player:test_mode(Player2, ?TEST_MOVES).
