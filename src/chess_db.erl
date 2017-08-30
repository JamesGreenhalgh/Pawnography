-module(chess_db).
-compile(export_all).

start() ->
	{ok, Started} = application:ensure_all_started(cqerl),
	io:format("Started:, ~p~n", [Started]).


foo() ->
	{ok, Client} = cqerl:get_client({}),
	io:format("Client:, ~p~n", [Client]),
	{ok, Result} = cqerl:run_query(Client, "USE tlp_lab;"),
	{ok, Result} = cqerl:run_query(Client, "USE tlp_lab;"),
	io:format("fooooo ~p", [Result]).




create_chess_db() ->
	{ok, Client} = cqerl:get_client({}),
	{ok, _Result} = cqerl:run_query(Client, 
		"CREATE KEYSPACE IF NOT EXISTS chess_db WITH REPLICATION = {'class' : 'NetworkTopologyStrategy', 'datacenter1' :3};"),
	{ok, _} = cqerl:run_query(Client, 
		"USE chess_db;"),
	{ok, _} = cqerl:run_query(Client, 
		"CREATE TABLE chess 
		(game_id uuid, 
		timestamp timestamp, 
		player ascii, 
		start  tuple<varchar, int>, 
		finish tuple<varchar, int>, 
		PRIMARY KEY(game_id, timestamp));").
	
insert_move(_Game, _Player, _From, _To) ->
	foo.
