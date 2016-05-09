-module(board).
-export([make_move/4, get_board/0, test/0, pretty_print_board/1]).
%-compile(export_all).
-define(NEW_BOARD, [
	{rook, white}, {knight, white}, {bishop, white}, {queen, white}, {king, white}, {bishop, white}, {knight, white}, {rook,white},
	{pawn, white}, {pawn, white}, {pawn, white}, {pawn, white}, {pawn, white}, {pawn, white}, {pawn, white}, {pawn, white}, 
	{}, {}, {}, {}, {}, {}, {}, {}, 
	{}, {}, {}, {}, {}, {}, {}, {}, 
	{}, {}, {}, {}, {}, {}, {}, {}, 
	{}, {}, {}, {}, {}, {}, {}, {}, 
	{pawn, black}, {pawn, black}, {pawn, black}, {pawn, black}, {pawn, black}, {pawn, black}, {pawn, black}, {pawn, black}, 
	{rook, black}, {knight, black}, {bishop, black}, {queen, black}, {king, black}, {bishop, black}, {knight, black}, {rook,black}]).

-define(TEST_MOVES, [
	{white, {c,2}, {c,4}},
	{black, {c,7}, {c,5}},
	{white, {b,1}, {c,3}},
	{black, {b,8}, {c,6}}
	]).

get_board() -> ?NEW_BOARD.

test() ->
	lists:foldl(fun({Colour, From, To}, Board) -> make_move(Board, Colour, From, To) end, ?NEW_BOARD, ?TEST_MOVES).
	%lists:foldl(fun(X, Sum) -> X + Sum end, 0, [1,2,3,4,5]).

make_move([], white, From, To) ->
	make_move(?NEW_BOARD, white, From, To);
make_move(Board, Colour, {A,B}, {X,Y}) when is_atom(A), is_atom(X) ->
	make_move(Board, Colour, {file_to_integer(A),B}, {file_to_integer(X),Y});
make_move(Board, Colour, From, To) ->
	case is_valid_move(Board,Colour,From,To) of 
		{false, Description} ->
			{badmove, Description};
		true ->
			{ok, move_piece(Board,From,To)}
	end.

is_valid_move(Board, Colour, From, To) ->
	% Get type and make sure colour matches
	{Type, FromColour} = get_square(From, Board),
	is_my_piece(FromColour, Colour) andalso
	% is destination empty, or contain enemy piece?
	is_empty_or_enemy(Board, Colour, To) andalso
	% is the to location potentially legal??
	legal_move(Type, From, To) andalso
	is_not_blocked(Board, From, To).

is_my_piece(FromColour, Colour) when FromColour =:= Colour -> true;
is_my_piece(_,_) -> {false, not_your_piece}.

is_not_blocked(Board, From, To) ->
	case get_square(From, Board) of
		{knight,_} -> true;
		{king,_} -> true;
		%% What about first move??
		{pawn,_} -> true;
		{rook,_} -> 
			squares_are_empty(get_intermediate_squares(From, To));
		{bishop,_} -> 
			squares_are_empty(get_intermediate_squares(From, To));
		{queen,_} -> 
			squares_are_empty(get_intermediate_squares(From, To))
	end.
	% knight doesn't need to check
	% king and pawn already checked
	
	% need to get all squares between diagonals
	% need to get all squares between straights

squares_are_empty([]) -> true;
squares_are_empty([X|RestOfSquares]) ->
	case X of
		{} -> squares_are_empty(RestOfSquares);
		_ -> {false, path_blocked}
	end.

% Horizontal/vertical
get_intermediate_squares({A,B}, {A,Y}) when B<Y ->
	[{A,Z} || Z <- lists:seq(B+1,Y-1)];
get_intermediate_squares({A,B}, {A,Y}) ->
	[{A,Z} || Z <- lists:reverse(lists:seq(B+1,Y-1))];
get_intermediate_squares({A,B}, {X,B}) when A<X ->
	[{Z,B} || Z <- lists:seq(A+1,X-1)];
get_intermediate_squares({A,B}, {X,B}) ->
	[{Z,B} || Z <- lists:reverse(lists:seq(A+1,X-1))];
% Diagonal
get_intermediate_squares({A,B}, {X,Y}) when abs(X-A) =:= abs(B-Y) ->
	FileList = case A<X of
		true ->
			lists:seq(A+1,X-1);
		false ->
			lists:reverse(lists:seq(X+1,A-1))
	end,
	RankList = case B<Y of
		true ->
			lists:seq(B+1,Y-1);
		false ->
			lists:reverse(lists:seq(Y+1,B-1))
	end,
	lists:zip(FileList, RankList).
	
legal_move(Type, From, To) ->
	case Type of
		knight -> is_horsie(From, To);
		queen -> is_straight(From, To) orelse
				is_diagonal(From, To);
		bishop -> is_diagonal(From, To);
		rook -> is_straight(From, To);
		pawn -> is_straight(From, To) orelse 
			is_diagonal(From, To);
		king -> is_straight(From,To) orelse
			is_diagonal(From, To)
	end.

is_empty_or_enemy(Board, Colour, To) ->
	%Also checks if From=To
	DestPiece = get_square(To, Board),
	case DestPiece of
		{} -> true;
		{_, Colour} -> {false, own_piece_at_destination};
		{_, _} -> true
	end.

is_diagonal({A,B},{X,Y}) ->
	case {abs(X-A), abs(Y-B)} of
		{C,C} -> true;
		_ -> {false, not_diagonal}
	end.

is_straight(From, To) ->
	case {From, To} of
		{{X,_},{X,_}} -> true;
		{{_,Y},{_,Y}} -> true;
		_ -> {false, not_straight}
	end.

is_horsie({A,B}, {X,Y}) ->
	case {abs(X - A), abs(Y-B)} of
		{2,1} -> true;
		{1,2} -> true;
		_ -> {false, not_horsie}
	end.

move_piece(Board, From, To) ->
	FromPiece = get_square(From, Board),
	NewBoard = replace_nth(Board, co_ordinate_to_element(To), FromPiece),
	replace_nth(NewBoard, co_ordinate_to_element(From), {}).

replace_nth(List, Index, NewValue) ->
	{Front, [_|Back]} = lists:split(Index-1, List),
	Front ++ [NewValue] ++ Back.

%print_board(Board) ->
%	Sublists = [lists:sublist(Board, Start, 8) || Start <- lists:seq(1,64,8)],
%	io:format("NOW IS BOARD:~n", []),
%	[io:format("~n~p~n~n", [Row]) || Row <- Sublists].

pretty_print_piece({rook,   Colour}) -> io:format("R~s ", [colour_atom_to_string(Colour)]);
pretty_print_piece({knight, Colour}) -> io:format("N~s ", [colour_atom_to_string(Colour)]);
pretty_print_piece({bishop, Colour}) -> io:format("B~s ", [colour_atom_to_string(Colour)]);
pretty_print_piece({queen,  Colour}) -> io:format("Q~s ", [colour_atom_to_string(Colour)]);
pretty_print_piece({king,   Colour}) -> io:format("K~s ", [colour_atom_to_string(Colour)]);
pretty_print_piece({pawn,   Colour}) -> io:format("P~s ", [colour_atom_to_string(Colour)]);
pretty_print_piece({}) -> io:format("   ", []).

pretty_print_rank(Rank) ->
	[pretty_print_piece(Piece)  || Piece <- Rank],
	io:format("~n", []).

pretty_print_board(Board) ->
	io:format("-------------------------~n", []),
	Sublists = [lists:sublist(Board, Start, 8) || Start <- lists:reverse(lists:seq(1,64,8))],
	[pretty_print_rank(Rank) || Rank <- Sublists],
	io:format("a--b--c--d--e--f--g--h--~n", []).

get_square(Coord,Board) -> lists:nth(co_ordinate_to_element(Coord), Board).
co_ordinate_to_element({File,Rank}) -> (Rank-1) * 8 + File.
colour_atom_to_string(white) -> "W";
colour_atom_to_string(black) -> "B".
%int_to_file(1) -> a;
%int_to_file(2) -> b;
%int_to_file(3) -> c;
%int_to_file(4) -> d;
%int_to_file(5) -> e;
%int_to_file(6) -> f;
%int_to_file(7) -> g;
%int_to_file(8) -> h.
file_to_integer(a) -> 1;
file_to_integer(b) -> 2;
file_to_integer(c) -> 3;
file_to_integer(d) -> 4;
file_to_integer(e) -> 5;
file_to_integer(f) -> 6;
file_to_integer(g) -> 7;
file_to_integer(h) -> 8.
