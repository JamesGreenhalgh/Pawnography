-module(board).
-compile(export_all).
-define(NEW_BOARD, [
	{rook, white}, {knight, white}, {bishop, white}, {queen, white}, {king, white}, {bishop, white}, {knight, white}, {rook,white},
	{pawn, white}, {pawn, white}, {pawn, white}, {pawn, white}, {pawn, white}, {pawn, white}, {pawn, white}, {pawn, white}, 
	{}, {}, {}, {}, {}, {}, {}, {}, 
	{}, {}, {}, {}, {}, {}, {}, {}, 
	{}, {}, {}, {}, {}, {}, {}, {}, 
	{}, {}, {}, {}, {}, {}, {}, {}, 
	{pawn, black}, {pawn, black}, {pawn, black}, {pawn, black}, {pawn, black}, {pawn, black}, {pawn, black}, {pawn, black}, 
	{rook, black}, {knight, black}, {bishop, black}, {queen, black}, {king, black}, {bishop, black}, {knight, black}, {rook,black}]).

test() ->
	Board = ?NEW_BOARD,
	pretty_print_board(Board),
	get_square({d,1}, Board),
	From = {a,2},
	To = {a,4},
	NewBoard = move_piece(Board,From,To), 
	is_straight(From, To),
	pretty_print_board(NewBoard).

is_valid_move(Board, From, To) ->
	% is from square empty?
	{_Type, _Colour} = get_square(From, Board),
	% is destination empty, or contain enemy piece?
	destination_possible(Colour, To),
	% is the to location potentially legal??
	legal_move(Type, From, To),
	is_blocked(From, To),
	no_check,
	
	true.

destination_possible(Colour, To) ->
	get_square(To).

is_diagonal({A,B},{X,Y}) ->
	case {abs(file_to_integer(X) - file_to_integer(A)), abs(Y-B)} of
		{C,C} -> true;
		_ -> false
	end.

is_straight(From, To) ->
	case {From, To} of
		{{X,_},{X,_}} -> true;
		{{_,Y},{_,Y}} -> true;
		_ -> false
	end.

is_horsie({A,B}, {X,Y}) ->
	case {abs(file_to_integer(X) - file_to_integer(A)), abs(Y-B)} of
		{2,1} -> true;
		{1,2} -> true;
		_ -> false
	end.

move_piece(Board, From, To) ->
	FromPiece = get_square(From, Board),
	NewBoard = replace_nth(Board, co_ordinate_to_element(To), FromPiece),
	replace_nth(NewBoard, co_ordinate_to_element(From), {}).

replace_nth(List, Index, NewValue) ->
	{Front, [_|Back]} = lists:split(Index-1, List),
	Front ++ [NewValue] ++ Back.

print_board(Board) ->
	Sublists = [lists:sublist(Board, Start, 8) || Start <- lists:seq(1,64,8)],
	io:format("NOW IS BOARD:~n", []),
	[io:format("~n~p~n~n", [Row]) || Row <- Sublists].

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
	Sublists = [lists:sublist(Board, Start, 8) || Start <- lists:reverse(lists:seq(1,64,8))],
	[pretty_print_rank(Rank) || Rank <- Sublists].

get_square(Coord,Board) -> lists:nth(co_ordinate_to_element(Coord), Board).

co_ordinate_to_element({File,Rank}) -> (Rank-1) * 8 + file_to_integer(File).

colour_atom_to_string(white) -> "W";
colour_atom_to_string(black) -> "B".

file_to_integer(a) -> 1;
file_to_integer(b) -> 2;
file_to_integer(c) -> 3;
file_to_integer(d) -> 4;
file_to_integer(e) -> 5;
file_to_integer(f) -> 6;
file_to_integer(g) -> 7;
file_to_integer(h) -> 8.
