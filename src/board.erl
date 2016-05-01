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

get_new_board() ->
	?NEW_BOARD.

print_new_board() ->
	Board = ?NEW_BOARD,
	print_board(Board).

test() ->
	Board = ?NEW_BOARD,
	pretty_print_board(Board),
	get_square({d,1}, Board).


print_board(Board) ->
	Sublists = [lists:sublist(Board, Start, 8) || Start <- lists:seq(1,64,8)],
	[io:format("NOW IS BOARD:~n~p~n~n", [Row]) || Row <- Sublists].


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

colour_atom_to_string(Colour) ->
	case Colour of
		white -> "W";
		black -> "B"
	end.
pretty_print_board(Board) ->
	Sublists = [lists:sublist(lists:reverse(Board), Start, 8) || Start <- lists:seq(1,64,8)],
	[pretty_print_rank(Rank) || Rank <- Sublists].


get_square(Coord,Board) -> lists:nth(co_ordinate_to_element(Coord), Board).

co_ordinate_to_element({File,Rank}) -> (Rank-1) * 8 + file_to_integer(File).

file_to_integer(a) -> 1;
file_to_integer(b) -> 2;
file_to_integer(c) -> 3;
file_to_integer(d) -> 4;
file_to_integer(e) -> 5;
file_to_integer(f) -> 6;
file_to_integer(g) -> 7;
file_to_integer(h) -> 8.
