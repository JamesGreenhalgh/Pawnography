# Pawnography
Erlang chess tinkering

game 
- gen server
- updates players and viewers
- receives turns from players
- gen_server exists per game
- keeps track of turn

board
- responsible for rules and validity
- not a server

player
- interacts with client program?
- is updated by game
- updates game (takes turn)

db
- stores games


== TODO

- Finish rules in board
- Get a whole game in test



== Synchronous and Async calls
---------------------------
When should I use one or the other?
