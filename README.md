# Play Othello (aka Reversi)

Program sets up framework for a simple game of Othello. AI is programmed to
follow a fairly simple heuristic. Alpha-beta pruning is used for minimax
search of the two-player game.



## Heuristic
The heuristic takes into account both trying to obtain the corner positions
and limiting the mobility of the opponent. It also factors in trying to obtain
higher number of pieces on the board (this does not mean the highest possible).
Corner pieces are stable pieces and are dominant positions in this game.
Usually, having these positions will yield a higher advantage over the opposing
player. The heuristic factors obtaining corner pieces as high priority, but
not the highest priority. If the corner position can be obtained at another
turn, the heuristic may suggest to delay it.

Piece_diff is the difference between Plyr and AI, and the AI
favours a higher difference in favour of the AI. We naturally want more pieces
flip over than the opposing player.

CornerBonus is the difference between the number of corner pieces the
AI owns and the number of corner pieces the opposing player owns. The AI
favours a higher difference in favour of the AI, with a factor of 10 to consider higher priority.

XCornerBonus considers diagonal position next to the corner. It is a negative
value because we don't want the AI to go there as it may open up opportunities
for human to take the corner position. Also surrounding the corner position is
not a good idea if we want to take it.

Move_diff is the difference in the number of available moves both players have.
The heuristic tries to limit the moves the opposing player has. This way, it
forces the opposing player to play certain moves from a limited pool of moves.

The heuristic value is calculated by summing Piece_diff, Move_diff, and all the
bonus scores.
