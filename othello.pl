:- ensure_loaded('abplay.pl').

% State Representation: 
% [Row0, Row1 ... Rown].
% each Rowi is a LIST of 6 elements '.' or '1' or '2' as follows: 
%    . means the position is  empty
%    1 means player one has a piece in this position
%    2 means player two has a piece in this position. 

% Inital state of the board 
initBoard([ [.,.,.,.,.,.], 
            [.,.,.,.,.,.],
            [.,.,1,2,.,.], 
            [.,.,2,1,.,.], 
            [.,.,.,.,.,.], 
            [.,.,.,.,.,.] ]).

% Initialize predefined state from above, initial player is human
% initialize/2 : initialize(InitialState, InitialPlayer)
% holds iff InitialState is the initial state and
% InitialPlyr is the player who moves first.
initialize(InitState, 1):- initBoard(InitState).

% Count the number of times an element appears in a list
% countBoard/3
% countBoard(Element, State, NumberOfElements)
% TODO: generalize for any size board (right now it's 5x5)
count(_, [], 0):- !. % empty list, base case
count(X, [X|T], N) :- count(X, T, N2), % if X is in the head of the list
                      N is N2 + 1.     % count on the tail (let this N2) 
count(X, [Y|T], N):- X \= Y,          % if X is not in the head
                     count(X, T, N).  % count the rest
countBoard(X,[A, B, C, D, E, F], N):- count(X, A, N1), % count the othello board
                                      count(X, B, N2),
                                      count(X, C, N3),
                                      count(X, D, N4),
                                      count(X, E, N5),
                                      count(X, F, N6),
                                      N is N1 + N2 + N3 + N4 + N5 + N6.

% Winner if the number of pieces of that player is greater than opponent
% winner/2
% winner(State, Plyr)
% returns winning player if State is a terminal position and
%     Plyr has a higher score than the other player
winner(State, 1):- terminal(State), % happens in terminal state only
                  countBoard(1, State, N1),
                  countBoard(2, State, N2),
                  N1 > N2.
winner(State, 2):- terminal(State), % Does the computer win instead?
                  countBoard(1, State, N1),
                  countBoard(2, State, N2),
                  N2 > N1.

% tie/1
% tie(State) 
%    - true if terminal State is a "tie" (no winner) 
tie(State):- terminal(State),
             \+winner(State,_). % no winner 

% terminal/1
% terminal(State)
% terminal state happens when MvList is 'n' for both players
%   - true if State is a terminal 
terminal(State):- moves(1,State,MvList1), member('n',MvList1),
                  moves(2,State,MvList2), member('n',MvList2).

% Helper functions: For debugging purposes
showState( G ):-
	printRows( G ).
 
printRows( [] ).
printRows( [H|L] ):-
	printList(H),
	nl,
	printRows(L).

printList([]).
printList([H | L]):-
	write(H),
	write(' '),
	printList(L).

% moves/3
% moves(Plyr, State, MvList)
%   - returns list MvList of all legal moves Plyr can make in State
% cycle through all positions on board to check if they are valid for given
% player and state
% sorts as it checks position from top row to bottom row and within a row from
% left column to right column by cycling through all columns within each row
% first
moves(Plyr, State, MvList):-
  testmoves(Plyr, [0,0], State, [], MvList). % start at [0,0] Pos

% testmoves/5
% testmoves(Plyr, Position, State, MvListSoFar, MvList)
testmoves(_,[6,_],_,SoFar,MvList):-
  \+member(_,SoFar), MvList = ['n'],!. % empty SoFar, so only valid move is 'n' 
testmoves(_,[6,_],_,MvList,MvList):-!. % The ending case, copy

 % Go to next row when column number is exceeded
testmoves(Plyr, Pos, State, SoFar, MvList):-
  [R,C] = Pos,
  C == 6, % if this happens, go to next row
  NextR is R + 1,
  testmoves(Plyr, [NextR, 0], State, SoFar, MvList),!.

% If this position is valid, store it
testmoves(Plyr, Pos, State, SoFar, MvList):-
  [R,C] = Pos,
  isvalid_index(Pos),
  checkvalid(Plyr, State, Pos,_),
  append(SoFar,[Pos],NSoFar), % if valid, add to list
  NextC is C + 1, % next column
  testmoves(Plyr, [R, NextC], State, NSoFar, MvList),!.

% If position is not valid, move on...  
testmoves(Plyr, Pos, State, SoFar, MvList):-
  [R,C] = Pos,
  isvalid_index(Pos),
  NextC is C + 1,
  testmoves(Plyr, [R, NextC], State, SoFar, MvList),!.


% checkvalid/4
% checkvalid(Plyr, State, Position, ValidDirectionOffsets)
% Checks if Pos is a valid position to play
% ValidDirectionOffsets are all the directions that the Plyr piece can change
checkvalid(Plyr, St, Pos, ValidDirectionOffsets):- 
  isempty(St, Pos), % check for empty spot
  direction_offsets(DirectionOffsets), % my starting list to check
  checkvalidList(Plyr, St, Pos, DirectionOffsets, [], ValidDirectionOffsets).

% checkvalidList/6
% checkvalidList(Plyr, State, Position, DirectionOffsets, SoFarOffsets, ValidDirectionOffsets)
% Record all valid directions offset into ValidDirectionOffsets
% The base case is when DirectionOffsets is 0, copy to ValidDirectionOffsets
checkvalidList(_,_,_,[],SoFarOffsets, ValidDirectionOffsets):- 
  SoFarOffsets \= [],
  SoFarOffsets = ValidDirectionOffsets. % SoFarOffsets list is the same

% Record all valid directions offset in SoFarOffsets
checkvalidList(Plyr, St, Pos, DirectionOffsets, SoFarOffsets,
               ValidDirectionOffsets):-
  DirectionOffsets = [DirectionOffset|DirectionOffsetsRest], % get first element
  checkvalid_offset(Plyr, St, Pos, DirectionOffset), % check first element
  % if valid append to SoFarOffsets
  append(SoFarOffsets, [DirectionOffset], NSoFar), % if valid, store it
  checkvalidList(Plyr, St, Pos, DirectionOffsetsRest, % check the rest
                NSoFar, ValidDirectionOffsets).

% Not a good direction, move to the next
checkvalidList(Plyr, St, Pos, DirectionOffsets, SoFarOffsets,
               ValidDirectionOffsets):-
  DirectionOffsets = [_|DirectionOffsetsRest],
  checkvalidList(Plyr, St, Pos, DirectionOffsetsRest, SoFarOffsets,
                ValidDirectionOffsets).
  
% checkvalid_offset/4
% checkvalid_offset(Plyr, State, Position, DirectionOffset)
% checks valid move by looking at offsets (surroundings)   
checkvalid_offset(Plyr, St, Pos, DirectionOffset):-
  isempty(St, Pos),
  [R,C] = Pos,
  nth0(0, DirectionOffset, RowOffset),
  nth0(1, DirectionOffset, ColOffset),
  NeighbourRow is R + RowOffset,
  NeighbourCol is C + ColOffset,
  NeighbourPos = [NeighbourRow, NeighbourCol], % this is the offset position
  isvalid_index(NeighbourPos), % is this within boundaries?
  getOppPlyr(Plyr, Opponent),
  get(St, NeighbourPos, Value),
  Value = Opponent,
  % bingo, found opponent's piece, now try to find your piece by walking
  find_colour(Plyr, St, NeighbourPos, RowOffset, ColOffset).

% find_colour/5
% find_colour(Plyr, State, Position, RowOffset, ColOffset)
% RowOffset, ColOffset define the offset's direction
% walks through opponent pieces and finds player's colour (1 or 2)
find_colour(Plyr, St, Pos, RowOffset, ColOffset):-
  [R,C] = Pos,
  NRowOffset is R + RowOffset,
  NColOffset is C + ColOffset,
  NPos = [NRowOffset, NColOffset],
  isvalid_index(NPos),
  get(St, NPos, Value),
  Value = Plyr.  % found it!

% did not find, continue on...
find_colour(Plyr, St, Pos, RowOffset, ColOffset):-
  [R,C] = Pos,
  NRowOffset is R + RowOffset,
  NColOffset is C + ColOffset,
  NPos = [NRowOffset, NColOffset],
  isvalid_index(NPos),
  getOppPlyr(Plyr, Opponent),
  get(St, NPos, Value),
  Value = Opponent, % keep walking
  find_colour(Plyr, St, NPos, RowOffset, ColOffset).
  
  
% isvalid_index/1
% isvalid_index(Position)
% checks if position is within boundaries
isvalid_index(Pos):-
  [R, C] = Pos,
  R >= 0, R =< 5,
  C >= 0, C =< 5.

% isempty/2
% isempty(State, Position)
% checks if position is empty
isempty(St, Pos):-
  get(St, Pos, Value),
  Value == '.'.

% direction_offsets/1
% direction_offsets(OffsetsList)  
% off-set list, the relative coordinate surrounding a piece to check for
direction_offsets(OffsetsList) :-
	OffsetsList = [[-1, 0], % left
			           [-1, 1], % left-down
			           [0, 1],  % down
                 [1, 1],  % down-right
                 [1, 0],  % right
                 [1, -1], % right-up
                 [0, -1], % up
                 [-1,-1]].% left-up

% getOppPlyr/2
% getOppPlyr(Plyr, Opponent)
% Gets opponent player piece
getOppPlyr(1, 2):- !.
getOppPlyr(2, 1).

% nextState/5
%  - given that Plyr makes Move in State, it determines NewState (i.e. the next 
%     state) and NextPlayer (i.e. the next player who will move).
% nextState(Plyr, Move, State, NewState, NextPlyr)
% check if skip turn
nextState(Plyr, Move, State, NewState, NextPlyr):-
  validmove(Plyr, State, Move),
  Move == 'n', % validmove will check for this
  NewState = State, % returns state left unchanged
  getOppPlyr(Plyr, NextPlyr).

% else, no turn skipped
nextState(Plyr, Move, State, NewState, NextPlyr):-
  validmove(Plyr, State, Move),
  checkvalid(Plyr, State, Move, ValidDirectionOffsets), % returns the valid offsets
  set(State, NS, Move, Plyr), % set the first piece, NS is intermediate new state
  setOffsets(NS, NewState, Move, Plyr, ValidDirectionOffsets), % set the rest
  getOppPlyr(Plyr, NextPlyr).

% setOffsets/5
% setOffsets(State, NewState, Move, Plyr, ValidDirectionOffsets)
% Only finished setting when ValidDirectionOffsets is empty and NewState is
% finalize
setOffsets(NewState, NewState,_,_,[]):-!. % Base case

% setting the pieces in all valid direction offsets
setOffsets(State, NewState, Move, Plyr, ValidDirectionOffsets):-
  ValidDirectionOffsets = [ValidDirectionOffset|ValidDirectionOffsetsRest],
  setOffset(State, NS, Move, Plyr, ValidDirectionOffset),
  % change pieces, NS is intermediate new state
  setOffsets(NS, NewState, Move, Plyr, ValidDirectionOffsetsRest). % next dir

% setOffset/5
% setOffset(State, NewState, Move, Plyr, DirectionOffset)
% Cycle through one direction
% Change pieces in a given direction
% Base case, found my own piece:
setOffset(State, NewState, Move, Plyr, ValidDirectionOffset):-
  [R,C] = Move,
  nth0(0, ValidDirectionOffset, RowOffset),
  nth0(1, ValidDirectionOffset, ColOffset),
  NRowOffset is R + RowOffset,
  NColOffset is C + ColOffset,
  NPos = [NRowOffset, NColOffset],
  isvalid_index(NPos),
  get(State, NPos, Value),
  Value = Plyr, % found the ending piece
  State = NewState, !. % end

% Change piece if it's the opponent
setOffset(State, NewState, Move, Plyr, ValidDirectionOffset):-
  [R,C] = Move,
  nth0(0, ValidDirectionOffset, RowOffset),
  nth0(1, ValidDirectionOffset, ColOffset),
  NRowOffset is R + RowOffset,
  NColOffset is C + ColOffset,
  NPos = [NRowOffset, NColOffset],
  isvalid_index(NPos),
  get(State, NPos, Value),
  getOppPlyr(Plyr, Opponent),
  Value = Opponent, % found an opponent piece, change
  set(State, NS, NPos, Plyr), % NS is intermediate new state
  setOffset(NS, NewState, NPos, Plyr, ValidDirectionOffset).

% validmove/3
%   - true if Proposed move by Plyr is valid at State.
% valid if moves are within legal moves
validmove(Plyr, State, Proposed):- moves(Plyr, State, MvList),
                                   member(Proposed, MvList).

%%%%%%%%%%%%%%%%%%%%%% AI Heuristic %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% h(State,Val)
%   - given State, returns heuristic Val of that state
%   - larger values are good for Max, smaller values are good for Min
%   NOTE1. If State is terminal h should return its true value.
%   NOTE2. If State is not terminal h should be an estimate of
%          the value of state (see handout on ideas about
%          good heuristics.
%h(S,V) returns V = 0 for any non-terminal state S, 
%h returns + for terminal state win, - for terminal state lose, 0 for tie state

% Simple Heuristic, uncomment to test
/*
h(State,100):- winner(State, 1), !.
h(State,-100):- winner(State, 2), !.
h(State,0):- tie(State), !.
h(_,0).                                  
*/

% tries to get both stable pieces and limits mobility of opponent
h(State,Val):- countBoard(1, State, N1),
               countBoard(2, State, N2),
               TotalN is N1 + N2, % Total number of pieces on board
               Piece_diff is N2 - N1, % aim to get a higher Piece_diff (high N2)
               getCorners(Corners), % corner coordinates
               getXCorner(XCorners), % X corner coordinates
               countPos(State, Corners, OpCorner, PlyrCorner),
               countPos(State, XCorners, OpXCorner, PlyrXCorner),
               CornerBonus is 10 * (PlyrCorner - OpCorner), % go corner
               XCornerBonus is -10 * (PlyrXCorner - OpXCorner), % don't go X corner
               moves(1, State, MvList1),
               length(MvList1, M1),
               moves(2, State, MvList2),
               length(MvList2, M2),
               Move_diff is M1 - M2, % limit the mobility of Player 1
               (
                 (TotalN < 36)-> Piece_diffScore is Piece_diff,
                                Move_diffScore is Move_diff
                                ;
                                Piece_diffScore is Piece_diff,
                                Move_diffScore is 0
               ),
               Val is Piece_diffScore + Move_diffScore + 
                 CornerBonus + XCornerBonus.
          
% countPos/4
% countPos(State, PositionList, PlyrCount, OpponentCount)
% counts the number of pieces both players have in PosList
countPos(State, PosList, Count, OpCount):-
  countPosList(State,PosList, 0, 0, Count, OpCount). % start count at 0

countPosList(State, PosList, SoFarCount, SoFarOpCount, Count, OpCount):-
  (
  PosList = [] -> Count = SoFarCount,
                  OpCount = SoFarOpCount % Base case, exit when list is empty
   ;
   PosList = [Pos|PosRest],
   Pos = [Rowi,Coli| CheckList], % Checklist is for XCorner cases
   (
     CheckList\=[]-> CheckList=[CheckRow,CheckCol],
                     get(State, [CheckRow, CheckCol], CheckPiece)
     ;
     CheckPiece = null
   ),
   get(State, [Rowi, Coli], Piece),
   (
     (Piece = 2, CheckPiece \= 2)-> NSoFarCount is SoFarCount + 1,
                                    NSoFarOpCount is SoFarOpCount
     ;
     (Piece = 1, CheckPiece \= 1)-> NSoFarCount is SoFarCount,
                                    NSoFarOpCount is SoFarOpCount + 1
     ;
                                    NSoFarCount is SoFarCount,
                                    NSoFarOpCount is SoFarOpCount
   ),
   countPosList(State,PosRest, NSoFarCount, NSoFarOpCount, Count, OpCount)
   ).

% getCorners/1
% getCorners(Corners)
% returns Corners as corners of board
getCorners(Corners):- Corners = [[0,0],
                                 [0,5],
                                 [5,0],
                                 [5,5]].
% getXCorners/2
% getXCorners(XCorners)
% returns XCorners as 2 positions in each corner
getXCorner(XCorners):- XCorners = [[1,1, 0,0],
                                   [1,4, 0,5],
                                   [4,1, 5,0],
                                   [4,4, 5,5]].

% lowerBound(B).  
%   - returns a value B that is less than the actual or heuristic value
%     of all states.
lowerBound(-10).


% upperBound(B). 
%   - returns a value B that is greater than the actual or heuristic value
%     of all states.
upperBound(20).


% Utility Functions
% get(Board, Point, Element)
%    : get the contents of the board at position row R column C
% set(Board, NewBoard, [R, C], Value):
%    : set Value at row R column C in Board and bind resulting grid to NewBoard
%
% The origin of the board is in the upper left corner with an index of
% [0,0], the upper right hand corner has index [0,5], the lower left
% hand corner has index [5,0], the lower right hand corner has index
% [5,5] (on a 6x6 board).
%
% Example
% ?- initBoard(B), showState(B), get(B, [3,2], Value). 
%. . . . . . 
%. . . . . . 
%. . 1 2 . . 
%. . 2 1 . . 
%. . . . . . 
%. . . . . . 
%
%B = [['.', '.', '.', '.', '.', '.'], ['.', '.', '.', '.', '.', '.'], 
%     ['.', '.', 1, 2, '.', '.'], ['.', '.', 2, 1, '.'|...], 
%     ['.', '.', '.', '.'|...], ['.', '.', '.'|...]]
%Value = 2 
%Yes
%?- 
%
% Setting values on the board
% ?- initBoard(B),  showState(B),set(B, NB1, [4,2], 1), set(NB1, NB2, [3,2], 1),  showState(NB2). 
%
% . . . . . . 
% . . . . . . 
% . . 1 2 . . 
% . . 2 1 . . 
% . . . . . . 
% . . . . . .
% 
% . . . . . . 
% . . . . . . 
% . . 1 2 . . 
% . . 1 1 . . 
% . . 1 . . . 
% . . . . . .
%
%B = [['.', '.', '.', '.', '.', '.'], ['.', '.', '.', '.', '.', '.'], ['.', '.', 
%1, 2, '.', '.'], ['.', '.', 2, 1, '.'|...], ['.', '.', '.', '.'|...], ['.', '.',
% '.'|...]]
%NB1 = [['.', '.', '.', '.', '.', '.'], ['.', '.', '.', '.', '.', '.'], ['.', '.'
%, 1, 2, '.', '.'], ['.', '.', 2, 1, '.'|...], ['.', '.', 1, '.'|...], ['.', '.
%', '.'|...]]
%NB2 = [['.', '.', '.', '.', '.', '.'], ['.', '.', '.', '.', '.', '.'], ['.', '.'
%, 1, 2, '.', '.'], ['.', '.', 1, 1, '.'|...], ['.', '.', 1, '.'|...], ['.', 
%'.', '.'|...]]

% get(Board, Point, Element): get the value of the board at position
% row R column C (indexing starts at 0).
get( Board, [R, C], Value) :- 
	nth0( R, Board, Row), 
	nth0( C, Row, Value).
 
% set( Board, NewBoard, [X, Y], Value) 
set( [Row|RestRows], [NewRow|RestRows], [0, C], Value)
    :- setInList(Row, NewRow, C, Value). 

set( [Row|RestRows], [Row|NewRestRows], [R, C], Value) :- 
	R > 0, 
	R1 is R-1, 
	set( RestRows, NewRestRows, [R1, C], Value). 

% setInList( List, NewList, Index, Value) 
setInList( [_|RestList], [Value|RestList], 0, Value). 

setInList( [Element|RestList], [Element|NewRestList], Index, Value) :- 
	Index > 0, 
	Index1 is Index-1, 
	setInList( RestList, NewRestList, Index1, Value). 
