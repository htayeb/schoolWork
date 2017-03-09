%
% move1( (X1,Y1), (X2,Y2) ) holds if a Knight can move in one step from
% (X1,Y1) to (X2,Y2) on a 8x8 chess board
% Solution by Humam Altayeb V00######
%
move1( (X1,Y1), (X2,Y2) ) :- up1( X1, X2 ), up2( Y1, Y2 ).
move1( (X1,Y1), (X2,Y2) ) :- up2( X1, X2 ), up1( Y1, Y2 ).
move1( (X1,Y1), (X2,Y2) ) :- up1( X1, X2 ), down2( Y1, Y2 ).
move1( (X1,Y1), (X2,Y2) ) :- up2( X1, X2 ), down1( Y1, Y2 ).
move1( (X1,Y1), (X2,Y2) ) :- down1( X1, X2 ), up2( Y1, Y2 ).
move1( (X1,Y1), (X2,Y2) ) :- down2( X1, X2 ), up1( Y1, Y2 ).
move1( (X1,Y1), (X2,Y2) ) :- down1( X1, X2 ), down2( Y1, Y2 ).
move1( (X1,Y1), (X2,Y2) ) :- down2( X1, X2 ), down1( Y1, Y2 ).

up1( U, V ) :- successor( U, V ).
up2( U, W ) :- successor( U, V ), successor( V, W ).
down1( U, V ) :- up1( V, U ).
down2( U, V ) :- up2( V, U ).

successor( 1, 2 ).
successor( 2, 3 ).
successor( 3, 4 ).
successor( 4, 5 ).
% successor( 5, 6 ).
% successor( 6, 7 ).
% successor( 7, 8 ).

%% PART I %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

path( A, B, N, P):-
	path(A, B, N, P, [A]),
	!.

path(A,A,_,[A],_):-!.
path(A,B,N,[A|P],Visited):-
	N > 0,
	move1(A,C),   % where can I move from A?
	not(member(C,Visited)),  % C has not been visited (to avoid cycles)
	N1 is N - 1,   % one step less
	path(C,B,N1,P,[C|Visited]).


%% PART 2 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:-dynamic shortest/2.  % to store the shortest path so far

shortest(A, B, P):-
	retractall(shortest(_,_)),   % remove previous solutions
	assert(shortest([],999999)), % best path initially is 999999
	not(allpaths(A, B)),	     % allpaths always fails, so we need the not
	shortest(P,_).		     % query the shortest path

allpaths(A, B):-
	path(A,B,100,P,[A]),            % get a path
	length(P,L),			% how long is it?
	shortest(BestPath, BestN),	% get the shortest path so far
	L < BestN,			% is it shortest than the previous one?
	retract(shortest(BestPath, BestN)), % store the new solution
	assert(shortest(P, L)),
	fail.                           % fail to keep working

%%	PART III %%%%%%%%%%%%%%%

visit(A, P, N):-
	visit(A, P, N, [A]).

visit(A, [A], 0, _):-!.         % length is 0, nothing to do here
visit(A, [A|P], N, Visited):-
	move1(A,C),                    % move one step
	not(member(C, Visited)),       % check that we have not visited C
	N1 is N - 1,		       % now we need paths of length N-1
	visit(C, P, N1, [C|Visited]).  % find those paths...




