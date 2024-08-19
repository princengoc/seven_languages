% Zebra Puzzle solution in SWI-Prolog

solve(ZebraOwner, WaterDrinker) :-
    Houses = [house(_,_,_,_,_), house(_,_,_,_,_), house(_,_,_,_,_), house(_,_,_,_,_), house(_,_,_,_,_)],
    member(house(red, englishman, _, _, _), Houses),
    member(house(_, spaniard, _, _, dog), Houses),
    member(house(green, _, coffee, _, _), Houses),
    member(house(_, ukrainian, tea, _, _), Houses),
    next_to(house(green, _, _, _, _), house(white, _, _, _, _), Houses),
    member(house(_, _, _, old_gold, snails), Houses),
    member(house(yellow, _, _, kools, _), Houses),
    Houses = [_, _, house(_, _, milk, _, _), _, _],
    Houses = [house(_, norwegian, _, _, _)|_],
    next_to(house(_, _, _, chesterfields, _), house(_, _, _, _, fox), Houses),
    next_to(house(_, _, _, kools, _), house(_, _, _, _, horse), Houses),
    member(house(_, _, orange_juice, lucky_strike, _), Houses),
    member(house(_, japanese, _, parliaments, _), Houses),
    next_to(house(_, norwegian, _, _, _), house(blue, _, _, _, _), Houses),
    member(house(_, ZebraOwner, _, _, zebra), Houses),
    member(house(_, WaterDrinker, water, _, _), Houses).

next_to(A, B, List) :- left_of(A, B, List).
next_to(A, B, List) :- left_of(B, A, List).

left_of(Left, Right, [Left, Right|_]).
left_of(Left, Right, [_|Rest]) :- left_of(Left, Right, Rest).

% Query example:
% ?- solve(ZebraOwner, WaterDrinker).

