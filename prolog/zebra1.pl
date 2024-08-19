% toy version of zebra with less stuff
% for testing out how to write prolog. 
% This works to express some pairwise constraints like Englishman lives in Red house,
% but it is clear that this method doesn't scale well. 

valid_xy(a,d). 
valid_xy(b,e). 
valid_xy(X,Y) :- 
  X \= a, 
  X \= b, 
  Y \= d, 
  Y \= e.

valid_xz(c,3). 
valid_xz(X,Z) :- 
  X \= c, 
  Z \= 3.

valid_yz(d,1).
valid_yz(e,2). 
valid_yz(Y,Z) :- 
  Y \= d, 
  Z \= 1, 
  Y \= e, 
  Z \= 2.

valid_constraints(X,Y,Z) :- 
  valid_xy(X,Y), 
  valid_yz(Y,Z), 
  valid_xz(X,Z).

valid_constraints_map([],[],[]).
valid_constraints_map([HeadX|TailX], [HeadY|TailY], [HeadZ|TailZ]) :- 
  valid_constraints(HeadX, HeadY, HeadZ), 
  valid_constraints_map(TailX, TailY, TailZ).
  
% Define a predicate that checks if all elements in a list are different
all_different([]).
all_different([H|T]) :-
    \+ member(H, T),   % H should not be in the tail of the list
    all_different(T).  % Recursively check the rest of the list  


% Define an all members predicate
all_members([], []). 
all_members([], List) :- true.
all_members([Head|Tail], S) :- 
  member(Head, S), 
  all_members(Tail, S).  
  
  
moo(Xs,Ys,Zs) :- 
  Xs = [X1, X2, X3],
  Ys = [Y1, Y2, Y3],
  Zs = [Z1, Z2, Z3],
  all_members(Xs, [a,b,c]),
  all_members(Ys, [d,e,f]), 
  all_members(Zs, [1,2,3]),
  valid_constraints_map(Xs,Ys,Zs),

  all_different(Xs),
  all_different(Ys),
  all_different(Zs).


