%classic zebra puzzle
%Red = 2 means 2nd house is red. 

:- use_module(library(clpfd)).

einstein(Colors, Smokes, Pets, Drinks, Nations) :- 
  Colors = [Red, Green, Yellow, Ivory, Blue], 
  Smokes = [Old_Gold, Chesterfields, Kools, Parliaments, Lucky_Strike], 
  Pets = [Dog, Fox, Horse, Snails, Zebra], 
  Drinks = [Milk, Tea, Coffee, Juice, Water], 
  Nations = [England, Spain, Ukraine, Norway, Japan],
  
  Colors ins 1..5, 
  Smokes ins 1..5, 
  Pets ins 1..5,
  Drinks ins 1..5, 
  Nations ins 1..5, 
  
  maplist(all_distinct, [Colors, Smokes, Pets, Drinks, Nations]),
  
  %rules
  England #= Red,
  Spain #= Dog, 
  Coffee #= Green, 
  Ukraine #= Tea, 
  Green #= Ivory + 1, 
  Old_Gold #= Snails, 
  Kools #= Yellow, 
  Milk #= 3, 
  Norway #= 1, 
  abs(Chesterfields - Fox) #= 1, 
  abs(Kools - Horse) #= 1, 
  Lucky_Strike #= Juice,
  Japan #= Parliaments, 
  abs(Norway - Blue) #= 1, 

  %print out solution  
  append([Colors, Smokes, Pets, Drinks, Nations], Vars), 
  label(Vars).  
  
  % DONE!!! Yay. 
  
%run
%einstein([Red, Green, Yellow, Ivory, Blue], [Old_Gold, Chesterfields, Kools, Parliaments, Lucky_Strike], [Dog, Fox, Horse, Snails, Zebra], [Milk, Tea, Coffee, Juice, Water], [England, Spain, Ukraine, Norway, Japan]).

einstein([_,_,_,_,_], [_,_,_,_,_], [_,_,_,_, Zebra], [_,_,_,_, Water], [_,_,_,_,_]).
