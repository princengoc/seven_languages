%smaller version for debugging
%Color = 2 means Green is the color of the second house. 
%This is an inferior implementation vs zebra.pl. Included for educational purposes. 
:- use_module(library(clpfd)).

colors([red, green, blue]). 
pets([dog, cat, bird]).
drinks([milk, water, juice]). 

relative_rules(Color, Pet, Drink) :- 
  (Color #= 1) #<==> (Drink #= 2), 
  (Pet #= 1) #<==> (Color #= 2). 

%shifting rules
rule_one([C1,C2,C3]) :- 
  (C1 #= 1) #<==> (C2 #= 3), 
  (C2 #= 1) #<==> (C3 #= 3). 
  
rule_four([P1,P2,P3], [D1,D2,D3]) :- 
  (P1 #= 3 #/\ D2 #= 1) #\/
  (P2 #= 3 #/\ D3 #= 1) #\/  
  (P2 #= 3 #/\ D1 #= 1) #\/    
  (P3 #= 3 #/\ D2 #= 1).


rule_five([P1,P2,P3], [D1,D2,D3]) :- 
  (P1 #= 2 #/\ D2 #= 3) #\/
  (P2 #= 2 #/\ D3 #= 3) #\/  
  (P2 #= 2 #/\ D1 #= 3) #\/    
  (P3 #= 2 #/\ D2 #= 3).
  
einstein(Colors, Pets, Drinks) :-
  Colors = [C1, C2, C3], 
  Pets = [P1, P2, P3], 
  Drinks = [D1, D2, D3], 

  append([Colors, Pets, Drinks], Vars), 
  Vars ins 1..3, 
  
  maplist(all_distinct, [Colors, Pets, Drinks]),   
  
  maplist(relative_rules, Colors, Pets, Drinks), 
  
  rule_one(Colors), 
  rule_four(Pets, Drinks), 
  rule_five(Pets, Drinks), 
  
  label(Vars).
