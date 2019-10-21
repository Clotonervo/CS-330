

teacher('Ms.Appleton').
teacher('Ms.Gross').
teacher('Mr.Knight').
teacher('Mr.McEvoy').
teacher('Ms.Parnell').

activity('antiquing').
activity('camping').
activity('sightseeing').
activity('spelunking').
activity('water-skiing').

subject('english').
subject('gym').
subject('history').
subject('math').
subject('science').

region('California').
region('Florida').
region('Maine').
region('Oregon').
region('Virginia').

solve :-

activity(AppletonAct),
activity(GrossAct),
activity(KnightAct),
activity(McEvoyAct),
activity(ParnellAct),
all_different([AppletonAct, GrossAct, KnightAct, McEvoyAct, ParnellAct]),


subject(AppletonSub),
subject(GrossSub),
subject(KnightSub),
subject(McEvoySub),
subject(ParnellSub),
all_different([AppletonSub, GrossSub, KnightSub, McEvoySub, ParnellSub]),


region(AppletonReg),
region(GrossReg),
region(KnightReg),
region(McEvoyReg),
region(ParnellReg),
all_different([AppletonReg, GrossReg, KnightReg, McEvoyReg, ParnellReg]),


Quad = [ ['Ms.Appleton', AppletonAct, AppletonSub, AppletonReg],
             ['Ms.Gross', GrossAct, GrossSub, GrossReg],
             ['Mr.Knight', KnightAct, KnightSub, KnightReg],
             ['Mr.McEvoy', McEvoyAct, McEvoySub, McEvoyReg],
             ['Ms.Parnell', ParnellAct, ParnellSub, ParnellReg] ],

% 1.

( member(['Ms.Gross', _, 'math', _], Quad);
	member(['Ms.Gross', _, 'science', _], Quad) ),

( member(['Ms.Gross', 'antiquing', _, _], Quad) -> ( member(['Ms.Gross', _, _,'Florida'], Quad) );
     member(['Ms.Gross', _, _, 'California'], Quad) ),


% 2.

member([_, 'water-skiing', 'science', _], Quad),

( member([_, _, 'science', 'California'], Quad);
	member([_, _, 'science', 'Florida'], Quad) ),

( member(['Mr.McEvoy', _, 'history', 'Maine'], Quad);
	member(['Mr.McEvoy', _, 'history', 'Oregon'], Quad) ),

% 3.



( member([_, _, 'english', 'Virginia'], Quad) ->
	( member(['Ms.Appleton', _, _, 'Virginia'], Quad) );
        member(['Ms.Parnell', _, _, 'Virginia'], Quad) ),

member(['Ms.Parnell', 'spelunking', _, _], Quad),


% 4.
\+ member([_, 'sightseeing', _, 'Maine'], Quad),
\+ member([_, _, 'gym', 'Maine'], Quad),



% 5.
( member(['Ms.Parnell', 'camping', _, _], Quad);
member(['Ms.Appleton', 'camping', _, _], Quad) ),

( member(['Ms.Gross', 'antiquing', _, _], Quad);
  member(['Ms.Appleton', 'antiquing', _, _], Quad);
  member(['Ms.Parnell', 'antiquing', _, _], Quad) ),

\+ member(['Mr.Knight', 'antiquing', _, _], Quad),
\+ member(['Mr.McEvoy', 'antiquing', _, _], Quad),


tell('Ms.Appleton', AppletonAct, AppletonSub, AppletonReg),
tell('Ms.Gross', GrossAct, GrossSub, GrossReg),
tell('Mr.Knight', KnightAct, KnightSub, KnightReg),
tell('Mr.McEvoy', McEvoyAct, McEvoySub, McEvoyReg),
tell('Ms.Parnell', ParnellAct, ParnellSub, ParnellReg).

all_different([H | T]) :- member(H, T), !, fail.
all_different([_ | T]) :- all_different(T).
all_different([_]).

tell(X, Y, Z, V) :-
    write(X), write(', the '), write(Z),
    write(' teacher, is going '), write(Y), write(' in '), write(V), write(' for their summer break.'), nl.

