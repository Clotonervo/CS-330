

teacher('Ms.Appleton').
teacher('Ms.Gross').
teacher('Mr.Knight').
teacher('Mr.McEnvoy').
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
activity(McEnvoyAct),
activity(ParnellAct),
all_different([AppletonAct, GrossAct, KnightAct, McEnvoyAct, ParnellAct]),


subject(AppletonSub),
subject(GrossSub),
subject(KnightSub),
subject(McEnvoySub),
subject(ParnellSub),
all_different([AppletonSub, GrossSub, KnightSub, McEnvoySub, ParnellSub]),


region(AppletonReg),
region(GrossReg),
region(KnightReg),
region(McEnvoyReg),
region(ParnellReg),
all_different([AppletonReg, GrossReg, KnightReg, McEnvoyReg, ParnellReg]),


Quad = [ ['Ms.Appleton', AppletonAct, AppletonSub, AppletonReg],
             ['Ms.Gross', GrossAct, GrossSub, GrossReg],
             ['Mr.Knight', KnightAct, KnightSub, KnightReg],
             ['Mr.McEnvoy', McEnvoyAct, McEnvoySub, McEnvoyReg],
             ['Ms.Parnell', ParnellAct, ParnellSub, ParnellReg] ],

% 1.

( member(['Ms.Gross', _, 'math', _], Quad);
	member(['Ms.Gross', _, 'science', _], Quad) ),

( member(['Ms.Gross', 'antiquing', _, 'Florida'], Quad);
	(member(['Ms.Gross', _, _, 'California'], Quad),
		\+ member(['Ms.Gross', 'antiquing', _, 'Florida'], Quad) ) ) ,


% 2. DONE!

( member([_, 'water-skiing', 'science', 'California'], Quad);
	member([_, 'water-skiing', 'science', 'Florida'], Quad) ),

( member(['Mr.McEnvoy', _, 'history', 'Maine'], Quad);
	member(['Mr.McEnvoy', _, 'history', 'Oregon'], Quad) ),

% 3.

member(['Ms.Parnell', 'spelunking', _, _], Quad),

( ( member([_, _, 'english', 'Virginia'], Quad),
	member(['Ms.Appleton', _, _, _], Quad) );
	(\+ member([_, _, 'english', 'Virginia'], Quad),
	member(['Ms.Parnell', _, _, _], Quad) ) ),


% 4.
\+ member([_, 'sightseeing', _, 'Maine'], Quad),
\+ member([_, _, 'gym', 'Maine'], Quad),



% 5.

\+ member(['Ms.Gross', 'camping', _, _], Quad),

( member(['Ms.Gross', 'antiquing', _, _], Quad);
  member(['Ms.Appleton', 'antiquing', _, _], Quad);
  member(['Ms.Parnell', 'antiquing', _, _], Quad) ),


tell('Ms.Appleton', AppletonAct, AppletonSub, AppletonReg),
tell('Ms.Gross', GrossAct, GrossSub, GrossReg),
tell('Mr.Knight', KnightAct, KnightSub, KnightReg),
tell('Mr.McEnvoy', McEnvoyAct, McEnvoySub, McEnvoyReg),
tell('Ms.Parnell', ParnellAct, ParnellSub, ParnellReg).

all_different([H | T]) :- member(H, T), !, fail.
all_different([_ | T]) :- all_different(T).
all_different([_]).

tell(X, Y, Z, V) :-
    write(X), write(', the '), write(Z),
    write(' teacher, is going '), write(Y), write(' in '), write(V), write(' for their summer break.'), nl.

