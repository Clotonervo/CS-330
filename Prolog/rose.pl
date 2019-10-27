

rose('Cottage Beauty').
rose('Golden Sunset').
rose('Mountain Bloom').
rose('Pink Paradise').
rose('Sweet Dreams').

event('anniversary').
event('charity auction').
event('retirement').
event('senior prom').
event('wedding').

item('balloons').
item('candles').
item('chocolates').
item('place cards').
item('streamers').

person('Hugh').
person('Ida').
person('Jeremy').
person('Leroy').
person('Stella').

solve :-
    rose(HughRose),
    rose(IdaRose),
    rose(JeremyRose),
    rose(LeroyRose),
    rose(StellaRose),
    all_different([HughRose, IdaRose, JeremyRose, LeroyRose, StellaRose]),

    event(HughEvent),
    event(IdaEvent),
    event(JeremyEvent),
    event(LeroyEvent),
    event(StellaEvent),
    all_different([HughEvent, IdaEvent, JeremyEvent, LeroyEvent, StellaEvent]),

    item(HughItem),
    item(IdaItem),
    item(JeremyItem),
    item(LeroyItem),
    item(StellaItem),
    all_different([HughItem, IdaItem, JeremyItem, LeroyItem, StellaItem]),

    Quad = [ ['Hugh', HughRose, HughEvent, HughItem],
             ['Ida', IdaRose, IdaEvent, IdaItem],
             ['Jeremy', JeremyRose, JeremyEvent, JeremyItem],
             ['Leroy', LeroyRose, LeroyEvent, LeroyItem],
             ['Stella', StellaRose, StellaEvent, StellaItem] ],

    % 1. Jeremy made a purchase for the senior prom. Stella (who didn't choose flowers for a wedding) picked the Cottage Beauty variety.
    member(['Jeremy', _, 'senior prom', _], Quad),
    \+ member(['Stella', _, 'wedding', _], Quad),
    member(['Stella', 'Cottage Beauty', _, _], Quad),

     % 2.
     member(['Hugh', 'Pink Paradise', _, _], Quad),
     \+ member(['Hugh', _, 'charity auction', _], Quad),
     \+ member(['Hugh', _, 'wedding', _], Quad),

     % 3.
     member([_, _, 'anniversary', 'streamers'], Quad),
     member([_, _, 'wedding', 'balloons'], Quad),

     % 4.
     member([_, 'Sweet Dreams', _, 'chocolates'], Quad),
     \+ member(['Jeremy', 'Mountain Bloom', _, _], Quad),

     % 5.
     member(['Leroy', _, 'retirement', _], Quad),
     member([_, _, 'senior prom', 'candles'], Quad),

     tell('Hugh', HughRose, HughItem, HughEvent),
     tell('Ida', IdaRose, IdaItem, IdaEvent),
     tell('Jeremy', JeremyRose, JeremyItem, JeremyEvent),
     tell('Leroy', LeroyRose, LeroyItem, LeroyEvent),
     tell('Stella', StellaRose, StellaItem, StellaEvent).


all_different([H | T]) :- member(H, T), !, fail.
all_different([_ | T]) :- all_different(T).
all_different([_]).

tell(X, Y, Z, V) :-
    write(X), write(' got '), write(Y),
    write(' flowers and '), write(Z), write(' for a '), write(V), write('.'), nl.


