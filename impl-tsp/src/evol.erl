-module(evol).
-export([]).

mate(_Creature,_Creature) ->
    {ok,mated}.

mutate(_Creature) ->
    ok. % random

% Soll Wert in fitnessliste abspeichern
fitness(_Creature) ->
    ok.
% getfittest() -> ok.
