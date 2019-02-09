:- multifile([type/1, pred/1, local_pred/1, open_pred/1, skipped/1, user_unit/1]).

user_unit(mondo2).
%  Un mondo con 5 stanze, balcone e pattumiera sul balcone

type([a,b,c,d,e,balcone]:luogo).

pred(porta(luogo,luogo)).
%  porta(S1,S2) : c'è una porta che si apre da S1 a S2
%  MODO (?,?) nondet.   Data Base
pred(comunica(luogo,luogo)).
%  comunica(S1,S2) :  S1 e S2 comunicano attraverso una porta
%  MODO (?,?) nondet.
pred(stanza(luogo)).
%  stanza(S) :  S è una stanza, non il balcone
%  MODO (?) nondet
pred(distanza_balcone(luogo,number)).
%  distanza_balcone(S,D) :  D distanza di S dal balcone
%  MODO(?,?) nondet

stanza(a).
stanza(b).
stanza(c).
stanza(d).
stanza(e).


porta(balcone,a).
porta(a,b).
porta(b,c).
porta(c,d).
porta(d,a).
porta(d,e).

distanza_balcone(a,1).
distanza_balcone(b,2).
distanza_balcone(c,3).
distanza_balcone(d,2).
distanza_balcone(e,3).

comunica(S1,S2) :-
	porta(S1,S2); porta(S2,S1).




