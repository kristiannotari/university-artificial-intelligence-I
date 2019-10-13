:- multifile([type/1, pred/1, local_pred/1, open_pred/1, skipped/1, user_unit/1]).

user_unit(mondo2a).
% Un mondo con il tracciato del Mugello e 1 giro

type([
	rettifilo,
	san_donato,
	luco,
	poggio_secco,
	1,
	materassi,
	borgo_san_lorenzo,
	2,
	casanova,
	savelli,
	arrabbiata1,
	arrabbiata2,
	3,
	scarperia,
	palagio,
	4,
	correntaio,
	biondetti1,
	biondetti2,
	5,
	bucine
]:sezione).
type([interna,centrale,esterna]:traiettoria).
type([v1,v2]:velocita).

%=============================================================================== DETTAGLI MACCHINA

usura_massima(50).

velocita(v1).
velocita(v2).

usura_massima_velocita(v1,0).
usura_massima_velocita(v2,Qmax) :- usura_massima(Qmax).

%=============================================================================== TRACCIATO

giri(1).

sez_succ(rettifilo,san_donato).
sez_succ(san_donato,luco).
sez_succ(luco,poggio_secco).
sez_succ(poggio_secco,1).
sez_succ(1,materassi).
sez_succ(materassi,borgo_san_lorenzo).
sez_succ(borgo_san_lorenzo,2).
sez_succ(2,casanova).
sez_succ(casanova,savelli).
sez_succ(savelli,arrabbiata1).
sez_succ(arrabbiata1,arrabbiata2).
sez_succ(arrabbiata2,3).
sez_succ(3,scarperia).
sez_succ(scarperia,palagio).
sez_succ(palagio,4).
sez_succ(4,correntaio).
sez_succ(correntaio,biondetti1).
sez_succ(biondetti1,biondetti2).
sez_succ(biondetti2,5).
sez_succ(5,bucine).
sez_succ(bucine,rettifilo).

griglia(rettifilo,esterna).
traguardo(rettifilo).

curva(san_donato).
curva(luco).
curva(poggio_secco).
curva(materassi).
curva(borgo_san_lorenzo).
curva(casanova).
curva(savelli).
curva(arrabbiata1).
curva(arrabbiata2).
curva(scarperia).
curva(palagio).
curva(correntaio).
curva(biondetti1).
curva(biondetti2).
curva(bucine).

rettilineo(rettifilo).
rettilineo(1).
rettilineo(2).
rettilineo(3).
rettilineo(4).
rettilineo(5).

traiettoria(interna).
traiettoria(centrale).
traiettoria(esterna).

pitlane_costo(20).
pitlane_in(bucine,esterna).
pitlane_out(san_donato,interna).

%=============================================================================== COSTI DI GARA

costo(v1,S,centrale,2,1) :- rettilineo(S).
costo(v2,S,centrale,1,2) :- rettilineo(S).
costo(V,S,T,3,3) :-
	velocita(V),
	traiettoria(T),
	rettilineo(S).
costo(v1,S,interna,2,1) :- curva(S).
costo(v2,S,interna,1,2) :- curva(S).
costo(v1,S,centrale,4,2) :- curva(S).
costo(v2,S,centrale,3,3) :- curva(S).
costo(v1,S,esterna,6,5) :- curva(S).
costo(v2,S,esterna,5,7) :- curva(S).