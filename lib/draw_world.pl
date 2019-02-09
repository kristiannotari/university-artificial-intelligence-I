:- module(draw_world, [new_picture/4,
		       destroy/1,
		       draw_fig/3,
		       draw_fig/5,
		       del_fig/3,
		       fig/4,
		       move_fig/4]).
:- style_check(-discontiguous).
:- dynamic(picture/5).
:- dynamic(fig/4).

type([p(integer,integer)]:point).
%  punto astratto (un quadretto)

type([col(colour), text(atom)]:filling).
%  riempimento colore e testo, ampliabile
%
type([box(integer,list(filling)),
        % box(Size, Fill) : quadrato con lato lungo Size e filling Fill
      circ(integer, list(filling))
        % circ(Size, Fill) : cerchio con diametro lungo Size e filling Fill
     ]: fig).
%  NOTA:
%  Filling = []	 :  bianco
%  member(col(C), Filling) :  di colore C
%  member(text(T), Filling) :  con iscrizione T

pred(new_picture(atom, integer, integer, integer)).
%  new_picture(PicName, NCol, NRow, Size) :  apre una nuova immagine
%  in cui i punti sono quadretti di lato Size, con NCol colonne e NRow
%  righe, identificata dal nome PicName; se c'era già una immagine con
%  quel nome, viene ricoperta
%  MODO (++,++,++,++) det
%
pred(draw_fig(atom, fig, point)).
%  draw_fig(PicName, Fig, p(Row, Col)):  Disegna sulla immagine
%  di nome PicName la figura Fig nel punto p(Row, Col)
%  MODO (++,++,++) semidet (fallisce se PicName non è stata aperta con
%  new_picture )
%
pred(del_fig(atom, fig, point)).
%  del_fig(PicName, Fig, Point) :  cancella la figura Fig in posizione
%  Point
%  MODO (++,??,++) semidet
%
pred(move_fig(atom, fig, point, point)).
%  move_fig(PicName, Fig, P1, P2) : cancella la figura Fig in posizione
%  P1 di PicName e la ricopia in posizione P2
%MODO (++, ??, ++, ++) semidet


fig_size(box(S,_), S).
fig_size(circ(S,_), S).

new_picture(Name, NCol, NRow, Size) :-
	retractall(picture(_, Name, _, _, _)),
	retractall(fig(_,Name,_,_)),
	Width is (NCol+1) * Size + 1,
	Height is (NRow+1) * Size + 1,
	term_to_atom(Name, AName),
	new(Pic, picture(AName, size(Width, Height))),
	send(Pic, open),!,
	assert(picture(Pic, Name, NCol, NRow, Size)).

destroy(Name) :-
	picture(Pic,Name,_,_,_),
	send(Pic,destroy).

draw_fig(Name,Fig, p(Row, Col)) :-
	fig_size(Fig, FigSize),
	picture(Pic, Name, _, _, Size),
	D is (Size-FigSize)/2,
	X is Col*Size + D,
	Y is Row*Size + D,
	new_fig(Fig, F),
	send(Pic, display(F, point(X,Y))),
	send(F, displayed,@on),
	send(Pic,redraw),!,
	assert(fig(F,Name,Fig,p(Row,Col))).

draw_fig(Name,Fig, p(Row, Col), DX, DY) :-
	picture(Pic, Name, _, _, Size),
	X is Col*Size+DX,
	Y is Row*Size+DY,
	new_fig(Fig, F),
	send(Pic, display(F, point(X,Y))),
	send(F, displayed,@on),
	send(Pic,redraw),!,
	assert(fig(F,Name,Fig,p(Row,Col))).

del_fig(Name, Fig, p(Row,Col)) :-
	picture(Pic, Name, _, _, _),
	fig(F, Name, Fig, p(Row, Col)),
	send(F, destroy),
	send(Pic,redraw),!,
	retract(fig(F, Name, Fig, p(Row, Col))).

move_fig(Name, Fig, P1, P2) :-
	del_fig(Name,Fig,P1),!,
	draw_fig(Name,Fig,P2).


new_fig(box(Size, Param), B) :-
	new(B, box(Size, Size)),
	send_fig(B, Size, Param).
new_fig(circ(Size, Param), B) :-
	new(B, circle(Size)),
	send_fig(B, Size, Param).

send_fig(_,_,[]) :- !.
send_fig(B, _, [col(C)]) :-!,
	send(B, fill_pattern, colour(C)).
send_fig(B, Size, [text(T)]) :-!,
	new(Fig, image),
	Left is (Size-14)//2 + 1,
	send(Fig, initialise, aa, Size, Size, pixmap),
        new(Text, text(T, center, font(screen, roman, 14))),
	send(Fig, draw_in, Text, point(Left,0)),
	send(B, fill_pattern, Fig).
send_fig(B, Size, Param) :-
	member(text(T),Param),
	member(col(Col), Param), !,
	new(Fig, image),
	Left is (Size-14)//2 + 1,
	send(Fig, initialise, aa, Size, Size, pixmap),
	send(Fig, background, Col),
        new(Text, text(T, center, font(screen, roman, 14))),
	send(Fig, draw_in, Text, point(Left,0)),
	send(B, fill_pattern, Fig).
send_fig(B, Size, Param) :-
	throw(error(non_implementati(Param),  send_fig(B, Size, Param))).
