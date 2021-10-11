:- ensure_loaded('checker.pl').

%test_mode(detailed).

% Considerăm următoarele reprezentări:
%
% O integramă este reprezentată prin structura (compusul)
% integ(H, W, Lista, Vocab), unde:
% H este înălțimea integramei
% W este lățimea integramei
% Lista este o listă de tupluri (Poz, Valoare), unde
%   Poz este un tuplu (R, C) conținând rândul și coloana (0-based)
%   Valoare este una dintre:
%     x - dacă celula este neagră (nu poate fi completată cu litere)
%     o literă, dacă celula este completată cu o literă
%     o listă de întrebări, reprezentate ca tupluri (Text, Dir, ID), cu
%       Text - un srting, textul întrebării
%       Dir - una dintre valorile j sau d, indicând direcția întrebării
%       ID - un identificator numeric al întrebării
% Vocab este o listă de stringuri reprezentând cuvinte disponibile
% pentru a rezolva întrebarea.
%
% În ieșirea predicatului intrebări, o întrebare este reprezentată ca
% ((R, C), Text, Dir, ID), unde
% R este rândul căsuței cu întrebarea (0-based)
% C este coloana căsuței cu întrebarea (0-based)
% Text este textul întrebării (un string)
% Dir este j sau d, reprezentând direcția în care trebuie să fie plasat
% răspunsul (jos sau dreapta)
% ID este un identificator numeric al întrebării.

% Puteți vizualiza integramele cu:
% integrama(0, W), print_integrama(W).
% integrama(1, W), print_integrama(W).
% integrama(2, W), print_integrama(W).
% integrama(3, W), print_integrama(W).
%
% Testați cu
% vmtest.
% Testați teste individuale (vedeți predicatul tt din checker.pl) cu
% vmtest(Test).
% de exemplu cu vmtest(intrebari).


% intrebari/2
% intrebari(integ(+H, +W, +Lista, +Vocab), -Lista_intrebari)
% Este adevărat atunci când Lista_intrebari este o lista de tupluri
% ((R, C), Text, Dir, ID), fiecare tuplu corespunzând unei întrebări din
% integramă (rândul, coloana, textul întrebării, direcția (j/d),
% identificatorul).
% BONUS: intrebari are o singură soluție (o singură listă) pentru o
% anumită integramă.
myConcat([],L,L).
myConcat([H1|T1],L2,[H1|TSol]) :- myConcat(T1, L2, TSol).

isQuestion(T) :- \+ T = (_, x).

modQuestions(_, [], []).
modQuestions(RC, [H|T], [M|L]) :- H = (Q, Dir, Id), M = (RC, Q, Dir, Id), modQuestions(RC, T, L).

listQuestions([], []).
listQuestions([H|T], Rest) :- isQuestion(H), H = (RC, Q), modQuestions(RC, Q, M), listQuestions(T, L), myConcat(M, L, Rest), !.
listQuestions([H|T], L) :- \+ isQuestion(H), listQuestions(T, L).

intrebari(_, _) :- false.
intrebari(integ(_, _, L, _), Q) :- listQuestions(L, Q).

% id_intrebare/2
% id_intrebare(+Integ, ?Intrebare, ?Q_ID)
% Este adevărat dacă în integrama reprezentată ca integ(...), Intrebare
% este un text iar Q_ID este un identificator care corespund aceleași
% întrebări.
id_intrebare(_, _, _) :- false.
id_intrebare(integ(_, _, LQ, _), Q_Text, Q_Id) :- intrebari(integ(_, _, LQ, _), Q), member((_, Q_Text, _, Q_Id), Q).

% completare/3
% completare(+Integ, +Sol, -Integrama)
% Predicatul produce Integrama, o structură de forma integ(...),
% pornind de la Integ, în care au fost completate celule conform cu
% soluția Sol.
% Soluția este reprezentată ca o listă de perechi (Întrebare, Răspuns),
% unde Întrebarea este textul unei întrebări, iar Răspuns este un cuvând
% de completat; ambele sunt stringuri.
% De exemplu, o soluție parțială pentru integrama 0 poate fi:
% [('Din care plouă', 'NOR'), ('Al doilea număr', 'DOI')]
% BONUS: lungime_spatiu are o singură soluție pentru o anumită
% întrebare.
% Puteți testa manual predicatul cu o interogare de forma:
% integrama(0, W), solutie(0, Sol), completare(W, Sol, W2),
%   print_integrama(W2).

cautareCoordonate(_, _, _) :- false.
cautareCoordonate([((R, C), I, Dir, _)|_], (I, _), ((R, C), Dir)).
cautareCoordonate([_|T], I, Raspuns) :- cautareCoordonate(T, I, Raspuns).

generareListaRaspuns(_, _, _) :- false.
generareListaRaspuns(_, _, [], []).
generareListaRaspuns((R, C), (Rp, Cp), [H|T], [M|L]) :-
	Rf is R + Rp, Cf is C + Cp,
	M = ((Rf, Cf), H),
	generareListaRaspuns((Rf, Cf), (Rp, Cp), T, L).

generareLista(_, _, _) :- false.
generareLista(_, [], []).
generareLista(Q, [H|T], L) :-
	cautareCoordonate(Q, H, ((R, C), d)), H = (_, Sol),
	atom_chars(Sol, ListaChars),
	generareListaRaspuns((R, C), (0, 1), ListaChars, ListaRaspunsuri),
	generareLista(Q, T, L2),
	myConcat(ListaRaspunsuri, L2, L).

generareLista(Q, [H|T], L) :-
	cautareCoordonate(Q, H, ((R, C), j)), H = (_, Sol),
	atom_chars(Sol, ListaChars),
	generareListaRaspuns((R, C), (1, 0), ListaChars, ListaRaspunsuri),
	generareLista(Q, T, L2),
	myConcat(ListaRaspunsuri, L2, L).

eliminaDuplicate(_, _) :- false.
eliminaDuplicate([], []).
eliminaDuplicate([H|T], [H|L]) :- \+ member(H, T), eliminaDuplicate(T, L), !.
eliminaDuplicate([_|T], L) :- eliminaDuplicate(T, L).

completare(_, _, _) :- false.
completare(integ(R, C, L, Vocab), Sol, integ(R, C, Lfinal, Vocab)) :-
	intrebari(integ(R, C, L, Vocab), Q),
	generareLista(Q, Sol, ListaCuRaspunsuri),
	eliminaDuplicate(ListaCuRaspunsuri, ListaCuRaspunsuri2),
	myConcat(L, ListaCuRaspunsuri2, Lfinal), !.


% lungime_spatiu/3
% lungime_spatiu(integ(+H, +W, +Lista, +Vocab), +Intrebare, -Lungime)
% Returnează lungimea spațiului asociat întrebării date.
% Întrebarea este indicată prin textul ei. De exemplu:
% lungime_spatiu pentru integrama 0 și întrebarea 'Al doilea număr'
% trebuie să lege Lungime la 3.
% BONUS: lungime_spatiu are o singură soluție pentru o anumită
% întrebare.
% Puteți testa manual predicatul cu o interogare de forma:
% integrama(0, W), id_intrebare(W, Text, 3), lungime_spatiu(W, Text, X).
lungime_intrebare((R, C), (Rp, Cp), Lista, 0) :- Rf is R + Rp, Cf is C + Cp, member(((Rf, Cf), _), Lista).
lungime_intrebare((R, C), (Rp, Cp), Lista, Len1) :- Rf is R + Rp, Cf is C + Cp, \+ member(((Rf, Cf), _), Lista), lungime_intrebare((Rf, Cf), (Rp, Cp), Lista, Len2), Len1 is 1 + Len2.

lungime_spatiu(_, _, _) :- false.
lungime_spatiu(integ(_, _, Lista, _), Q, Len) :- intrebari(integ(_, _, Lista, _), Qs), member(((R, C), Q, d, _), Qs), lungime_intrebare((R, C), (0, 1), Lista, Len).
lungime_spatiu(integ(_, _, Lista, _), Q, Len) :- intrebari(integ(_, _, Lista, _), Qs), member(((R, C), Q, j, _), Qs), lungime_intrebare((R, C), (1, 0), Lista, Len).


% intersectie/5
% intersectie(integ(+H, +W, +Lista, +Voc), +I1, -Poz1, +I2, -Poz2)
% Pentru o integramă și două întrebări date prin textul lor (I1 și I2),
% al căror răspunsuri se intersectează, întoarce în Poz1 indicele din
% răspunsul la I1 la care este intersecția, și în Poz2 indicele din
% răspunsul la I2 la care este intersecția. Indecșii incep de la 0.
%
% De exemplu, în integrama 0:
%  █       █       2↓      3↓      █
%  █       0↓,1→   -       -       █
%  4→      -       -       -       █
%  5→      -       -       -       █
%  █       █       █       █       █
%
%  Întrebările 'Primii 3 din artă' și 'Afirmativ' (3, respectiv 1) se
%  intersectează la pozițiile 0, respectiv 2 (va fi litera A, de la
%  ART, respectiv DA).
intersectie(_, _, _, _, _) :- false.
intersectie(integ(_, _, Lista, _), I1, Poz1, I2, Poz2) :-
	intrebari(integ(_, _, Lista, _), Qs),
	member(((R1, C1), I1, d, _), Qs),
	member(((R2, C2), I2, j, _), Qs),
	R2 < R1, C1 < C2,
	lungime_intrebare((R1, C1), (0, 1), Lista, Len1),
	lungime_intrebare((R2, C2), (1, 0), Lista, Len2),
	Temp1 is C2 - C1 - 1, Temp2 is R1 - R2 - 1,
	Temp1 < Len1, Temp2 < Len2,
	Poz1 = Temp1, Poz2 = Temp2.

intersectie(integ(_, _, Lista, _), I1, Poz1, I2, Poz2) :-
	intrebari(integ(_, _, Lista, _), Qs),
	member(((R1, C1), I1, j, _), Qs),
	member(((R2, C2), I2, d, _), Qs),
	R1 < R2, C2 < C1,
	lungime_intrebare((R1, C1), (1, 0), Lista, Len1),
	lungime_intrebare((R2, C2), (0, 1), Lista, Len2),
	Temp1 is R2 - R1 - 1, Temp2 is C1 - C2 - 1,
	Temp1 < Len1, Temp2 < Len2,
	Poz1 = Temp1, Poz2 = Temp2.


% solutii_posibile/2
% solutii_posibile(integ(+H, +W, +Lista, +Vocabular), -Solutii)
% Formează o listă Solutii, conținând perechi de forma
% (Întrebare, Cuvinte), unde
% Întrebare este textul unei întrebări din integramă, iar Cuvinte este o
% listă de cuvinte sunt din Vocabular și au lungimea corectă pentru a fi
% răspuns la întrebare. Solutii conține câte o pereche pentru fiecare
% întrebare din integramă.
% Cuvintele sunt reprezentate ca liste de stringuri, fiecare string
% având lungime 1 (o singură literă).
% De exemplu, pentru integrama 0, Solutii conține 6 perechi, două dintre
% ele fiind:
% ('Afirmativ', [['D', 'A'], ['N', 'U']])
% ('Din care plouă',
% [['N','O','R'],['A','R','T'],['U','I','T'],['D','O','I']])
solutii_intrebare(_, _, _) :- false.
solutii_intrebare(_, [], _, []).
solutii_intrebare(integ(_, _, Qs, _), [(_, Q, _, _)|T_Qs], Ras, [Sol|T_Sol]) :-
	findall(X, (member(X, Ras), lungime_spatiu(integ(_, _, Qs, _), Q, Len), length(X, Len)), Pereche),
	Sol = (Q, Pereche), solutii_intrebare(integ(_, _, Qs, _), T_Qs, Ras, T_Sol).

solutii_posibile(_, _) :- false.
solutii_posibile(integ(H, W, L, Vocab), Sol) :-
	intrebari(integ(H, W, L, Vocab), Qs),
	findall(X, (member(Y, Vocab), atom_chars(Y, X)), Vocab_chars),
	solutii_intrebare(integ(_, _, L, _), Qs, Vocab_chars, Sol).

% rezolvare/2
% rezolvare(+Integ, -Solutie)
% Rezolvare produce în Solutie soluția integramei Integ. Soluția este
% reprezentată ca o listă de perechi de stringuri, fiecare pereche
% conținând textul unei întrebări și cuvântul (ca string) care este
% răspunsul la întrebare.
rezolvare(_, _) :- false.
