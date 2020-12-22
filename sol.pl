parents('Александр Макаров','Борис Макаров', 'Елизавета Макарова').
parents('Валерий Макаров','Александр Макаров', 'Елена Макарова').
parents('Елена Макарова','Кузьма Титов', 'Вера Титова').
parents('Борис Макаров','Алексей Макаров', 'Светлана Макарова').
parents('Алексей Макаров','Вячеслав Макаров', 'Ольга Макарова').
parents('Вячеслав Макаров','Виктор Макаров', 'Виктория Макарова').
parents('Владимир Макаров','Борис Макаров', 'Елизавета Макарова').
parents('Сергей Макаров','Владимир Макаров', 'Александра Мышляева').
parents('Антон Макаров','Алексей Макаров', 'Светлана Макарова').
parents('Иван Макаров','Антон Макаров', 'Анастасия Акчурина').
parents('Анастасия Акчурина','Ренат Акчурин', 'Анна Акчурина').
parents('Анна Акчурина','Артур Акчурин', 'Лера Акчурина').
parents('Игорь Акчурин','Артур Акчурин', 'Лера Акчурина').
parents('Олег Акчурин','Игорь Акчурин','  ').
parents('Григорий Макаров','Виктор Макаров', 'Виктория Макарова').
parents('Алексей Макаров','Григорий Макаров', 'Анна Пономарева').
parents('Вера Титова','Александр Володьков', 'Татьяна Володькова').
parents('Татьяна Володькова','Анатолий Володьков', 'Анна Володькова').
parents('Ольга Володькова','Анатолий Володьков', 'Анна Володькова').
parents('Аркадий Володьков','Павел Володьков', 'Ольга Володькова').
parents('Алиса Макарова','Владимир Макаров', 'Александра Мышляева').
parents('Иван Володьков','Александр Володьков', 'Татьяна Володькова').
parents('Федор Макаров','Сергей Макаров', 'Юлия Макарова').
parents('Арсений Володьков','Иван Володьков', 'Людмила Володькова').

sex('Валерий Макаров',male).
sex('Сергей Макаров',male).
sex('Алиса Макарова',female).
sex('Иван Макаров',male).

% ребенок
child(Name,X) :- parents(Name, X, _); parents(Name, _, X).

% брат/сестра
sibling(X, Y) :- parents(X, A, B), parents(Y, A, B), 
    X\=Y.

% двоюродный брат/сестра
cousin(X, Y) :- parents(X, A, B), parents(Y, C, D), (sibling(A, C); sibling(A, D); sibling(B, C); sibling(B, D)),
    X\=Y.

cousin_bro(Name,X) :- ((sibling(A,B), child(Name,A), child(X,B),parents(_,Name,_));
(sibling(A,B), child(X,A), child(Name,B), parents(_,Name,_))),
Name \= X.

relative(son,A,B) :- child(A,B), (parents(_,A,_);sex(A,male)).
relative(daughter,A,B) :- child(A,B), (parents(_,_,A);sex(A,female)).
relative(brother, A,B) :- sibling(A,B), (parents(_,A,_);sex(A,male)).
relative(sister, A,B) :- sibling(A,B), (parents(_,_,A);sex(A,female)).
relative(father, A, B) :- parents(B, A, _).
relative(mother, A, B) :- parents(B, _, A).
relative(husband, A, B) :- parents(_, A, B), !.
relative(wife, A, B) :- parents(_, A, B), !.
relative(cousin_brother, A,B) :- cousin(A, B), (parents(_,A,_);sex(A,male)).
relative(cousin_sister, A,B) :-  cousin(A, B), (parents(_,_,A);sex(A,female)).

    
% поиск в ширину	
prolong([X|T],[Y,X|T]):-
	relative(_,X,Y),
	not(member(Y,[X|T])).
	
bfs([[H|T]|_],H,[H|T]).
bfs([Curr|Other],H,Way):-
	findall(W,prolong(Curr,W),Ways),
	append(Other,Ways,New), !,
	bfs(New,H,Way).

	
add([_], Obj, Obj).
add([X,Y|T], Obj, Relation) :-
    relative(Relate, X, Y),
    add([Y|T], [Relate|Obj], Relation). 

related(Relation, X, Y) :-
    bfs([[X]], Y, R),
	reverse(R, Chain),
	add(Chain, [], Relation1),
	reverse(Relation1, Relation).


relations(X):- member(X, [father, mother, brother, sister, husband, wife, cousin_brother, cousin_sister, son,daughter]).
pronouns(X) :- member(X,[he,she,it]).
pronouns_s(X) :- member(X, [his,her,its]).
verb_to_be(X):- member(X, [is,are]).

% проверка корректности построения вопросов, в зависимости от типа
check_type_how(Q) :- 
    Q = [how,many|_], 
    last(Q,?), 
    without_last(Q,Q1),
    last(Q1,have),
    find_agent(Q,Name),
    my_remove(Name,Q,Q2),
    not(find_agent(Q2,_)),
    find_relation(Q,Rel),
    my_remove(Rel,Q,Q3),
    not(find_relation(Q3,_)).

check_type_who(Q) :- 
    Q = [who|Tail], 
    Tail = [ToBe|_], 
    verb_to_be(ToBe), 
    last(Q,?),
    find_agent(Q,Name),
    my_remove(Name,Q,Q2),
    not(find_agent(Q2,_)),
    find_relation(Q,Rel),
    my_remove(Rel,Q,Q3),
    not(find_relation(Q3,_)).

check_type_is(Q) :-
    Q = [ToBe|_], 
    verb_to_be(ToBe),
    find_agent(Q,Name),
    my_remove(Name,Q,Q2),
    find_agent(Q2,Name1),
    not(Name==Name1),
    find_relation(Q,Rel),
    my_remove(Rel,Q,Q3),
    not(find_relation(Q3,_)).

check_type_how_obj(Q) :-
    Q = [how,many|_], 
    last(Q,?), 
    without_last(Q,Q1),
    last(Q1,have),
    find_pronoun(Q,Pr),
    my_remove(Pr,Q,Q2),
    not(find_pronoun(Q2,_)),
    find_relation(Q,Rel),
    my_remove(Rel,Q,Q3),
    not(find_relation(Q3,_)).

check_type_who_obj(Q) :- 
    Q = [who|Tail], 
    Tail = [ToBe|_], 
    verb_to_be(ToBe), 
    last(Q,?),
    find_pronoun_s(Q,Pr),
    my_remove(Pr,Q,Q2),
    not(find_pronoun_s(Q2,_)),
    find_relation(Q,Rel),
    my_remove(Rel,Q,Q3),
    not(find_relation(Q3,_)).

check_type_is_obj_1(Q) :-
    Q = [ToBe|_], 
    verb_to_be(ToBe),
    find_pronoun(Q,Pr),
    my_remove(Pr,Q,Q2),
    not(find_pronoun(Q2,_)),
    find_agent(Q2,_),
    find_relation(Q,Rel),
    my_remove(Rel,Q,Q3),
    not(find_relation(Q3,_)).

check_type_is_obj_2(Q) :-
    Q = [ToBe|_], 
    verb_to_be(ToBe),
    find_pronoun_s(Q,Pr),
    my_remove(Pr,Q,Q2),
    not(find_pronoun_s(Q2,_)),
    find_agent(Q2,_),
    find_relation(Q,Rel),
    my_remove(Rel,Q,Q3),
    not(find_relation(Q3,_)).
    

% вопрос
q(Q) :- 
% how many Relation Name have ?
    (check_type_how(Q), find_agent(Q,Name),find_relation(Q,Rel),
        findall(Res,relative(Rel, Res, Name),L), length(L,X),
    	write(Name),write(" has "),write(X),write(" "),write(Rel),write("s");
% who is Name Relation ?     
    check_type_who(Q), find_agent(Q,Name),find_relation(Q,Rel),
        relative(Rel, Res, Name), write(Res), write(" is "), write(Name), write("'s "), write(Rel);
% is Name1 Name2 Relation ?    
   check_type_is(Q), find_relation(Q,Rel),find_agent(Q,Name1),my_remove(Name1,Q,Q1),find_agent(Q1,Name2),
        (relative(Rel, Name1, Name2) -> write("Yes. "),write(Name1),write(" is "),write(Name2),write(" "),write(Rel);
      	not(relative(Rel, Name1, Name2)) -> write("No. "),write(Name1),write(" is not "),write(Name2),write(" "),write(Rel));
% how many Relation Pronoun have?    
   check_type_how_obj(Q);
% who is Pronoun Relation?
   check_type_who_obj(Q);
% is Pronoun Name Relation?   
   check_type_is_obj_1(Q);
% is Name Pronoun Relation?
   check_type_is_obj_2(Q)).
   


% вспомогательные предикаты
find_name(X) :- parents(X,_,_),!;parents(_,X,_),!;parents(_,_,X).

find_agent([],[]) :- fail.
find_agent([Head|Tail],Name):-
    find_name(Head), Name = Head,!;
    find_agent(Tail,Name).

find_pronoun([],[]) :- fail.
find_pronoun([Head|Tail],Pr):-
    pronouns(Head), Pr = Head,!;
    find_pronoun(Tail,Pr).

find_pronoun_s([],[]) :- fail.
find_pronoun_s([Head|Tail],Pr):-
    pronouns_s(Head), Pr = Head,!;
    find_pronoun_s(Tail,Pr).

find_relation([],[]) :- fail.
find_relation([Head|Tail],Rel):-
    relations(Head), Rel = Head,!;
    find_relation(Tail,Rel).
    
my_remove(X, [X|Tail], Tail).
my_remove(X, [Y|Tail], [Y|Z]):-my_remove(X, Tail, Z).

last([X], X):-!.
last([_|Tail], Element):- last(Tail, Element).

without_last([_], []).
without_last([X|Xs], [X|WithoutLast]) :- 
    without_last(Xs, WithoutLast).
