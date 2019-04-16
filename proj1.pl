% ----- Antonio Romeu Paulo Pinheiro, 92427 ----- %

:- use_module(library(lists)).

% ----- Predicados Auxiliares ----- %

conta_ocur(_, [], 0).
conta_ocur(P, [P | R], N) :-
    conta_ocur(P, R, N1),
    !, N is N1 + 1.
conta_ocur(P, [_ | R], N) :-
    conta_ocur(P, R, N).

conta_num(P, L, N) :-
    exclude(var, L, L1),
    conta_ocur(P, L1, N).

conta_variaveis(L, Cont) :-
    include(var, L, L_vars),
    length(L_vars, Cont).

preenche_triplo([X, Y, Z], Res) :-
    conta_variaveis([X, Y, Z], Num_Vars),
    (Num_Vars == 1, preenche_1([X, Y, Z], Res);
    Num_Vars == 0, (conta_ocur(1, [X, Y, Z], N), N =\= 3, N > 0;
                    conta_ocur(0, [X, Y, Z], N), N =\= 3, N > 0),
                    Res = [X, Y, Z];
    Num_Vars == 0, (conta_ocur(1, [X, Y, Z], N), N == 3, false;
                    conta_ocur(0, [X, Y, Z], N), N == 3, false);
    Num_Vars == 2, Res = [X, Y, Z]).

preenche_1([X, Y, Z], Res) :-
    (var(X), NX is 2 - Z - Y, (NX == 2, Res = [1, Y, Z];
                                NX == 1, Res = [X, Y, Z];
                                NX == 0, Res = [NX, Y, Z]);
    var(Y), NY is 2 - Z - X, (NY == 2, Res = [X, 1, Z];
                                NY == 1, Res = [X, Y, Z];
                                NY == 0, Res = [X, NY, Z]);
    var(Z), NZ is 2 - X - Y, (NZ == 2, Res = [X, Y, 1];
                                NZ == 1, Res = [X, Y, Z];
                                NZ == 0, Res = [X, Y, NZ])).

preenche_lista_aux([X, Y, Z], Res) :-
    preenche_triplo([X, Y, Z], Res).

preenche_lista_aux([X, Y, Z | R], Res) :-
    preenche_triplo([X, Y, Z], [NX, NY, NZ]),
    preenche_lista_aux([NY, NZ | R], Res_Temp),
    Res = [NX | Res_Temp].

preenche_lista(L, Res) :-
    preenche_lista_aux(L, Res_Temp),
    (L == Res_Temp, Res = Res_Temp;
     preenche_lista(Res_Temp, Res)).

verifica_se_quantidade_ultrapassa(Fila, D) :-
    conta_num(D, Fila, N), length(Fila, C), Comp is C / 2, N =< Comp.

verifica_se_sao_metade(Fila, D) :-
    conta_num(D, Fila, N), length(Fila, C), Comp is C / 2, N == Comp.

adiciona_elementos(_, [], L, L).
adiciona_elementos(El, [P | L], N_L, N_L1) :-
    (number(P), append(N_L, [P], N_L0);
    \+number(P), append(N_L, [El], N_L0)),
    adiciona_elementos(El, L, N_L0, N_L1).

% ----- Predicados Principais ----- %

aplica_R1_triplo(Triplo, N_Triplo) :-
    preenche_triplo(Triplo, N_Triplo).

aplica_R1_fila_aux(Fila, N_Fila) :-
    preenche_lista_aux(Fila, N_Fila).

aplica_R1_fila(Fila, N_Fila) :-
    preenche_lista(Fila, N_Fila).

aplica_R2_fila(Fila, N_Fila) :-
    verifica_se_quantidade_ultrapassa(Fila, 0), verifica_se_quantidade_ultrapassa(Fila, 1),
    (verifica_se_sao_metade(Fila, 0), adiciona_elementos(1, Fila, [], N_Fila);
    verifica_se_sao_metade(Fila, 1), adiciona_elementos(0, Fila, [], N_Fila);
    Fila = N_Fila).