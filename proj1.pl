% ----- Antonio Romeu Paulo Pinheiro, 92427 ----- %

% ----- Bibliotecas ----- %

:- include('codigo_comum').
:- use_module(library(lists)).
:- use_module(library(clpfd)).

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
    (Num_Vars == 0, (conta_ocur(1, [X, Y, Z], N), N =\= 3, N > 0;
                    conta_ocur(0, [X, Y, Z], N), N =\= 3, N > 0),
                    Res = [X, Y, Z];
    Num_Vars == 0, (conta_ocur(1, [X, Y, Z], N), N == 3, false;
                    conta_ocur(0, [X, Y, Z], N), N == 3, false);
    Num_Vars == 1, preenche_1([X, Y, Z], Res);
    Num_Vars == 2, Res = [X, Y, Z];
    Num_Vars == 3, Res = [X, Y, Z]).

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

aplica_R1_R2_linhas([], N_Puz, N_Puz).
aplica_R1_R2_linhas([P | R], N_Puz, N_PuzAux) :-
    aplica_R1_R2_fila(P, N_L),
    append(N_Puz, [N_L], N_Puz1),
    aplica_R1_R2_linhas(R, N_Puz1, N_PuzAux).

aplica_R1_R2_colunas(L, N_Puz, N_Puz_Final) :-
    mat_transposta(L, L_Transposta),
    aplica_R1_R2_linhas(L_Transposta, N_Puz, N_PuzAux),
    mat_transposta(N_PuzAux, N_Puz_Final).

resolve_inicializa(Puz, Novo) :-
	aplica_R1_R2_puzzle(Puz, N_Puz),
	(Puz = N_Puz -> Novo = N_Puz;
	resolve_inicializa(N_Puz, Novo)).

compara_sublistas([]).
compara_sublistas([L_Aux | R1]) :-
    compara_sublistas_aux(L_Aux, R1),
    compara_sublistas(R1).

compara_sublistas_aux(_, []).
compara_sublistas_aux(L_Aux, [P | R]) :-
    (conta_variaveis(L_Aux, V), V \== 0, compara_sublistas_aux(L_Aux, R);
    conta_variaveis(L_Aux, V), V == 0, L_Aux \== P, compara_sublistas_aux(L_Aux, R);
    conta_variaveis(L_Aux, V), V == 0, L_Aux == P, false).

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

aplica_R1_R2_fila(Fila, N_Fila) :-
    aplica_R1_fila(Fila, N_Fila0) -> aplica_R2_fila(N_Fila0, N_Fila); false.

aplica_R1_R2_puzzle(Puz, N_Puz) :-
    aplica_R1_R2_linhas(Puz, [], N_Puz0) -> aplica_R1_R2_colunas(N_Puz0, [], N_Puz); false.

inicializa(Puz, N_Puz) :-
    resolve_inicializa(Puz, N_Puz0) -> resolve_inicializa(N_Puz0, N_Puz); false.

verifica_R3(Puz) :-
    compara_sublistas(Puz).

propaga_posicoes(P, Puz, Puz_F) :-
    (mat_ref(Puz, P, El), var(El), mat_muda_posicao(Puz, P, 0, Puz0), inicializa(Puz0, Puz_F), verifica_R3(Puz_F);
    mat_ref(Puz, P, El), var(El), mat_muda_posicao(Puz, P, 1, Puz0), inicializa(Puz0, Puz_F), verifica_R3(Puz_F)).

resolve(Puz, Sol) :-
	propaga_posicoes(_, Puz, N_Puz),
	(Puz = N_Puz -> Sol = N_Puz;
	resolve(N_Puz, Sol)).

%[[1,0,_,_,_,1],[_,_,_,_,_,1],[_,_,_,_,_,0],[1,0,1,1,0,0],[_,1,_,0,_,1],[0,1,0,1,1,0]]
%[[0,_,_,1],[0,_,_,1],[1,_,_,_],[1,0,_,_]]
%[[0,1,0,1],[1,_,_,_],[0,_,_,1],[1,0,_,_]]
%[[0,1,0,1],[0,1,0,1],[1,_,_,_],[1,0,_,_]]
%[[_,_,0,_,_,1],[_,_,_,_,_,1],[_,_,_,_,_,_],[_,_,_,_,0,0],[_,1,_,_,_,_],[0,_,0,_,_,_]]