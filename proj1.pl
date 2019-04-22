% ----- Antonio Romeu Paulo Pinheiro, 92427 ----- %

:- consult(codigo_comum).

% ----- Predicados Auxiliares ----- %

% conta_ocur(P, L, N)
% Conta numero de ocurrencias do numero P em L e guarda esse valor em N,
% sendo L uma lista composta apenas por numeros
conta_ocur(_, [], 0).
conta_ocur(P, [P | R], N) :-
    conta_ocur(P, R, N1),
    !, N is N1 + 1.
conta_ocur(P, [_ | R], N) :-
    conta_ocur(P, R, N).

% conta_num(P, L, N)
% Conta numero de ocurrencias do numero P em L e guarda esse valor em N
conta_num(P, L, N) :-
    exclude(var, L, L1),
    conta_ocur(P, L1, N).

% conta_variaveis(L, Cont)
% Conta numero de 
conta_variaveis(L, Cont) :-
    include(var, L, L_vars),
    length(L_vars, Cont).

conta_variaveis_matrix(Mat, Cont1) :-
    Cont = 0,
    conta_variaveis_matrix_aux(Mat, Cont, Cont1).

conta_variaveis_matrix_aux([], C, C).
conta_variaveis_matrix_aux([P | R], Cont0, Cont) :-
    conta_variaveis(P, N),
    Cont1 is N + Cont0,
    conta_variaveis_matrix_aux(R, Cont1, Cont).

incr(X, X1) :-
    X1 is X+1.

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

primeira_pos_livre(Mat, Pos) :-
    mat_ref(Mat, Pos, El), var(El), !.

devolve_linha([P | _], (L, _), P, L).
devolve_linha([_ | R], (L, C), Linha, Cont) :-
    incr(Cont, N_Cont), devolve_linha(R, (L, C), Linha, N_Cont).

devolve_coluna(Mat, (L, C), Coluna, Cont) :-
    mat_transposta(Mat, N_Mat),
    devolve_coluna_aux(N_Mat, (L, C), Coluna, Cont).

devolve_coluna_aux([P | _], (_, C), P, C).
devolve_coluna_aux([_ | R], (L, C), Coluna, Cont) :-
    incr(Cont, N_Cont), devolve_coluna_aux(R, (L, C), Coluna, N_Cont).

compara_matrizes(Mat1, Mat2) :-
    flatten(Mat1, Mat1_F), exclude(var, Mat1_F, Mat1_Sem_Var),
    flatten(Mat2, Mat2_F), exclude(var, Mat2_F, Mat2_Sem_Var),
    Mat1_Sem_Var == Mat2_Sem_Var.

compara_colunas(Mat1, _, N_C, _, N_C, _, L, L) :- mat_dimensoes(Mat1, _, C), incr(C, N_C).
compara_colunas(Mat1, Mat2, Cont_C, Cont_L, Cont_L_Aux, Lista_Vazia, Lista0, Lista1) :-
    compara_colunas_aux(Mat1, Mat2, Cont_C, Cont_L, Lista_Vazia, N_Lista), incr(Cont_C, N_Cont_C), incr(Cont_L_Aux, N_Cont_L_Aux),
    (length(N_Lista, Comp), Comp > 0, append(Lista0, N_Lista, Lista2), compara_colunas(Mat1, Mat2, N_Cont_C, Cont_L, N_Cont_L_Aux, Lista_Vazia, Lista2, Lista1);
    compara_colunas(Mat1, Mat2, N_Cont_C, Cont_L, N_Cont_L_Aux, Lista_Vazia, Lista0, Lista1)), !.

compara_colunas_aux(Mat1, Mat2, Cont_C, Cont_L, Lista0, Lista1) :-
    mat_elementos_coluna(Mat1, Cont_C, C1), mat_elementos_coluna(Mat2, Cont_C, C2),
    compara_linhas(C1, C2, Cont_C, Cont_L, Lista0, Lista1).

compara_linhas([], [], _, _, Lista, Lista).
compara_linhas([P1 | R1], [P2 | R2], Cont_C, Cont_L, Lista0, Lista1) :-
    (var(P1), \+var(P2), append(Lista0, [(Cont_L, Cont_C)], Lista), incr(Cont_L, N_Cont_L), compara_linhas(R1, R2, Cont_C, N_Cont_L, Lista, Lista1);
    incr(Cont_L, N_Cont_L), compara_linhas(R1, R2, Cont_C, N_Cont_L, Lista0, Lista1)).

resolve_aux(Puz, Sol) :-
    (primeira_pos_livre(Puz, Pos), (mat_muda_posicao(Puz, Pos, 0, N_Puz0), propaga_posicoes([Pos], N_Puz0, N_Puz1), verifica_R3(N_Puz1);
                                      mat_muda_posicao(Puz, Pos, 1, N_Puz0), propaga_posicoes([Pos], N_Puz0, N_Puz1), verifica_R3(N_Puz1)), resolve_aux(N_Puz1, Sol);
    Sol = Puz).

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
    conta_variaveis_matrix(Puz, N1),
    resolve_inicializa(Puz, N_Puz0),
    conta_variaveis_matrix(N_Puz0, N2),
    (N1 =\= N2, inicializa(N_Puz0, N_Puz);
    N_Puz = N_Puz0).

verifica_R3(Puz) :-
    compara_sublistas(Puz).

propaga_posicoes([], Puz, Puz).
propaga_posicoes([(L, C) | R], Puz, N_Puz) :-
    devolve_linha(Puz, (L, C), Linha, 1), aplica_R1_R2_fila(Linha, N_Linha), mat_muda_linha(Puz, L, N_Linha, N_Puz0),
    devolve_coluna(N_Puz0, (L, C), Coluna, 1), aplica_R1_R2_fila(Coluna, N_Coluna), mat_muda_coluna(N_Puz0, C, N_Coluna, N_Puz1),
    compara_colunas(Puz, N_Puz1, 1, 1, 1, [], [], L1),
    (length(L1, Comp), Comp > 0, append(L1, R, L2), propaga_posicoes(L2, N_Puz1, N_Puz);
    propaga_posicoes(R, N_Puz1, N_Puz)), !.

resolve(Puz, Sol) :-
    inicializa(Puz, N_Puz), verifica_R3(N_Puz), 
    (primeira_pos_livre(N_Puz, _), resolve_aux(N_Puz, Sol);
    Sol = N_Puz).
