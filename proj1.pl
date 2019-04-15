% Antonio Romeu PAulo Pinheiro, 92427

% Funcoes Auxiliares

comprimento([], 0).
comprimento([_ | R], C + 1) :-
    compriento(R, C_aux),
    C is C_aux + 1.

conta_el(_, [], 0).
conta_el(X, [X | T], N) :-
    !,
    conta_el(X, T, N1),
    N is N1 + 1.
conta_el(X, [_ | T], N) :-
    conta_el(X, T, N).

verifica_3_elementos(Triplo) :-
    conta_el(1, Triplo, N),
    N =\= 3,
    N =\= 0,
    true.

% Funcoes Principais

aplica_R1_triplo(Triplo, N_Triplo) :-
    comprimento(Triplo, C),
    C == 3,
    verifica_3_elementos(Triplo).

aplica_R1_triplo(Triplo, N_Triplo) :-
    comprimento(Triplo, C),
    C == 2,
    verifica_2_elementos(Triplo).
