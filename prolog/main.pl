use_module(library(random)).
% Jogo de Batalha Naval
% Símbolos:
% "~" -> Água
% "#" -> Navio
% "o" -> Água sem navio
% "X" -> Navio atingido

main:-
	limpaTela(),
	printaApresentacao(),
	
	read_string(user_input, ".", "\n", _, Opcao),
	executaOpcao(Opcao),

	halt.

limpaTela():-
	write('\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n').

printaApresentacao():-
	write('Bem-vindo a Batalha Naval!\n'),
	write('O que voce ira fazer?\n'),
	write('(n) - Novo Jogo\n'),
	write('(c) - Carregar Jogo\n'),
	write('(s) - Sair do Jogo\n').

executaOpcao("n"):- novoJogo().
executaOpcao("c"):- carregarJogo().
executaOpcao("s"):- halt.
executaOpcao(_):-
	write('Opcao invalida!\n'),
	main.

carregarJogo():-
	write('Carregando jogo'),
	sleep(0.3),
	carregarTabs(Tab_J, Tab_J_Ve_B, Tab_B, Tab_B_Ve_J, Round),
	loopPartida(Tab_J, Tab_J_Ve_B, Tab_B, Tab_B_Ve_J, Round).

salvarTabs(Tab_J, Tab_J_Ve_B, Tab_B, Tab_B_Ve_J, Round):-
	open('t1.txt', write, IO1),
	write(IO1, Tab_J),
	write(IO1, '.'),
	close(IO1),

	open('t2.txt', write, IO2),
	write(IO2, Tab_J_Ve_B),
	write(IO2, '.'),
	close(IO2),

	open('t3.txt', write, IO3),
	write(IO3, Tab_B),
	write(IO3, '.'),
	close(IO3),

	open('t4.txt', write, IO4),
	write(IO4, Tab_B_Ve_J),
	write(IO4, '.'),
	close(IO4),
	
	open('r.txt', write, IO5),
	write(IO5, Round),
	write(IO5, '.'),
	close(IO5).

carregarTabs(Tab_J, Tab_J_Ve_B, Tab_B, Tab_B_Ve_J, Round):-
	open('t1.txt', read, IO1),
	read(IO1, Tab_J),
	close(IO1),

	open('t2.txt', read, IO2),
	read(IO2, Tab_J_Ve_B),
	close(IO2),

	open('t3.txt', read, IO3),
	read(IO3, Tab_B),
	close(IO3),

	open('t4.txt', read, IO4),
	read(IO4, Tab_B_Ve_J),
	close(IO4),
	
	open('r.txt', read, IO5),
	read(IO5, Round),
	close(IO5).

novoJogo():-
	montaTabuleiro("", Tab_J),
	montaTabuleiro("", Tab_J_Ve_B),
	montaTabuleiro("B", Tab_B),
	montaTabuleiro("", Tab_B_Ve_J),

	% salvarTabs(Tab_J, Tab_J_Ve_B, Tab_B, Tab_B_Ve_J, _),

	% % TESTE
	% writeTabs(Tab_JC, Tab_J_Ve_BC, Tab_BC, Tab_B_Ve_JC, _),
	% write('1\n'),
	% write(Tab_JC),
	% write('\n2\n'),
	% write(Tab_J_Ve_BC),
	% write('\n3\n'),
	% write(Tab_BC),
	% write('\n4\n'),
	% write(Tab_B_Ve_JC),
	% write('\n'),
	% sleep(2.5),
	% % 

	limpaTela(),
	write('Hora da preparacao, escolha as posicoes de seus navios!\n\n'),
	
	sleep(2.5),
	limpaTela(),
	posicionaNavios(Tab_J, 5, Tab_J1),
	sleep(1),
	limpaTela(),
	posicionaNavios(Tab_J1, 4, Tab_J2),
	sleep(1),
	limpaTela(),
	posicionaNavios(Tab_J2, 3, Tab_J3),
	sleep(1),
	limpaTela(),
	posicionaNavios(Tab_J3, 3, Tab_J4),
	sleep(1),
	limpaTela(),
	posicionaNavios(Tab_J4, 2, Tab_JF),
	sleep(1),
	limpaTela(),
	
	printaTabEMensagem(Tab_JF, "     Tabuleiro final:\n"),
	sleep(3),

	loopPartida(Tab_JF, Tab_J_Ve_B, Tab_B, Tab_B_Ve_J, 1).

printaTabEMensagem(Tab, Mensagem):-
	write(Mensagem),
	preparaTabParaPrint(Tab, 0, Tab_R),
	write(Tab_R).

vitoria():-
	write('Voce venceu!\n'),
	write('Gostaria de jogar novamente? (s ou n)\n'),
	read(Opcao),
	promptRejogar(Opcao, R),
	R == true ->
		write('Opcao invalida!\n'),
		vitoria();
	!.

promptRejogar(n, false):- 
	write('Fechando jogo...'),
	halt.
promptRejogar(s, false):- main().
promptRejogar(K, R):- K \= n, K \= s, R = true.

derrota():-
	write('Voce perdeu!\n'),
	write('Gostaria de jogar novamente? (s ou n)\n'),
	read(Opcao),

	promptRejogar(Opcao, R),
	R == true ->
		write('Opcao invalida!\n'),
		derrota();
	!.

preparaTabParaPrint([], _, "").
preparaTabParaPrint([[]], _, "").
preparaTabParaPrint(Tab, 0, R):-
	preparaTabParaPrint(Tab, 1, R1),
	string_concat("    1 2 3 4 5 6 7 8 9 10\n", R1, R).
preparaTabParaPrint([H|_], 10, R):-
	insereEspacos(H, "", K),
	string_concat("10 ", K, R1),
	string_concat(R1, "\n", R).
preparaTabParaPrint([H|T], I, R):-
	I < 10,
	string_concat(" ", I, R1),
	string_concat(R1, " ", R2),
	insereEspacos(H, "", K),
	string_concat(R2, K, R3),
	string_concat(R3, "\n", R4),
	NovoI is I + 1,
	preparaTabParaPrint(T, NovoI, R5),
	string_concat(R4, R5, R).

insereEspacos([L|[]], K, R):-
	string_concat(K, L, R).
insereEspacos([T0, T1 | []], K, R):-
	string_concat(K, " ", R1),
	string_concat(R1, T0, R2),
	string_concat(R2, " ", R3),
	string_concat(R3, T1, R4),
	R = R4.
insereEspacos([T0 | T1], K, R):-
	string_concat(K, " ", R1),
	string_concat(R1, T0, R2),
	insereEspacos(T1, R2, R).

concat_list_strings(ListaStrings, R) :-
    maplist(atom_chars, ListaStrings, Listas),
    append(Listas, Lista),
    atom_chars(R, Lista).

contaNaviosTab([H|[]], NumNaviosF):-
	contaNaviosLinha(H, NumNaviosF).

contaNaviosTab([H|T], NumNaviosF):-
	contaNaviosLinha(H, NumNavios),
	contaNaviosTab(T, NumNaviosE),
	NumNaviosF is NumNavios + NumNaviosE.

contaNaviosLinha(["~"|[]], 0).
contaNaviosLinha(["X"|[]], 0).
contaNaviosLinha(["o"|[]], 0).
contaNaviosLinha(["#"|[]], 1).
contaNaviosLinha(["~"|T], NumNaviosF):-
	contaNaviosLinha(T, NumNavios),
	NumNaviosF is 0 + NumNavios.
contaNaviosLinha(["X"|T], NumNaviosF):-
	contaNaviosLinha(T, NumNavios),
	NumNaviosF is 0 + NumNavios.
contaNaviosLinha(["o"|T], NumNaviosF):-
	contaNaviosLinha(T, NumNavios),
	NumNaviosF is 0 + NumNavios.
contaNaviosLinha(["#"|T], NumNaviosF):-
	contaNaviosLinha(T, NumNavios),
	NumNaviosF is 1 + NumNavios.

verificaFinalizacaoPartida(0, K):-
	K \= 0,
	derrota().

verificaFinalizacaoPartida(_, 0):-
	vitoria().

verificaFinalizacaoPartida(A, B):- A =\= 0, B =\= 0.

% TODO: TESTAR TODAS AS FUNCOES COMENTADAS
loopPartida(Tab_J, Tab_J_Ve_B, Tab_B, Tab_B_Ve_J, Round):-
	contaNaviosTab(Tab_J, NumNavios_J),
	contaNaviosTab(Tab_B, NumNavios_B),

	limpaTela(),
	write('Numero de navios restantes do jogador: '),
	write(NumNavios_J),
	write('\n'),
	write('Numero de navios restantes do bot: '),
	write(NumNavios_B),
	write('\n\n'),

	verificaFinalizacaoPartida(NumNavios_J, NumNavios_B),

	printaTabEMensagem(Tab_J, "   Tabuleiro do jogador\n"),
	write('\n'),
	printaTabEMensagem(Tab_J_Ve_B, "   Tabuleiro de ataque\n"),
	write('Round: '),
	write(Round),
	write('\n\n'),

	write('Gostaria de (d)isparar ou (s)alvar? (Isso ira sobrescrever a gravacao existente)\n'),

	read_string(user_input, ".", "\n", _, Opcao),
	
	(Opcao == "d"; Opcao == "s") -> (
		verificaESalva(Opcao, Tab_J, Tab_J_Ve_B, Tab_B, Tab_B_Ve_J, Round),
		write('Iniciando Round.\n'),
		sleep(0.3),
		disparaAoBot(Tab_B, Tab_J_Ve_B, Tab_BF, Tab_J_Ve_BF),
		write('\nVez do bot...\n'),
		sleep(0.6),
		disparaAoJogador(Tab_J, Tab_B_Ve_J, Tab_JF, Tab_B_Ve_JF),
		NovoRound is Round + 1,

		limpaTela(),
		loopPartida(Tab_JF, Tab_J_Ve_BF, Tab_BF, Tab_B_Ve_JF, NovoRound)
	);
	write('Opcao invalida!'),
	sleep(3.0),
	loopPartida(Tab_J, Tab_J_Ve_B, Tab_B, Tab_B_Ve_J, Round).
	
verificaESalva("s", Tab_J, Tab_J_Ve_B, Tab_B, Tab_B_Ve_J, Round):-
	salvarTabs(Tab_J, Tab_J_Ve_B, Tab_B, Tab_B_Ve_J, Round),
	write('\nJogo salvo!\n\n').
verificaESalva(A, _, _, _, _, _):- A \= "s".

disparaAoJogador(Tab_J, Tab_B_Ve_J, Tab_JF, Tab_B_Ve_JF):-
	random(1, 11, X),
	random(1, 11, Y),

	verificaJaDisparadoAoJogador(Tab_B_Ve_J, X, Y, RDisparo),

	verificaDisparoFinal(RXY, RDisparo, RVerificacao),
	RVerificacao == true -> (
		selecionaSimboloNavio(Tab_J, X, Y, Simbolo),
		posicionaSimbolo(Tab_J, Tab_B_Ve_J, X, Y, Simbolo, Tab_JF, Tab_B_Ve_JF)
	);
	disparaAoJogador(Tab_J, Tab_B_Ve_J, Tab_JF, Tab_B_Ve_JF).

disparaAoBot(Tab_B, Tab_J_Ve_B, Tab_BF, Tab_J_Ve_BF):-
	write('Sua vez de disparar, escolha as posicoes X (de 1 a 10) e Y (de 1 a 10).\n'),

	read(X),
	read(Y),

	write('X: '),
	write(X),
	write('\nY: '),
	write(Y),
	write('\n'),

	verificaEntradaX(X, R1),
	verificaEntradaY(Y, R2),
	verificaXY(R1, R2, RXY),
	% write('RXY: '),
	% write(RXY),
	% write('\n'),
	verificaJaDisparadoAoBot(Tab_J_Ve_B, X, Y, RDisparo),

	% write('RDisparo: '),
	% write(RDisparo),
	% write('\n'),

	verificaDisparoFinal(RXY, RDisparo, RVerificacao),
	RVerificacao == true -> (
		% write('Entrou'),
		selecionaSimboloNavio(Tab_B, X, Y, Simbolo),
		posicionaSimbolo(Tab_B, Tab_J_Ve_B, X, Y, Simbolo, Tab_BF, Tab_J_Ve_BF)
	);
	write('Posicao invalida, digite uma nova posicao valida.\n'),
	disparaAoBot(Tab_B, Tab_J_Ve_B, Tab_BF, Tab_J_Ve_BF).

selecionaSimboloNavio(Tab, X, Y, SimboloF):-
	nth1(X, Tab, Linha),
	nth1(Y, Linha, Simbolo),
	verificaSimbolo(Simbolo, SimboloF).

posicionaSimbolo(Tab_B, Tab_J_Ve_B, X, Y, "X", Tab_BF, Tab_J_Ve_BF):-
	write('Voce acertou um navio!\n'),
	posicionaCelula(Tab_B, X, Y, "X", Tab_BF),
	posicionaCelula(Tab_J_Ve_B, X, Y, "X", Tab_J_Ve_BF).

posicionaSimbolo(Tab_B, Tab_J_Ve_B, X, Y, "o", Tab_B, Tab_J_Ve_BF):-
	write('Voce acertou na agua!\n'),
	posicionaCelula(Tab_J_Ve_B, X, Y, "o", Tab_J_Ve_BF).

posicionaCelula(Tab, X, Y, Simbolo, Tab_F):-
	LinhaInserir is X - 1,
	PosInserir is Y - 1,
	remontaNaviosHorizontal(Tab, 1, LinhaInserir, PosInserir, Simbolo, Tab_F).

remontaNaviosHorizontal([HLista|[]], 1, 0, PosInserir, ElementoEntrada, R):- 
	I = PosInserir,
	montaLista(HLista, 0, I, I, [], ElementoEntrada, true, R).

remontaNaviosHorizontal([H|T], 1, 0, PosInserir, ElementoEntrada, R):-
	I = PosInserir,
	montaLista(H, 0, I, I, [], ElementoEntrada, false, R1),
	remontaNaviosHorizontal(T, 1, -1, PosInserir, ElementoEntrada, R2),
	append([R1], R2, R).

remontaNaviosHorizontal([H|[]], _, LinhaInserir, _, _, [H]):- LinhaInserir =\= 0.

remontaNaviosHorizontal([H|T], 1, LinhaInserir, PosInserir, ElementoEntrada, R):-
	LinhaInserir =\= 0,
	NovaLinhaInserir is LinhaInserir - 1,
	remontaNaviosHorizontal(T, 1, NovaLinhaInserir, PosInserir, ElementoEntrada, R1),
	append([H], R1, R).

montaLista(_, 10, _, _, NovaLista, ElementoEntrada, true, R):- R = [NovaLista].
montaLista(_, 10, _, _, NovaLista, ElementoEntrada, false, R):- R = NovaLista.
montaLista(LEntrada, I, MinI, MaxI, LSaida, ElementoEntrada, Flag, R):-
	I >= 0,
	I < 10,
	((I < MinI); (I > MaxI)) ->
		(nth0(I, LEntrada, ElementoInteresse),
		NovoI is I + 1,
		append(LSaida, [ElementoInteresse], NovoLSaida),
		montaLista(LEntrada, NovoI, MinI, MaxI, NovoLSaida, ElementoEntrada, Flag, R));
		(ElementoInteresse = ElementoEntrada,
		NovoI is I + 1,
		append(LSaida, [ElementoInteresse], NovoLSaida),
		montaLista(LEntrada, NovoI, MinI, MaxI, NovoLSaida, ElementoEntrada, Flag, R)).



verificaSimbolo("#", "X").
% verificaSimbolo("~", "o").
verificaSimbolo(Simbolo, "o"):- Simbolo =\= "#".

verificaDisparoFinal(_, false, false).
verificaDisparoFinal(false, _, false).
verificaDisparoFinal(true, true, true).

verificaJaDisparadoAoBot(Tab, X, Y, R):-
	(X > 0, X < 11, Y > 0, Y < 11) -> (
		nth1(X, Tab, Linha),
		nth1(Y, Linha, ElementoInteresse),
		ehDisparoAoBot(ElementoInteresse, R)
	);
	R = false.

verificaJaDisparadoAoJogador(Tab, X, Y, R):-
	(X > 0, X < 11, Y > 0, Y < 11) -> (
		nth1(X, Tab, Linha),
		nth1(Y, Linha, ElementoInteresse),
		ehDisparoAoJogador(ElementoInteresse, R)
	);
	R = false.

ehDisparoAoBot("X", false).
ehDisparoAoBot("o", false).
ehDisparoAoBot(E, R):- E =\= "X", E =\= "o", R = true.

ehDisparoAoJogador("X", false).
ehDisparoAoJogador("o", false).
ehDisparoAoJogador(E, R):- E =\= "X", E =\= "o", R = true.

montaTabuleiro("", Tab_J):-
	montaMatrizLimpa(_, 0, Tab_J).
montaTabuleiro("B", Tab_B):-
	montaMatrizLimpa(_, 0, Tab),
	montaTabuleiroBotInteiro(Tab, Tab_B).

montaTabuleiroBotInteiro(Tab, Tab_BF):-
	tell('b.txt'),
	montaTabuleiroBot(Tab, 5, Tab_B1),
	write(Tab_B1),
	write('\n'),
	montaTabuleiroBot(Tab_B1, 4, Tab_B2),
	write(Tab_B2),
	write('\n'),
	montaTabuleiroBot(Tab_B2, 3, Tab_B3),
	write(Tab_B3),
	write('\n'),
	montaTabuleiroBot(Tab_B3, 3, Tab_B4),
	write(Tab_B4),
	write('\n'),
	montaTabuleiroBot(Tab_B4, 2, Tab_BF),
	write(Tab_BF),
	write('\n'),
	read(A),
	told.

montaTabuleiroBot(Tab, TamNavio, R):-
	random(1, 11, X),
	random(1, 11, Y),
	random(0, 2, Orient_I),

	write('\n'),
	write('X: '),
	write(X),
	write('\n'),
	write('Y: '),
	write(Y),
	write('\n'),
	write('Orientacao: '),
	write(Orient_I),
	write('\n'),

	montaTabuleiroBotRecurs(Tab, X, Y, TamNavio, Orient_I, R1),
	
	write(R1),
	R1 == true -> (
		verificaEPosicionaNavioBot(Tab, X, Y, TamNavio, Orient_I, R)
	);
	montaTabuleiroBot(Tab, TamNavio, R).

verificaEPosicionaNavioBot(Tab, X, Y, TamNavio, 0, R):-
	posicionaNaviosHorizontal(Tab, X, Y, TamNavio, R).
verificaEPosicionaNavioBot(Tab, X, Y, TamNavio, 1, R):-
	posicionaNaviosVertical(Tab, X, Y, TamNavio, R).


montaTabuleiroBotRecurs(Tab, X, Y, TamNavio, 0, R):-
	K is Y + TamNavio - 1,
	K =< 10 ->
		verificaTemNavioHorizontal(Tab, X, Y, TamNavio, R)
	;
	R = false.

montaTabuleiroBotRecurs(Tab, X, Y, TamNavio, 1, R):-
	K is X + TamNavio - 1,
	K =< 10 ->
		verificaTemNavioVertical(Tab, X, Y, TamNavio, R)
	;
	R = false.

verificaEntradaX(X, R):-
	(X < 1; X > 10) -> (
		write('O valor X eh invalido, insira um valor entre 1 e 10.\n'),
		sleep(2.5),
		R = false, !
	);
	R = true.

verificaEntradaY(Y, R):-
	(Y < 1; Y > 10) -> (
		write('O valor Y eh invalido, insira um valor entre 1 e 10.\n'),
		sleep(2.5),
		R = false, !
	);
	R = true.

verificaEntradaOrient(Orientacao, R):-
	(Orientacao =\= "H", Orientacao =\= "V") -> (
		write('O valor de ORIENT eh invalido, insira o valor H para Horizontal ou V para Vertical.\n'),
		sleep(2.5),
		R = false, !
	);
	R = true.

verificaLimites(Tab, "H", X, Y, TamNavio, R):-
	K is Y + TamNavio - 1,
	(K =< 10) -> (
		verificaTemNavioHorizontal(Tab, X, Y, TamNavio, R), !
	);
	R = false.

verificaLimites(Tab, "V", X, Y, TamNavio, R):-
	K is X + TamNavio - 1,
	(K =< 10) -> (
		verificaTemNavioVertical(Tab, X, Y, TamNavio, R), !
	);
	R = false.
verificaLimites(_, _, _, _, _, false).

posicionaFinal(Tab, X, Y, "H", TamNavio, R):-
	posicionaNaviosHorizontal(Tab, X, Y, TamNavio, R).

posicionaFinal(Tab, X, Y, "V", TamNavio, R):-
	posicionaNaviosVertical(Tab, X, Y, TamNavio, R).

verificaXYOri(false, _, _, false).
verificaXYOri(_, false, _, false).
verificaXYOri(_, _, false, false).
verificaXYOri(true, true, true, true).

verificaXY(false, _, false).
verificaXY(_, false, false).
verificaXY(true, true, true).

rFinal(false, _, false).
rFinal(_, false, false).
rFinal(true, true, true).

verificaEInsereTab(Tab, _, _, _, TamNavio, false, R):-
	write('Entradas invalidas, insira novamente.\n'),
	sleep(3),
	R = Tab.
verificaEInsereTab(Tab, X, Y, Orientacao, TamNavio, true, R):-
	posicionaFinal(Tab, X, Y, Orientacao, TamNavio, R).

posicionaNavios(Tab, TamNavio, R):-
	printaTabEMensagem(Tab, '      Seu tabuleiro\n'),
	write('Insira as posicoes X (de 1 a 10) Y (de 1 a 10) ORIENTACAO (H ou V) para posicionar seu navio.\n'),
	write('Tamanho do navio: '),
	write(TamNavio),
	write('\n'),

	read(X),
	read(Y),
	read_string(user_input, ".", "\n", _, Orientacao),

	write('X: '),
	write(X),
	write('\nY: '),
	write(Y),
	write('\nORIENTACAO: '),
	write(Orientacao),
	write('\n'),

	verificaEntradaX(X, R1),
	verificaEntradaY(Y, R2),
	% TODO VERIFICAR ONDE ESTÁ A DEFINIÇÃO DE R3 (Verificar orientacao ???)

	verificaXYOri(R1, R2, R3, RXYOri),
	verificaLimites(Tab, Orientacao, X, Y, TamNavio, RLimite),
	rFinal(RXYOri, RLimite, RFinal),
	% write('\nRFinal: '),
	% write(RFinal),
	write('\n'),
	RFinal == true -> (
		verificaEInsereTab(Tab, X, Y, Orientacao, TamNavio, RFinal, R)
	);
	write('\nInformacoes invalidas, tente novamente.\n'),
	sleep(3),
	posicionaNavios(Tab, TamNavio, R).

% Implementation by Jan Wielemaker.
take(0, _, []) :- !.
take(N, [H|TA], [H|TB]) :-
  N > 0,
  N2 is N - 1,
  take(N2, TA, TB).
%

% Implementation by Jan Wielemaker.
drop(0,LastElements,LastElements) :- !.
drop(N,[_|Tail],LastElements) :-
  N > 0,
  N1 is N  - 1,
  drop(N1,Tail,LastElements).
%

transpose([[]|_], []).
transpose(Tab, [Linha|Linhas]) :- transpose_col_1(Tab, Linha, TabRest),
                                 transpose(TabRest, Linhas).
transpose_col_1([], [], []).
transpose_col_1([[H|T]|Linhas], [H|Hs], [T|Ts]) :- transpose_col_1(Linhas, Hs, Ts).

% Verifica se há um navio # no Tabuleiro nas posições X e Y até o tamanho do Navio
% Caso não haja, retorna true, caso contrário, retorna false.
verificaTemNavioHorizontal(Tab, X, Y, TamNavio, R):-
    nth1(X, Tab, LinhaTab),
    NumDeDrops is Y - 1,
    drop(NumDeDrops, LinhaTab, LinhaTabDrop),
    take(TamNavio, LinhaTabDrop, LinhaTabDropTake),
    temNavio(LinhaTabDropTake, R1),
    notBool(R1, R).

% Verifica se há um navio # no Tabuleiro nas posições X e Y até o tamanho do Navio
% Caso não haja, retorna true, caso contrário, retorna false.
verificaTemNavioVertical(Tab, X, Y, TamNavio, R):-
    transpose(Tab, TabTransp),
	verificaTemNavioHorizontal(TabTransp, Y, X, TamNavio, R).
    % nth1(Y, TabTransp, LinhaTab),
    % NumDeDrops is X - 1,
    % drop(NumDeDrops, LinhaTab, LinhaTabDrop),
    % take(TamNavio, LinhaTabDrop, LinhaTabDropTake),
    % temNavio(LinhaTabDropTake, R1),
    % notBool(R1, R).

posicionaNaviosHorizontal(Tab, X, Y, TamNavio, R):-
	LinhaInserir is X - 1,
	PosInserir is Y - 1,
	nth0(LinhaInserir, Tab, Linha),
	drop(PosInserir, Linha, LinhaDrop),
	take(TamNavio, LinhaDrop, LinhaDropTake),
	temNavio(LinhaDropTake, TemNavioR),
	notBool(TemNavioR, NotTemNavioR),
	NotTemNavioR == true -> (remontaNaviosHorizontal(Tab, TamNavio, LinhaInserir, PosInserir, R));
	R = [].

posicionaNaviosVertical(Tab, X, Y, TamNavio, R):-
	transpose(Tab, TabTransp),
	posicionaNaviosHorizontal(TabTransp, Y, X, TamNavio, RTransp),
	transpose(RTransp, R).

remontaNaviosHorizontal([HLista|[]], TamNavio, 0, PosInserir, R):- 
	MinI = PosInserir,
	MaxI is PosInserir + TamNavio - 1, 
	montaLista(HLista, 0, MinI, MaxI, [], true, R).
remontaNaviosHorizontal([H|T], TamNavio, 0, PosInserir, R):-
	MinI = PosInserir,
	MaxI is PosInserir + TamNavio - 1, 
	montaLista(H, 0, MinI, MaxI, [], R1),
	remontaNaviosHorizontal(T, TamNavio, -1, PosInserir, R2),
	append([R1], R2, R).
remontaNaviosHorizontal([H|[]], _, _, _, [H]). 
remontaNaviosHorizontal([H|T], TamNavio, LinhaInserir, PosInserir, R):-
	NovaLinhaInserir is LinhaInserir - 1,
	remontaNaviosHorizontal(T, TamNavio, NovaLinhaInserir, PosInserir, R1),
	append([H], R1, R).

montaLista(_, 10, _, _, NovaLista, true, [NovaLista]).
montaLista(LEntrada, I, MinI, MaxI, LSaida, true, R):-
	I >= 0,
	I < 10,
	((I < MinI); (I > MaxI)) ->
		(nth0(I, LEntrada, ElementoInteresse),
		NovoI is I + 1,
		append(LSaida, [ElementoInteresse], NovoLSaida),
		montaLista(LEntrada, NovoI, MinI, MaxI, NovoLSaida, true, R));
		(ElementoInteresse = "#",
		NovoI is I + 1,
		append(LSaida, [ElementoInteresse], NovoLSaida),
		montaLista(LEntrada, NovoI, MinI, MaxI, NovoLSaida, true, R)).

montaLista(_, 10, _, _, NovaLista, NovaLista).
montaLista(LEntrada, I, MinI, MaxI, LSaida, R):-
	I >= 0,
	I < 10,
	((I < MinI); (I > MaxI)) ->
		(nth0(I, LEntrada, ElementoInteresse),
		NovoI is I + 1,
		append(LSaida, [ElementoInteresse], NovoLSaida),
		montaLista(LEntrada, NovoI, MinI, MaxI, NovoLSaida, R));
		(ElementoInteresse = "#",
		NovoI is I + 1,
		append(LSaida, [ElementoInteresse], NovoLSaida),
		montaLista(LEntrada, NovoI, MinI, MaxI, NovoLSaida, R)).

montaMatrizLimpa(R, 10, R).
montaMatrizLimpa(LEntrada, I, R):-
	I >= 0,
	I < 10,
	I1 is I + 1,
	montaListaLimpa(_, 0, R1),
	append(LEntrada, [R1], MFinal),
	montaMatrizLimpa(MFinal, I1, R).


montaListaLimpa(R, 10, R).
montaListaLimpa(K, J, R):-
	J >= 0,
	J < 10,
	J1 is J + 1,
	append(K, ["~"], SaidaL),
	montaListaLimpa(SaidaL, J1, R).

% Retorna true caso haja uma célula #, caso contrário, retorna false.
temNavio([], false).
temNavio(["#"|_], true).
temNavio([#|_], true).
temNavio([H|T], R):- H =\= "#", H \= #, temNavio(T, R).

% Implementação egoísta de not
notBool(false, true).
notBool(true, false).