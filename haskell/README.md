<h1 align="center"> Batalha Naval </h1>
<p align="center">
  <b>O objetivo deste projeto foi desenvolver um jogo de batalha naval feito em Haskell, para a disciplina de Paradigmas de Linguagem de Programação (PLP) do Curso de Ciência da Computação da UFCG.</b>
  
  <h3>Guia de Instalação</h3>
  <ol>
    <li>git clone https://github.com/pedrohos/plp-BatalhaNaval.git</li>
    <li>cd plp-BatalhaNaval/haskell/</li>
    <li>cabal install random</li>
    <li>cabal install ansi-terminal</li>
  </ol>
  
  <h3>Como compilar para jogar</h3>
  <ol>
    <li> gch main</li>
    <li> ./main (Linux)</li>
    <li> .\main.exe (Windows)</li>
  </ol>
  
  <h3>
    Sobre o jogo
  </h3>
    No início da partida o jogador poderá posicionar os navios e a cada rodada poderá realizar um disparo ao inimigo. Após o disparo, será reportado ao jogador se a célula atingida foi na água, um navio ou se a posição já foi atingida anteriormente. Na vez do bot, o mesmo ocorrerá. Assim o jogo dará sequência até que todos os navios de um dos participantes sejam eliminados primeiro.
O jogador, em seu turno, poderá salvar o jogo, persistindo todas as informações da partida para dar continuidade posteriormente.

No início do jogo o jogador poderá escolher carregar uma partida salva anteriormente ou iniciar um novo jogo.

  <h3>
    Símbolos:
  
    "~" -> Água 
    "#" -> Navio 
    "o" -> Água sem navio 
    "X" -> Navio atingido 
  </h3>
</p>
