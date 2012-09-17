
module Tipos (
	-- Carta
	Mes (
		Janeiro,
		Fevereiro,
		Marco,
		Abril,
		Maio,
		Junho,
		Julho,
		Agosto,
		Setembro,
		Outubro,
		Novembro,
		Dezembro
	),
	TipoFaixa (
		FaixaVermelha,
		FaixaRoxa,
		FaixaEscrita
	),
	TipoBrilhante (
		Geral,
		Ameman
	),
	TipoAnimal (
		AnimalGeral,
		InoShikaCho
	),
	Tipo (
		Plain,
		Faixa,
		Animal,
		Brilhante,
		Sake
	),
	Carta,
	cartaMes,
	cartaTipo,
	cartaVisibilidade,
	cartaGetSelecionada,
	cartaSetSelecionada,
	cartaSetVisibilidade,
	Deck,
	Jogador,
	jogadorCartasJogaveis,
	jogadorCartasJogadas,
	jogadorPanel,
	jogadorPontuacao,
	jogadorSetCartasJogaveis,
	jogadorSetCartasJogadas,
	jogadorMesaJogador,
	jogadorSetPontuacao,
	Mesa,
	mesaPanel,
	mesaCartaMesa,
	mesaPilha,
	mesaPontuacao,
	mesaCartaDoMes,
	mesaSetCartaMesa,
	mesaSetPilha,
	mesaSetCartaDoMes,
	mesaSetPontuacao,
	Estado,
	estadoVarCPU,
	estadoVarJogador,
	estadoVarMesa,
	estadoVarQuemComecou,
	estadoVarProximoDaVez,
	estadoJanela,
	estadoCPU,
	estadoJogador,
	estadoMesa,
	estadoQuemComecou,
	estadoProximoDaVez,
	Pontuacao,
	MesaJogador,
	CartasPlain,
	CartasFaixa,
	CartasAnimal,
	CartasBright,
	CartaMes
	) where

import Graphics.UI.WX
import Graphics.UI.WXCore

-- Carta
data Mes = Janeiro | Fevereiro | Marco | Abril | Maio | Junho | Julho | Agosto | Setembro | Outubro | Novembro | Dezembro deriving(Eq,Ord,Show)

data TipoFaixa = FaixaVermelha | FaixaRoxa | FaixaEscrita deriving(Eq,Ord,Show)

data TipoBrilhante = Geral | Ameman deriving(Eq,Ord,Show)

data TipoAnimal = AnimalGeral | InoShikaCho deriving(Eq,Ord,Show)

data Tipo = Plain | Faixa TipoFaixa | Animal TipoAnimal | Brilhante TipoBrilhante | Sake deriving(Eq,Ord,Show)

type CartaMes = Mes

type Carta = (
	Mes,  -- mes da carta
	Tipo,  -- tipo da carta
	String, -- arquivo da imagem
	Bool,  -- visibilidade
	Bool  -- selecionada
	)
	
cartaMes :: Carta -> Mes
cartaMes (a,b,c,d,e) = a

cartaTipo :: Carta -> Tipo
cartaTipo (a,b,c,d,e) = b

cartaVisibilidade :: Carta -> Bool
cartaVisibilidade (a,b,c,d,e) = d
	
cartaGetSelecionada :: Carta -> Bool
cartaGetSelecionada (a,b,c,d,e) = e
	
cartaSetSelecionada :: Carta -> Bool -> Carta
cartaSetSelecionada (a,b,c,d,e) valor = (a,b,c,d,valor)

cartaSetVisibilidade :: Carta -> Bool -> Carta
cartaSetVisibilidade (a,b,c,d,e) valor = (a,b,c,valor,e)

type Deck = [Carta]

type CartasPlain = [Carta]

type CartasFaixa = [Carta]

type CartasAnimal = [Carta]

type CartasBright = [Carta]

type MesaJogador = (
	CartasPlain, 
	CartasFaixa, 
	CartasAnimal, 
	CartasBright
	)
	

type Pontuacao = (
	String, 
	[Carta], 
	Int
	)

type Jogador = (
	Deck, -- cartas jogaveis
	Deck, -- cartas jogadas 
	Panel(), -- panel total
	Int -- ultima pontuacao
	)
	
jogadorCartasJogaveis :: Jogador -> Deck
jogadorCartasJogaveis (d1,d2,p,pontos) = d1

jogadorCartasJogadas :: Jogador -> Deck
jogadorCartasJogadas (d1,d2,p,pontos) = d2

jogadorPanel :: Jogador -> Panel()
jogadorPanel (d1,d2,p,pontos) = p

jogadorPontuacao :: Jogador -> Int
jogadorPontuacao (d1,d2,p,pontos) = pontos

jogadorSetCartasJogaveis :: Jogador -> Deck -> Jogador
jogadorSetCartasJogaveis (a,b,c,pontos) deck = (deck,b,c,pontos)

jogadorSetCartasJogadas :: Jogador -> Deck -> Jogador
jogadorSetCartasJogadas (a,b,c,pontos) deck = (a,deck,c,pontos)

jogadorSetPontuacao :: Jogador -> Int -> Jogador
jogadorSetPontuacao (d1,d2,p,pontos) newPontuacao = (d1,d2,p,newPontuacao)

jogadorMesaJogador :: Jogador -> MesaJogador
jogadorMesaJogador (d1,d2,p,pontos) =
	foldr distribuir ([],[],[],[]) d2
	where
		distribuir carta (plains, faixas, animais, brights)
			| cartaTipo carta == Plain = (plains ++ [carta], faixas, animais, brights)
			| cartaTipo carta == Faixa FaixaVermelha || cartaTipo carta == Faixa FaixaRoxa || cartaTipo carta == Faixa FaixaEscrita = (plains, faixas ++ [carta], animais, brights)
			| cartaTipo carta == Animal AnimalGeral || cartaTipo carta == Animal InoShikaCho || cartaTipo carta == Sake = (plains, faixas, animais ++ [carta], brights)
			| cartaTipo carta == Brilhante Geral || cartaTipo carta == Brilhante Ameman = (plains, faixas, animais, brights ++ [carta]) 

type Mesa = (
	Panel(), -- panel da mesa
	Deck, -- pilha de cartas
	Deck, -- cartas da mesa
	Carta, -- carta do mes
	Int -- Pontuacao
	)
	
mesaPanel :: Mesa -> Panel()
mesaPanel (p,pilha,mesa,mes,pontuacao) = p

mesaCartaMesa :: Mesa -> Deck
mesaCartaMesa (p,pilha,mesa,mes,pontuacao) = mesa

mesaPilha :: Mesa -> Deck
mesaPilha (p,pilha,mesa,mes,pontuacao) = pilha

mesaPontuacao :: Mesa -> Int
mesaPontuacao (p,pilha,mesa,mes,pontuacao) = pontuacao

mesaCartaDoMes :: Mesa -> Carta
mesaCartaDoMes (p,pilha,mesa,mes,pontuacao) = mes

mesaSetCartaMesa :: Mesa -> Deck -> Mesa
mesaSetCartaMesa (p,pilha,mesa,mes,pontuacao) newMesa = (p,pilha,newMesa,mes,pontuacao)

mesaSetPilha :: Mesa -> Deck -> Mesa
mesaSetPilha (p,pilha,mesa,mes,pontuacao) newPilha = (p,newPilha,mesa,mes,pontuacao)

mesaSetCartaDoMes :: Mesa -> Carta -> Mesa
mesaSetCartaDoMes (p,pilha,mesa,mes,pontuacao) newMes = (p,pilha,mesa,newMes,pontuacao)

mesaSetPontuacao :: Mesa -> Int -> Mesa
mesaSetPontuacao (p,pilha,mesa,mes,pontuacao) newPontuacao = (p,pilha,mesa,mes,newPontuacao)

-- Estado do jogo
type Estado = (
	-- Elementos graficos
	Frame(), -- janela principal
	-- Elementos do jogo
	Var Jogador, -- jogador A
	Var Jogador, -- jogador B
	Var Mesa, -- mesa do jogo
	Var Int, -- quem comecou
	Var Int -- proximo da vez
	)

estadoVarCPU :: Estado -> Var Jogador
estadoVarCPU (janela,cpu,jogador,mesa,a,b) = cpu

estadoVarJogador :: Estado -> Var Jogador
estadoVarJogador (janela,cpu,jogador,mesa,a,b) = jogador

estadoVarMesa :: Estado -> Var Mesa
estadoVarMesa (janela,cpu,jogador,mesa,a,b) = mesa

estadoJanela :: Estado -> IO (Frame())
estadoJanela (janela,cpu,jogador,mesa,a,b) = do return janela

estadoVarQuemComecou :: Estado -> Var Int
estadoVarQuemComecou (janela,cpu,jogador,mesa,a,b) = a

estadoQuemComecou :: Estado -> IO Int
estadoQuemComecou e = get (estadoVarQuemComecou e) value

estadoVarProximoDaVez :: Estado -> Var Int
estadoVarProximoDaVez (janela,cpu,jogador,mesa,a,b) = b

estadoProximoDaVez :: Estado -> IO Int
estadoProximoDaVez e = get (estadoVarProximoDaVez e) value

estadoCPU :: Estado -> IO Jogador
estadoCPU e = get (estadoVarCPU e) value

estadoJogador :: Estado -> IO Jogador
estadoJogador e = get (estadoVarJogador e) value

estadoMesa :: Estado -> IO Mesa
estadoMesa e = get (estadoVarMesa e) value

