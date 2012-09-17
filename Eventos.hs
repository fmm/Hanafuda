
module Eventos (
	iniciar
	) where 
	
import Control.Monad

import System.Random
import System.Posix
	
import Graphics.UI.WX
import Graphics.UI.WXCore

import Tipos
import GUI
import Regras
import Util
import Cartas
import IA

--iniciar Jogo
iniciar :: Estado -> IO()
iniciar estado = do
	mesa <- estadoMesa estado
	aleatorio <- (randomIO :: IO Int)
	let newMesa = (mesaPanel mesa,[],[],baralho !! (aleatorio `mod` 48),10) -- TODO
	set (estadoVarMesa estado) [value := newMesa]
	set (estadoVarQuemComecou estado) [value := 0]
	set (estadoVarProximoDaVez estado) [value := 0]
	loop estado
	comecaRodada estado
	return ()

comecaRodada :: Estado -> IO()
comecaRodada estado = do
	loop estado
	janela <- estadoJanela estado
	mesa <- estadoMesa estado
	cpu <- estadoCPU estado
	jogador <- estadoJogador estado
	let newCPU = ([],[],jogadorPanel cpu,0)
	let newJogador = ([],[],jogadorPanel jogador,0)
	aleatorio <- (randomIO :: IO Int)
	let pontos = (mesaPontuacao mesa)
	let newMesa = (mesaPanel mesa,[],[],baralho !! (aleatorio `mod` 48),pontos)
	set (estadoVarCPU estado) [value := newCPU]
	set (estadoVarJogador estado) [value := newJogador]
	set (estadoVarMesa estado) [value := newMesa]
	if(pontos <= 0) then do
		dialogoPerdedor janela
		else do
			if(pontos >= 50) then do
				dialogoVencedor janela
				else do
					-- jogar ate acabar
					embaralhar estado
					return()
	return()
	
--embaralhar cartas e colocar na pilha
embaralhar :: Estado -> IO()
embaralhar estado = do
	mesa <- estadoMesa estado
	randomico <- listaRandomica
	--TODO:debug
	--putStrLn $ show randomico
	--randomico <- return [28,7,32,27,29,19,12,3,2,5,39,35,33,31,17,18,36,1,43,10,38,26,4,0,8,37,47,46,34,30,41,22,6,15,40,23,24,44,25,9,16,11,45,14,13,20,21,42]
		
	let deck = [ baralho !! (randomico !! i) | i <- [0..(length randomico - 1)] ]
	let newMesa = mesaSetPilha mesa deck
	set (estadoVarMesa estado) [value := newMesa]
	distribuir estado
	return ()

distribuir :: Estado -> IO()
distribuir estado = do
	-- 8 para um
	mesa <- estadoMesa estado
	jogador <- estadoJogador estado
	cpu <- estadoCPU estado
	pilha <- return $ mesaPilha mesa
	let deckJogador = take 8 pilha
	pilha <- return $ drop 8 pilha
	let deckCPU = take 8 pilha
	pilha <- return $ drop 8 pilha
	let deckMesa = take 8 pilha
	pilha <- return $ drop 8 pilha
	let newMesa = mesaSetPilha mesa pilha
	let newJogador = jogadorSetCartasJogaveis jogador deckJogador
	let newCPU = jogadorSetCartasJogaveis cpu (map (\x -> cartaSetVisibilidade x False) deckCPU)
	let newMesa = mesaSetCartaMesa (mesaSetPilha mesa (map (\x -> cartaSetVisibilidade x False) pilha)) deckMesa
	set (estadoVarJogador estado) [value := newJogador]
	set (estadoVarCPU estado) [value := newCPU]
	set (estadoVarMesa estado) [value := newMesa]
	loop estado
	agora <- estadoProximoDaVez estado
	set (estadoVarQuemComecou estado) [value := agora]
	jogar estado
	return()

finalizarRodada :: Estado -> IO()
finalizarRodada estado = do
	janela <- estadoJanela estado
	jogador <- estadoJogador estado
	cpu <- estadoCPU estado
	quemComecou <- estadoQuemComecou estado
	mesa <- estadoMesa estado
	let cartaDoMes = (cartaMes $ mesaCartaDoMes mesa)
	let pontuacaoJogador = (jogadorPontuacao jogador)
	let pontuacaoCPU = (jogadorPontuacao cpu)
	if(pontuacaoJogador /= 0 || pontuacaoCPU /= 0) then do
		--alguem fez alguma pontuacao, sem janela mesmo
		if(pontuacaoJogador /= 0) then do
			dialogoJogadorPontos janela "Voce fez " pontuacaoJogador
			else do
				dialogoJogadorPontos janela "Eu fiz " pontuacaoCPU
		return() -- =D
		else do
		-- dealer's privilege
			janela <- estadoJanela estado
			dialogoDealer janela
			let newCPU = jogadorSetPontuacao cpu ((\x -> if(x == 0) then 0 else 6) quemComecou)
			set (estadoVarCPU estado) [value := newCPU]
			let newJogador = jogadorSetPontuacao jogador ((\x -> if(x == 1) then 0 else 6) quemComecou)
			set (estadoVarJogador estado) [value := newJogador]
	jogador <- estadoJogador estado
	cpu <- estadoCPU estado
	pontuacao <- return $ mesaPontuacao mesa
	pontuacao <- return $ pontuacao + (jogadorPontuacao jogador)
	pontuacao <- return $ pontuacao - (jogadorPontuacao cpu)
	let newMesa = mesaSetPontuacao mesa pontuacao
	set (estadoVarMesa estado) [value := newMesa]
	if(jogadorPontuacao jogador /= 0) then do set (estadoVarProximoDaVez estado) [value := 0] else do return()
	if(jogadorPontuacao cpu /= 0) then do set (estadoVarProximoDaVez estado) [value := 1] else do return()
	comecaRodada estado
	return()

-- loop
jogar :: Estado -> IO()
jogar estado = do
	loop estado
	mesa <- estadoMesa estado
	jogador <- estadoJogador estado
	cpu <- estadoCPU estado
	vez <- estadoProximoDaVez estado	
	quemVaiJogar <- return $ (\x -> if(x == 0) then jogador else cpu) vez 
	podeJogar <- return $ length (jogadorCartasJogaveis quemVaiJogar) > 0
	if(not podeJogar) then do
		finalizarRodada estado
		else do
			let nvez = 1 - vez
			set (estadoVarProximoDaVez estado) [value := nvez]
			if(vez == 0) then do
				-- jogador
				prepararJogador estado
				else do
					-- cpu
					prepararIA estado
			return()
	return ()

--TODO,FIXME,fmm,AKI!!!!!!
-- para IA
prepararIA :: Estado -> IO()
prepararIA estado = do
	loop estado
	janela <- estadoJanela estado	
	cpu <- estadoCPU estado
	jogador <- estadoJogador estado
	mesa <- estadoMesa estado
	-- informar
	let informar = sequence [do loop estado, do infoDialog janela "Rival" "Hmm... essa vai ser minha jogada certo?!"]
	-- escolher carta 1
	
	-- NivelDificuldade -> CartaMes -> [Carta] -> Deck -> MesaJogador -> MesaJogador -> [Carta] -- retorno : [cartaEscolhida, cartaMesa <se existente>]
	
	deck <- return $ jogadorCartasJogaveis cpu
	
	
	--carta1 <- return $ head deck -- TODO: colocar randomico?
	ret <- return $ escolhaIA 3 (cartaMes (mesaCartaDoMes mesa)) (jogadorCartasJogaveis cpu) (mesaCartaMesa mesa) (jogadorMesaJogador cpu) (jogadorMesaJogador jogador)
	
	--putStrLn $ show ret
	
	carta1 <- return $ ret !! 0
	
	newDeckCartasJogaveis <- return $ filter (/= carta1) deck
	let f x = cartaSetSelecionada (cartaSetVisibilidade x True) True
	newDeck <- return $ [(\x -> if(x == carta1) then f x else x) y | y <- deck]
	cpu <- return $ jogadorSetCartasJogaveis cpu newDeck
	set (estadoVarCPU estado) [value := cpu]
	
	
	-- escolhe pareamento
	cartasMesa <- return $ mesaCartaMesa mesa
	let mesmoMes = \x y -> (cartaMes x) == (cartaMes y)
	cartasPossiveis <- return $ filter (\x -> mesmoMes x carta1) cartasMesa
	if(length cartasPossiveis == 0) then do
		--TODO
		--putStrLn "1"
		informar
		-- vai pra mesa
		newCartasMesa <- return $ cartasMesa ++ [cartaSetVisibilidade carta1 True]
		mesa <- return $ (mesaSetCartaMesa mesa newCartasMesa)
		set (estadoVarMesa estado) [value := mesa]
		-- tirar carta do jogador
		cpu <- return $ jogadorSetCartasJogaveis cpu newDeckCartasJogaveis
		set (estadoVarCPU estado) [value := cpu]
		else do
			-- pareamento
			--TODO
			--putStrLn "2"
			--carta2 <- return $ head cartasPossiveis
			carta2 <- return $ ret !! 1
			cartaMesaIluminada <- return $ [ if(x == carta2) then (cartaSetSelecionada carta2 True) else x | x <- cartasMesa]
			mesa <- return $ mesaSetCartaMesa mesa cartaMesaIluminada
			set (estadoVarMesa estado) [value := mesa]
			informar
			-- realmente jogar
			newCartasMesa <- return $ filter (/= carta2) cartasMesa
			mesa <- return $ mesaSetCartaMesa mesa newCartasMesa
			set (estadoVarMesa estado) [value := mesa]
			-- colocar em jogador
			cpu <- estadoCPU estado
			cpu <- return $ jogadorSetCartasJogaveis cpu newDeckCartasJogaveis
			newDeckCartasJogagadas <- return $ (jogadorCartasJogadas cpu) ++ [cartaSetVisibilidade carta1 True,carta2]
			cpu <- return $ jogadorSetCartasJogadas cpu newDeckCartasJogagadas
			set (estadoVarCPU estado) [value := cpu]
			return()
	loop estado
	--pegar carta da pilha
	-- selecionar carta da pilha
	mesa <- estadoMesa estado
	pilha <- return $ mesaPilha mesa
	carta1 <- return $ last pilha
	newPilha <- return $ filter (/=carta1) pilha ++ [cartaSetVisibilidade (cartaSetSelecionada carta1 True) True]
	mesa <- return $ mesaSetPilha mesa newPilha
	set (estadoVarMesa estado) [value := mesa]
	-- escolher carta da mesa
	cartasMesa <- return $ mesaCartaMesa mesa
	cartasPossiveis <- return $ filter (\x -> mesmoMes x carta1) cartasMesa
	if(length cartasPossiveis == 0) then do
		--TODO
		--putStrLn "3"
		informar
		-- nao tem pra parear, vai pra mesa
		newPilha <- return $ filter (/=carta1) pilha
		mesa <- return $ mesaSetPilha mesa newPilha
		newCartasMesa <- return $ (mesaCartaMesa mesa) ++ [cartaSetVisibilidade carta1 True]
		mesa <- return $ mesaSetCartaMesa mesa newCartasMesa
		set (estadoVarMesa estado) [value := mesa]
		else do
			--TODO
			--putStrLn "4"
			-- selecionar carta
			ret <- return $ escolhaIA 3 (cartaMes (mesaCartaDoMes mesa)) [carta1] (mesaCartaMesa mesa) (jogadorMesaJogador cpu) (jogadorMesaJogador jogador)
			
			--carta2 <- return $ head cartasPossiveis
			carta2 <- return $ ret !! 1
			
			cartaMesaIluminada <- return $ [if(x == carta2) then (cartaSetSelecionada x True) else x | x <- cartasMesa]
			mesa <- return $ mesaSetCartaMesa mesa cartaMesaIluminada
			set (estadoVarMesa estado) [value := mesa]
			informar
			-- tirar da pilha
		 	newPilha <- return $ filter (/=carta1) pilha
			mesa <- return $ mesaSetPilha mesa newPilha
			-- tira da mesa
			newCartasMesa <- return $ filter (/=carta2) cartasMesa
			mesa <- return $ mesaSetCartaMesa mesa newCartasMesa
			set (estadoVarMesa estado) [value := mesa]
			-- coloca em cpu
			cpu <- estadoCPU estado
			newDeckCartasJogadas <- return $ (jogadorCartasJogadas cpu) ++ [cartaSetVisibilidade carta1 True,carta2]
			cpu <- return $ jogadorSetCartasJogadas cpu newDeckCartasJogadas
			set (estadoVarCPU estado) [value := cpu]
	-- TODO: verificar pontuacao e decidir
	-- TODO: passa pro jogador / finalizar rodada
	--}
	loop estado
	verificarRodadaIA estado
	return()
	
verificarRodadaIA :: Estado -> IO()
verificarRodadaIA estado = do
	return()
	-- veriricar e atualizar pontuacao da cpu
	cpu <- estadoCPU estado
	let mesaJogador = (jogadorMesaJogador cpu)
	mesa <- estadoMesa estado
	let cartaDoMes = (cartaMes $ mesaCartaDoMes mesa)
	let listaPontuacao = (calcularPontuacao mesaJogador cartaDoMes)
	if(length listaPontuacao > 0) then do
		janela <- estadoJanela estado
		let nomes = [ x | (x,y,z) <- listaPontuacao ]
		let lista = [ y | (x,y,z) <- listaPontuacao ]
		let pontuacao = [ z | (x,y,z) <- listaPontuacao ]
		let totalPontos = sum pontuacao
		let oldPontos = (jogadorPontuacao cpu)
		cpu <- return $ jogadorSetPontuacao cpu totalPontos
		set (estadoVarCPU estado) [value := cpu]
		if(totalPontos > oldPontos) then do
			continuarJogo <- dialogoPontucaoIA janela nomes lista pontuacao
			if(continuarJogo) then do
				-- chamar jogador
				jogar estado
				else do
					jogador <- estadoJogador estado
					let newJogador = jogadorSetPontuacao jogador 0
					set (estadoVarJogador estado) [value := newJogador]
					finalizarRodada estado
			else do
				jogar estado
		else do
			jogar estado

-- para jogador
prepararJogador :: Estado -> IO ()
prepararJogador estado = do
	colocarEventoEscolherCartaJogavel estado
	colocarEventoEscolherCartaMesa estado
	return ()

colocarEventoEscolherCartaJogavel :: Estado -> IO ()
colocarEventoEscolherCartaJogavel estado = do
	jogador <- estadoJogador estado
	let p = jogadorPanel jogador
	set p [on click := clicarPanelJogador estado]
	return ()

atualizaJogador :: Jogador -> Point -> IO Jogador
atualizaJogador jogador (Point x y) = do
	let deck = jogadorCartasJogaveis jogador
	let f c = (cartaSetSelecionada c (not (cartaGetSelecionada c)))
	let g c = (cartaSetSelecionada c False)
	let newDeck = [let carta = deck !! i in if (i == (x `div` comprimentoCarta)) then f carta else g carta | i <- [0..(length deck-1)]]
	return $ jogadorSetCartasJogaveis jogador newDeck

-- evento do jogador ao clicar no panel das cartas jogaveis
clicarPanelJogador :: Estado -> Point -> IO()
clicarPanelJogador estado ponto = do
	-- atualiza jogador
	jogador <- estadoJogador estado
	newJogador <- atualizaJogador jogador ponto
	set (estadoVarJogador estado) [value := newJogador]
	-- atualizar mesa
	mesa <- estadoMesa estado
	let deckJogador = filter (\x -> cartaGetSelecionada x) (jogadorCartasJogaveis newJogador)
	newMesa <- selecionaCartaMesa mesa deckJogador
	set (estadoVarMesa estado) [value := newMesa]
	-- repintar
	loop estado
	return ()
	
-- para mesa
selecionaCartaMesa :: Mesa -> [Carta] -> IO Mesa
selecionaCartaMesa mesa deck = do
	let carta = head deck
	let mesmoMes = (\x -> or [(cartaMes x) == (cartaMes carta) | carta <- deck])
	let deck = mesaCartaMesa mesa
	let newDeck = [let carta = deck !! i in if(mesmoMes carta) then cartaSetSelecionada carta True else cartaSetSelecionada carta False | i <- [0..(length deck-1)]]
	let newMesa = mesaSetCartaMesa mesa newDeck
	return newMesa

--evento da mesa para completar carta do jogador
colocarEventoEscolherCartaMesa :: Estado -> IO ()
colocarEventoEscolherCartaMesa estado = do
	mesa <- estadoMesa estado
	let p = mesaPanel mesa
	set p [on click := clicarPanelMesa estado]
	return ()

--por clicar na mesa... novo jogador,mesa, finished?
atualizaMesa :: Mesa -> Jogador -> Point -> IO (Jogador,Mesa,Bool)
atualizaMesa mesa jogador (Point x y) = do
	let deck = mesaCartaMesa mesa
	let p = mesaPanel mesa
	let tamanhoCarta = length deck
	let espaco = 10
	let f k = (k `div` 2)
	let g k = if(k `mod` 2 == 0) then (pt (150 + (comprimentoCarta+espaco)*(f k)) 20) else (pt (150 + (comprimentoCarta+espaco)*(f k)) (20+espaco+larguraCarta))
	let colide (Point x1 y1) = let (x2,y2) = (x1 + comprimentoCarta, y1 + larguraCarta) in (x1 < x && x < x2) && (y1 < y && y < y2)
	let mesmoMes = (\x y -> (cartaMes x) == (cartaMes y))
	let cartasJogadorSelecionada = filter (\x -> cartaGetSelecionada x) (jogadorCartasJogaveis jogador)
	let temJogador carta = let len = length $ filter (\x -> (cartaGetSelecionada x) && (mesmoMes x carta)) (jogadorCartasJogaveis jogador) in len > 0
	---------------------------------------------------
	let listaCartasSelecionadas = filter (\x -> (cartaGetSelecionada x)) deck -- existe alguma carta selecionada na mesa?
	---------------------------------------------------
	let toTrue c = (cartaSetSelecionada c True)
	let toFalse c = (cartaSetSelecionada c False)
	let update carta ponto = if(colide ponto && temJogador carta) then toTrue carta else toFalse carta
	let clicadaValida = [deck !! i | i <- [0..(tamanhoCarta-1)], colide (g i) && temJogador (deck !! i)]
	---------------------------------------------------
	if(length cartasJogadorSelecionada > 0) then do
		if(length clicadaValida > 0) then do
				-- apaga cartas do jogado,mesa para cartas jogadas
				-- limpar mesa
				let carta1 = head cartasJogadorSelecionada
				let carta2 = head clicadaValida
				let newDeckJogadorCartasJogaveis = filter (\x -> x /= carta1) (jogadorCartasJogaveis jogador)
				let newDeckJogadorCartasJogadas = (jogadorCartasJogadas jogador) ++ [(cartaSetSelecionada carta1 False),(cartaSetSelecionada carta2 False)]
				let newJogador1 = (jogadorSetCartasJogaveis jogador newDeckJogadorCartasJogaveis)
				let newJogador2 = (jogadorSetCartasJogadas newJogador1 newDeckJogadorCartasJogadas)
				-- novaMesa
				let newCartasDaMesa = filter (\x -> x /= carta2) (mesaCartaMesa mesa)
				let newMesa1 = (mesaSetCartaMesa mesa newCartasDaMesa)
				newMesa2 <- selecionaCartaMesa newMesa1 []
				return (newJogador2,newMesa2,True)
				else do 
					if(length listaCartasSelecionadas > 0) then do 
						-- fez nada
						let newMesa = mesa
						let newJogador = jogador
						return (newJogador,newMesa,False)
						else do 
							-- carta sem par possivel, vai para mesa
							let carta1 = head cartasJogadorSelecionada
							let newDeckJogadorCartasJogaveis = filter (\x -> x /= carta1) (jogadorCartasJogaveis jogador)
							let newDeckJogadorCartasJogadas = (jogadorCartasJogadas jogador)
							let newJogador1 = (jogadorSetCartasJogaveis jogador newDeckJogadorCartasJogaveis)
							let newJogador2 = (jogadorSetCartasJogadas newJogador1 newDeckJogadorCartasJogadas)
							-- novaMesa
							let newCartasDaMesa = (mesaCartaMesa mesa) ++ [(cartaSetSelecionada carta1 False)]
							let newMesa1 = (mesaSetCartaMesa mesa newCartasDaMesa)
							newMesa2 <- selecionaCartaMesa newMesa1 []
							return (newJogador2,newMesa2,True)
		else do
			--nada pra rolar
			let newMesa = mesa
			let newJogador = jogador
			return (newJogador,newMesa,False)

--clicou na mesa
clicarPanelMesa :: Estado -> Point -> IO()
clicarPanelMesa estado ponto = do
	-- variaveis
	cpu <- estadoCPU estado
	jogador <- estadoJogador estado
	mesa <- estadoMesa estado
	janela <- estadoJanela estado
	-- atualiza mesa
	(newJogador,newMesa,acabou) <- atualizaMesa mesa jogador ponto
	set (estadoVarJogador estado) [value := newJogador]
	set (estadoVarMesa estado) [value := newMesa]
	-- repintar
	loop estado
	-- verificar rodada, acabou
	if(acabou) then do
		-- tira evento do panel de cartas jogaveis
		let p = jogadorPanel newJogador
		set p [on click := (\_ -> do return ())]
		mesa <- estadoMesa estado
		let pilha = mesaPilha mesa
		-- verificar se pode jogar com a pilha
		if(length pilha > 0) then do
			colocarEventoEscolherCartaPilha estado
			return()
			else do
			--jogador terminou de jogar
			verificarRodadaJogador estado
			return()
		else do
			return()
	return()

colocarEventoEscolherCartaPilha :: Estado -> IO ()
colocarEventoEscolherCartaPilha estado = do
	-- atualiza mesa 1
	mesa <- estadoMesa estado
	let pilha = mesaPilha mesa
	let carta = last pilha
	let newPilha =  filter (/=carta) pilha ++ [cartaSetVisibilidade (cartaSetSelecionada carta True) True]
	newMesa <- selecionaCartaMesa (mesaSetPilha mesa newPilha) [cartaSetVisibilidade (cartaSetSelecionada carta True) True]
	set (estadoVarMesa estado) [value := newMesa]
	loop estado
	colocarEventoEscolherCartaMesa2 estado
	return()

-- colocar evento da carta da pilha
colocarEventoEscolherCartaMesa2 :: Estado -> IO ()
colocarEventoEscolherCartaMesa2 estado = do
	mesa <- estadoMesa estado
	let p = mesaPanel mesa
	set p [on click := clicarPanelMesa2 estado]
	return ()
	
--clicou na mesa
clicarPanelMesa2 :: Estado -> Point -> IO()
clicarPanelMesa2 estado ponto = do
	-- variaveis
	cpu <- estadoCPU estado
	jogador <- estadoJogador estado
	mesa <- estadoMesa estado
	janela <- estadoJanela estado
	-- atualiza mesa
	(newJogador,newMesa,acabou) <- atualizaMesa2 mesa jogador ponto
	set (estadoVarJogador estado) [value := newJogador]
	set (estadoVarMesa estado) [value := newMesa]
	-- verificar rodada, acabou
	if(acabou) then do
		-- repintar
		loop estado
		-- tira evento
		let p = mesaPanel mesa
		set p [on click := (\_ -> do return ())]
		return ()
		-- verificar/decidir fim da rodada
		verificarRodadaJogador estado
		else do
			-- espera acabar =)
			return()
	return()
	
--por clicar na mesa... novo jogador,mesa, finished?
atualizaMesa2 :: Mesa -> Jogador -> Point -> IO (Jogador,Mesa,Bool)
atualizaMesa2 mesa jogador (Point x y) = do
	let deck = mesaCartaMesa mesa
	let pilha = mesaPilha mesa
	let p = mesaPanel mesa
	let tamanhoCarta = length deck
	let espaco = 10
	let f k = (k `div` 2)
	let g k = if(k `mod` 2 == 0) then (pt (150 + (comprimentoCarta+espaco)*(f k)) 20) else (pt (150 + (comprimentoCarta+espaco)*(f k)) (20+espaco+larguraCarta))
	let colide (Point x1 y1) = let (x2,y2) = (x1 + comprimentoCarta, y1 + larguraCarta) in (x1 < x && x < x2) && (y1 < y && y < y2)
	let mesmoMes = (\x y -> (cartaMes x) == (cartaMes y))
	let cartasJogadorSelecionada = filter (\x -> cartaGetSelecionada x) (pilha)
	let temJogador carta = let len = length $ filter (\x -> (cartaGetSelecionada x) && (mesmoMes x carta)) (pilha) in len > 0
	---------------------------------------------------
	let listaCartasSelecionadas = filter (\x -> (cartaGetSelecionada x)) deck -- existe alguma carta selecionada na mesa?
	---------------------------------------------------
	let toTrue c = (cartaSetSelecionada c True)
	let toFalse c = (cartaSetSelecionada c False)
	let update carta ponto = if(colide ponto && temJogador carta) then toTrue carta else toFalse carta
	let clicadaValida = [deck !! i | i <- [0..(tamanhoCarta-1)], colide (g i) && temJogador (deck !! i)]
	---------------------------------------------------
	if(length cartasJogadorSelecionada > 0) then do
		if(length clicadaValida > 0) then do
				-- apaga cartas do jogado,mesa para cartas jogadas
				-- limpar mesa
				let carta1 = head cartasJogadorSelecionada
				let carta2 = head clicadaValida
				let newDeckJogadorCartasJogaveis = filter (\x -> x /= carta1) (jogadorCartasJogaveis jogador)
				let newDeckJogadorCartasJogadas = (jogadorCartasJogadas jogador) ++ [(cartaSetSelecionada carta1 False),(cartaSetSelecionada carta2 False)]
				let newJogador1 = (jogadorSetCartasJogaveis jogador newDeckJogadorCartasJogaveis)
				let newJogador2 = (jogadorSetCartasJogadas newJogador1 newDeckJogadorCartasJogadas)
				-- novaMesa
				let newCartasDaMesa = filter (\x -> x /= carta2) (mesaCartaMesa mesa)
				let newMesa1 = (mesaSetCartaMesa mesa newCartasDaMesa)
				newMesa2 <- selecionaCartaMesa newMesa1 []
				--mudar pilha de cartas
				let newPilha = filter (/= carta1) pilha
				let newMesa3 = mesaSetPilha newMesa2 newPilha
				return (newJogador2,newMesa3,True)
				else do 
					if(length listaCartasSelecionadas > 0) then do 
						-- fez nada
						let newMesa = mesa
						let newJogador = jogador
						return (newJogador,newMesa,False)
						else do 
							-- carta sem par possivel, vai para mesa
							let carta1 = head cartasJogadorSelecionada
							let newDeckJogadorCartasJogaveis = filter (\x -> x /= carta1) (jogadorCartasJogaveis jogador)
							let newDeckJogadorCartasJogadas = (jogadorCartasJogadas jogador)
							let newJogador1 = (jogadorSetCartasJogaveis jogador newDeckJogadorCartasJogaveis)
							let newJogador2 = (jogadorSetCartasJogadas newJogador1 newDeckJogadorCartasJogadas)
							-- novaMesa
							let newCartasDaMesa = (mesaCartaMesa mesa) ++ [(cartaSetSelecionada carta1 False)]
							let newMesa1 = (mesaSetCartaMesa mesa newCartasDaMesa)
							newMesa2 <- selecionaCartaMesa newMesa1 []
							--mudar pilha de cartas
							let newPilha = filter (/= carta1) pilha
							let newMesa3 = mesaSetPilha newMesa2 newPilha
							return (newJogador2,newMesa3,True)
		else do
			--nada pra rolar
			let newMesa = mesa
			let newJogador = jogador
			return (newJogador,newMesa,False)

--verificar fim de rodada para um jogador
verificarRodadaJogador :: Estado -> IO()
verificarRodadaJogador estado = do
	-- veriricar e atualizar pontuacao do jogador
	jogador <- estadoJogador estado
	let mesaJogador = (jogadorMesaJogador jogador)
	mesa <- estadoMesa estado
	let cartaDoMes = (cartaMes $ mesaCartaDoMes mesa)
	let listaPontuacao = (calcularPontuacao mesaJogador cartaDoMes)
	if(length listaPontuacao > 0) then do
		janela <- estadoJanela estado
		let nomes = [ x | (x,y,z) <- listaPontuacao ]
		let lista = [ y | (x,y,z) <- listaPontuacao ]
		let pontuacao = [ z | (x,y,z) <- listaPontuacao ]
		let totalPontos = sum pontuacao
		let oldPontos = (jogadorPontuacao jogador)
		jogador <- return $ jogadorSetPontuacao jogador totalPontos
		set (estadoVarJogador estado) [value := jogador]
		if(totalPontos > oldPontos) then do
			continuarJogo <- dialogoPontucao janela nomes lista pontuacao
			if(continuarJogo) then do
				-- chama IA
				jogar estado
				else do
					-- finalizar rodada
					cpu <- estadoCPU estado
					let newCPU = jogadorSetPontuacao cpu 0
					set (estadoVarCPU estado) [value := newCPU]
					finalizarRodada estado
			else do
				--chama pra IA
				jogar estado
		else do
			--chama pra IA
			jogar estado
			

