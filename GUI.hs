
module GUI (
	comprimentoJanela,
	larguraJanela,
	comprimentoCarta,
	larguraCarta,
	desenharCarta,
	desenharJogadorCartas,
	desenharMesa,
	loop,
	dialogoContinuarRodada,
	dialogoPontucao,
	dialogoPontucaoIA,
	dialogoDealer,
	pintarYaku,
	dialogoPerdedor,
	dialogoVencedor,
	dialogoJogadorPontos
	) where

import Graphics.UI.WX
import Graphics.UI.WXCore

import Tipos
import Cartas

-- dimensoes da janela
comprimentoJanela :: Int
comprimentoJanela = 900

larguraJanela :: Int
larguraJanela = 600

-- dimensoes da carta
comprimentoCarta :: Int
comprimentoCarta = 56

larguraCarta :: Int
larguraCarta = 90

-- funcao para desenhar carta
desenharCarta :: Carta -> DC a -> Point -> IO()
desenharCarta carta dc ponto = do
	(_,_,arquivo,visivel,selecionada) <- return carta
	if(visivel) then do
		img <- bitmapCreateFromFile arquivo
		drawBitmap dc img ponto False []
		else do
			let fundo = "Imagens/Fundo.png"
			img <- bitmapCreateFromFile fundo 
			drawBitmap dc img ponto False []
	if(selecionada && visivel) then do
		let tam = 3
		let cor = yellow
		let (Point x y) = ponto
		drawRect dc (Rect x y comprimentoCarta tam) [color := cor, bgcolor := cor]
		drawRect dc (Rect x y tam larguraCarta) [color := cor, bgcolor := cor]
		drawRect dc (Rect x (y + larguraCarta - tam) comprimentoCarta tam) [color := cor, bgcolor := cor]
		drawRect dc (Rect (x + comprimentoCarta - tam) y tam larguraCarta) [color := cor, bgcolor := cor]
		else do
			return()

-- funcao de desenhar cartas do jogador
desenharJogadorCartas :: Jogador -> IO()
desenharJogadorCartas jog = do
	set p [on paint := func]
	repaint p
	return ()
	where
		d1 = jogadorCartasJogaveis jog
		d2 = jogadorCartasJogadas jog
		p = jogadorPanel jog
		q1 = length d1
		q2 = length d2
		comprimento1 = q1*comprimentoCarta
		comprimento2 = comprimentoJanela-(q1*comprimentoCarta)
		espaco = ((\x -> if(x /= 0) then 20 else 0) q1)
		passo = min ((comprimento2 - comprimentoCarta - espaco) `div` ((max q2 2) - 1)) comprimentoCarta
		pula = (comprimento2 - comprimentoCarta) - passo * ((max q2 2) - 1)
		func dc rect = do
			-- cartas jogaveis
			sequence [do desenharCarta (d1 !! i) dc (pt (comprimentoCarta*i) 5) | i <- [0..(q1-1)]]
			-- cartas jogadas
			--TODO:separar em categorias
			sequence [do desenharCarta (d2 !! i) dc (pt (passo*i+pula+comprimento1) 5) | i <- [0..(q2-1)]]
			return ()

-- funcoes de desenhar mesa
desenharMesa :: Mesa -> IO()
desenharMesa (p,pilha,mesa,mes,pontuacao) = do
	set p [clientSize := sz comprimentoJanela 250]
	set p [on paint := func]
	repaint p
	return ()
	where
		tamanhoPilha = length pilha
		tamanhoCarta = length mesa
		espaco = 10
		f x = (x `div` 2)
		g x = if(x `mod` 2 == 0) then (pt (150 + (comprimentoCarta+espaco)*(f x)) 20) else (pt (150 + (comprimentoCarta+espaco)*(f x)) (20+espaco+larguraCarta))
		h x = (show x) ++ " Pt" ++ ((\y -> if(y > 1) then "s" else "") x)
		func dc rect = do
			-- desenhar pilha de cartas
			sequence [ do desenharCarta (pilha !! i) dc (pt (30+(f i)) (75+(f i))) | i <- [0..(tamanhoPilha-1)] ]
			-- cartas da mesa
			sequence [ do desenharCarta (mesa !! i) dc (g i) | i <- [0..(tamanhoCarta-1)] ]
			-- carta do mes
			drawRect dc (Rect 0 0 0 0) [bgcolor := red] -- comente e descubra
			drawText dc "Carta do Mes" (pt (comprimentoJanela - 140) 45) []
			desenharCarta mes dc (pt (comprimentoJanela - 125) 75)
			drawText dc (h pontuacao) (pt (comprimentoJanela - 117) 180) []
			return ()

-- Desenhar
loop :: Estado -> IO()
loop estado = do
	-- variaveis
	cpu <- estadoCPU estado
	jogador <- estadoJogador estado
	mesa <- estadoMesa estado
	janela <- estadoJanela estado
	-- desenhar cartas da cpu
	desenharJogadorCartas cpu 
	-- desenhar cartas do player
	desenharJogadorCartas jogador
	-- desenhar pilha de cartas
	desenharMesa mesa
	repaint janela
	return ()

-------------------------------------------------------
--TESTAR ----------------------------------------------
-------------------------------------------------------
dialogoPontucao :: Frame() -> [String] -> [Deck] -> [Int] -> IO Bool
dialogoPontucao janela nomes lista pontuacao = do
	d <- dialog janela [text := "Pontuacao"]
	let tt = length nomes
	p <- panel d [clientSize := sz 500 (tt * larguraCarta + (tt-1) * 20 + 50)]
	set p [on paint := pintarYaku nomes lista pontuacao]
	ok <- button d [text := "Ok"]
	--TODO melhorar posicao do botao dps
	set d [layout := column 0 [widget p, widget ok]]
	result <- showModal d (\stop -> set ok [on command := stop (Just 42)])
	--TODO: debug do resultado
	--putStrLn (show result)
	--TODO: janela para decidir continuar (jogador), o da IA q decide 
	dialogoContinuarRodada janela
	
dialogoPontucaoIA :: Frame() -> [String] -> [Deck] -> [Int] -> IO Bool
dialogoPontucaoIA janela nomes lista pontuacao = do
	d <- dialog janela [text := "Pontuacao"]
	let tt = length nomes
	p <- panel d [clientSize := sz 500 (tt * larguraCarta + (tt-1) * 20 + 50)]
	set p [on paint := pintarYaku nomes lista pontuacao]
	ok <- button d [text := "Ok"]
	--TODO melhorar posicao do botao dps
	set d [layout := column 0 [widget p, widget ok]]
	result <- showModal d (\stop -> set ok [on command := stop (Just 42)])
	--TODO: debug do resultado
	--putStrLn (show result)
	--TODO: janela para decidir continuar (jogador), o da IA q decide 
	return False --TODO:randomizar decisao?
	
dialogoContinuarRodada :: Frame() -> IO Bool
dialogoContinuarRodada janela = do
	confirmDialog janela "Continuar..." "Voce deseja continuar a rodada?" True
	
dialogoDealer :: Frame() -> IO()
dialogoDealer janela = do
	infoDialog janela "Dealer's privilege" "   6 Pts                    "

pintarYaku :: [String] -> [Deck] -> [Int] -> DC a -> Rect -> IO()
pintarYaku nomes lista pontuacao dc rect = do
	let comprimento = 300
	let espaco = larguraCarta + 20
	let tnome = 130
	let q = length lista
	let h x = (show x) ++ " Pt" ++ ((\y -> if(y > 1) then "s" else "") x)
	sequence [ do
		let deck = lista !! i
		let t = length deck
		let pulo = min ((comprimento - comprimentoCarta) `div` (t - 1)) (comprimentoCarta + 10)
		-- desenhar nome da pontuacao
		drawText dc (nomes !! i) (pt 10 (i*espaco+40)) []
		-- desenhar lista de cartas
		sequence [ do
			desenharCarta (deck !! j) dc (pt (tnome + j*pulo) (i*espaco+5))
			return ()
			| j <- [0..(t-1)]
			]
		-- desenhar pontuacao
		drawText dc (h (pontuacao !! i))  (pt (comprimento + tnome + 20) (i*espaco+40)) []
		return ()
		| i <- [0..(q-1)]
		]
	-- desenhar pontuacao total
	let tudo = sum pontuacao
	drawText dc (h tudo)  (pt (comprimento + tnome + 20) (q*espaco)) []
	return ()

-- dialogos de fim de jogo
dialogoPerdedor :: Frame() -> IO()
dialogoPerdedor janela = do
	infoDialog janela "Game over" "Que pena, eu venci de vc, tente uma proxima vez ^^."

dialogoVencedor :: Frame() -> IO()
dialogoVencedor janela = do
	infoDialog janela "Finalizando..." "Que legal, voce me derrotou, parabens =)!"
	
dialogoJogadorPontos :: Frame() -> String -> Int -> IO()
dialogoJogadorPontos janela nome pontos = do
	let h x = (show x) ++ " ponto" ++ ((\y -> if(y > 1) then "s" else "") x)
	infoDialog janela "Resultado final da rodada" (nome ++ " " ++ (h pontos) ++ "                  ")

	

