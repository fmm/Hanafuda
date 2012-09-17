module IA (
	escolhaIA
	) where
	
import Tipos

type NivelDificuldade = Int

escolhaIA :: NivelDificuldade -> CartaMes -> [Carta] -> Deck -> MesaJogador -> MesaJogador -> [Carta] -- retorno : [cartaEscolhida, cartaMesa <se existente>]
escolhaIA dificuldade mes entrada mesa mesaJogador mesaOponente = escolhaFinal melhorEscolha entrada mesa
			where
				pesosIndice = avaliar entrada 0 mesa mes mesaJogador mesaOponente []
				melhorEscolha = pesosIndice!!( escolherCarta (getPesos pesosIndice) dificuldade)
				
escolhaFinal :: (Int,Int,Int) -> [Carta] -> Deck -> [Carta]
escolhaFinal (iMao, iMesa,_) cartasMao cartasMesa 
								|iMesa < 0 = [cartasMao!!iMao, cartasMao!!iMao]
								|otherwise = [cartasMao!!iMao, cartasMesa!!iMesa]

getPesos :: [(Int,Int,Int)] -> [Int]
getPesos [] = []
getPesos (x:xs) = getTerceiro x : getPesos xs
 	
getTerceiro :: (Int,Int,Int) -> Int
getTerceiro (_,_,n) = n
				
escolherCarta :: [Int] -> Int -> Int
escolherCarta pesos dificuldade
								| dificuldade >= 3 = i1
								| dificuldade == 2 = i2
								| otherwise = i3
								where
									(i1, i2, i3) = escolherCartaMax pesos 0 (0, 0, 0) (-1, -1, -1)

escolherCartaMax :: [Int] -> Int -> (Int, Int, Int) -> (Int, Int, Int) -> (Int, Int, Int)
escolherCartaMax [] indice (i1, i2, i3) (maxi, max2, max3)
														| i2 < 0 = (i1, i1, i1)
														| i3 < 0 = (i1, i2, i2)
														| otherwise = (i1, i2, i3)

escolherCartaMax (p:ps) indice (i1, i2, i3) (maxi, max2, max3)
                	|p >= maxi = escolherCartaMax ps (indice+1) (indice, i1, i2) (p, maxi, max2)
									|p >= max2 = escolherCartaMax ps (indice+1) (i1, indice, i2) (maxi, p, max2)
									|p >= max3 = escolherCartaMax ps (indice+1) (i1, i2, indice) (maxi, max2, p)
                	|otherwise = escolherCartaMax ps (indice+1) (i1, i2, i3) (maxi, max2, max3)

avaliar :: [Carta] -> Int -> [Carta] -> CartaMes -> MesaJogador -> MesaJogador -> [(Int, Int, Int)] -> [(Int, Int, Int)]
avaliar [] _ _ _ _ _ triplas = triplas
avaliar (entrada:entradas) indice mesa mes mesaJogador mesaOponente triplas = avaliar entradas (indice + 1) mesa mes mesaJogador mesaOponente (triplas ++ [(indice, iMesa, valor)])
																					where
																						mesaAval = avaliarMesa (entrada:entradas) mesa mes mesaJogador mesaOponente []
																						(iMesa, valor) = casarMesaMao mesaJogador entrada mesa mesaAval mes 0 (-1, -1)
																						
casarMesaMao :: MesaJogador -> Carta -> [Carta] -> [Int] -> CartaMes -> Int -> (Int, Int) -> (Int, Int)
casarMesaMao mesaJogador entrada [] _ mes _ (iMesa, valor)
									| iMesa == -1 = (iMesa, (avaliarDescarte entrada mesaJogador mes))
									| otherwise = (iMesa, valor)
casarMesaMao mesaJogador entrada (mesa:mesas) valorMesa mes indice (iMesa, valor)
														| (cartaMes entrada) == (cartaMes mesa) && valorAtual > valor = casarMesaMao mesaJogador entrada mesas valorMesa mes (indice + 1) (indice, valorAtual)
														| otherwise = casarMesaMao mesaJogador entrada mesas valorMesa mes (indice + 1) (iMesa, valor)
														where
															valorAtual = ((valorMesa !! indice) + (avaliarCarta (cartaTipo entrada) mesaJogador))

avaliarDescarte :: Carta -> MesaJogador -> CartaMes -> Int
avaliarDescarte entrada mesaJogador mes
									| tipo == Sake = 6
									| tipo == Plain = 8
									| tipo == Faixa FaixaVermelha = 7
									| tipo == Faixa FaixaRoxa || tipo == Faixa FaixaEscrita  = 3
									| tipo == Animal AnimalGeral = 5
									| tipo == Animal InoShikaCho = 2
									| tipo == Brilhante Geral = 1
									| tipo == Brilhante Ameman = 4
									where
										tipo = cartaTipo entrada
										plains = mesaJogadorPlains mesaJogador
										faixas = mesaJogadorFaixa mesaJogador
										animais = mesaJogadorAnimal mesaJogador
										brights = mesaJogadorBright mesaJogador
	
somarMes :: Carta -> CartaMes -> Int
somarMes entrada mes
					| cartaMes entrada == mes = 5
					| otherwise = 0
															
avaliarMesa :: [Carta] -> [Carta] -> CartaMes -> MesaJogador -> MesaJogador -> [Int] -> [Int]
avaliarMesa _ [] _ _ _ valores = valores
avaliarMesa entrada (mesa:mesas) mes mesaJogador mesaOponente valores
	| mes == (cartaMes mesa) = avaliarMesa entrada mesas mes mesaJogador mesaOponente (valores ++ [(avaliarCarta (cartaTipo mesa) mesaJogador) + (contarMes mes mesaJogador)+ (avaliarCarta (cartaTipo mesa) mesaOponente) + (contarMes mes mesaOponente)])
	| otherwise = avaliarMesa entrada mesas mes mesaJogador mesaOponente (valores ++ [(avaliarCarta (cartaTipo mesa) mesaJogador) + (avaliarCarta (cartaTipo mesa) mesaOponente)])
	
avaliarCarta :: Tipo -> MesaJogador -> Int
avaliarCarta tipo mesaJogador
								| tipo == Sake = 25 + (10 * length plains) + (20 * (length animais))
								| tipo == Plain = 10 + (10 * length plains)
								| tipo == Faixa FaixaVermelha = 20 + 20 * (length faixas)
								| tipo == Faixa FaixaRoxa || tipo == Faixa FaixaEscrita  = 60 + (20 * (length faixas)) + (40 * (contarTipo tipo faixas 0))
								| tipo == Animal AnimalGeral = 20 + 20 * (length animais)
								| tipo == Animal InoShikaCho = 50 + (20 * (length faixas)) + (30 * (contarTipo tipo animais 0)) 
								| tipo == Brilhante Geral = 20 + (60 * length brights)
								| tipo == Brilhante Ameman = 10 + (50 * length brights)
								where
									plains = mesaJogadorPlains mesaJogador
									faixas = mesaJogadorFaixa mesaJogador
									animais = mesaJogadorAnimal mesaJogador
									brights = mesaJogadorBright mesaJogador
									
contarTipo :: Tipo -> [Carta] -> Int -> Int
contarTipo tipo [] qtde = qtde
contarTipo tipo (mesa:mesas) qtde 
							| tipo == Faixa FaixaRoxa && cartaTipo mesa == Faixa FaixaRoxa = contarTipo tipo mesas(qtde + 1)
							| tipo == Faixa FaixaEscrita && cartaTipo mesa == Faixa FaixaEscrita = contarTipo tipo mesas (qtde + 1)
							| tipo == Animal InoShikaCho && cartaTipo mesa == Animal InoShikaCho = contarTipo tipo mesas (qtde + 1)
							| otherwise = contarTipo tipo mesas qtde
												
contarMes :: CartaMes -> MesaJogador -> Int
contarMes mes mesaJogador = (contarMesTipo mes brights(contarMesTipo mes animais(contarMesTipo mes faixas(contarMesTipo mes plains 0))))
							where
									plains = mesaJogadorPlains mesaJogador
									faixas = mesaJogadorFaixa mesaJogador
									animais = mesaJogadorAnimal mesaJogador
									brights = mesaJogadorBright mesaJogador
									
contarMesTipo :: CartaMes -> [Carta] -> Int -> Int
contarMesTipo _ [] qtde = qtde
contarMesTipo mes (carta:cartas) qtde
									| (cartaMes carta) == mes = contarMesTipo mes cartas (qtde + 1)
									| otherwise =  contarMesTipo mes cartas qtde

mesaJogadorPlains :: MesaJogador -> CartasPlain
mesaJogadorPlains (pl, fx, an, br) = pl

mesaJogadorFaixa :: MesaJogador -> CartasFaixa
mesaJogadorFaixa (pl, fx, an, br) = fx

mesaJogadorAnimal :: MesaJogador -> CartasAnimal
mesaJogadorAnimal (pl, fx, an, br) = an

mesaJogadorBright :: MesaJogador -> CartasBright
mesaJogadorBright (pl, fx, an, br) = br 
