
module Regras (
	calcularPontuacao
	) where

import Tipos

-- valores e nomes da pontuacao
kasu = 10
tanzaku = 5
tane = 5
inoshikacho = 5
akatan = 6
aotan = 6
sanko = 6
ameshiko = 8
shiko = 10
goko = 15
tsukifuda = 4
oyaken = 6
	
calcularPontuacao :: MesaJogador -> CartaMes -> [Pontuacao]
calcularPontuacao (plains, faixas, animais, brights) mes = (calcularBright brights(calcularAnimal animais (calcularFaixa faixas (calcularPlain plains animais (calcularMes (plains, faixas, animais, brights) mes [])))))

calcularMes :: MesaJogador -> CartaMes -> [Pontuacao] -> [Pontuacao]
calcularMes (plains, faixas, animais, brights) mes pontuacao
															| length cartas == 4 = [("Tsuki-fuda", cartas, tsukifuda)]
															| otherwise = pontuacao
															where
																cartas = (procurarMes plains mes (procurarMes faixas mes (procurarMes animais mes (procurarMes brights mes []))))
																
procurarMes :: [Carta] -> CartaMes -> [Carta] -> [Carta]
procurarMes [] mes cartas = cartas
procurarMes (head:tail) mes cartas
										| cartaMes head == mes = procurarMes tail mes (cartas ++ [head])
										| otherwise = procurarMes tail mes cartas

calcularPlain :: CartasPlain -> CartasAnimal -> [Pontuacao] -> [Pontuacao]
calcularPlain plains animais pontuacao
										| qtde >= kasu = pontuacao ++ [("Kasu", plainsSake, (qtde - (kasu - 1)))]
										| otherwise = pontuacao
										where 
											plainsSake = plains ++ (procurarSake animais)
											qtde = length plainsSake

calcularFaixa :: CartasFaixa -> [Pontuacao] -> [Pontuacao]
calcularFaixa faixas pontuacao
								| qtde >= tanzaku = novaPontuacao ++ [("Tanzaku", faixas, (qtde - (tanzaku - 1)))]
								| otherwise = novaPontuacao
								where
									qtde = (length faixas)
									novaPontuacao = (calcularFaixaEspecifica FaixaEscrita faixas [] (calcularFaixaEspecifica FaixaRoxa faixas [] pontuacao))

calcularFaixaEspecifica :: TipoFaixa -> CartasFaixa -> [Carta] -> [Pontuacao] -> [Pontuacao]
calcularFaixaEspecifica tipo [] cartas pontuacao
									| qtde == 3 && tipo == FaixaRoxa = pontuacao ++ [("Aotan", cartas, aotan)]
									| qtde == 3 && tipo == FaixaEscrita = pontuacao ++ [("Akatan", cartas, akatan)]
									| otherwise = pontuacao
									where
										qtde = length cartas
calcularFaixaEspecifica tipo (head:tail) cartas pontuacao
									| cartaTipo head == Faixa tipo = calcularFaixaEspecifica tipo tail (cartas ++ [head]) pontuacao
									| otherwise = calcularFaixaEspecifica tipo tail cartas pontuacao

calcularAnimal :: CartasAnimal -> [Pontuacao] -> [Pontuacao]
calcularAnimal animais pontuacao
								| qtde >= tane = novaPontuacao ++ [("Tane", animais, (qtde - (tane - 1)))]
								| otherwise = novaPontuacao
								where
									qtde = length animais
									novaPontuacao = calcularInoShikaCho animais [] pontuacao

calcularInoShikaCho :: CartasAnimal -> [Carta] -> [Pontuacao] -> [Pontuacao]
calcularInoShikaCho [] cartas pontuacao
										| qtde == 3 = pontuacao ++ [("Ino-Shika-Cho", cartas, inoshikacho)]
										| otherwise = pontuacao
										where
											qtde = length cartas
calcularInoShikaCho (head:tail) cartas pontuacao
												| cartaTipo head == Animal InoShikaCho = calcularInoShikaCho tail (cartas ++ [head]) pontuacao
												| otherwise = calcularInoShikaCho tail cartas pontuacao

calcularBright :: CartasBright -> [Pontuacao] -> [Pontuacao]
calcularBright brights pontuacao
								| qtdeGeral + qtdeAmeman == 5 = pontuacao ++ [("Goko", brights, goko)]
								| qtdeGeral == 4 = pontuacao ++ [("Shiko", brights, shiko)]
								| qtdeGeral == 3 && qtdeAmeman == 1 = pontuacao ++ [("Ame-Shiko", brights, ameshiko)]
								| qtdeGeral == 3 = pontuacao ++ [("Sanko", brights, sanko)]
								| otherwise = pontuacao
								where
									qtdeGeral = length geral
									qtdeAmeman = length chuva
									(geral, chuva) = separarAmeman brights ([], [])

separarAmeman :: CartasBright -> ([Carta], [Carta]) -> ([Carta], [Carta])
separarAmeman [] tupla = tupla
separarAmeman (head:tail) (geral, chuva)
							| (cartaTipo head) == Brilhante Ameman = separarAmeman tail (geral, (chuva ++ [head]))
							| otherwise = separarAmeman tail ((geral ++ [head]), chuva)
																
procurarSake :: CartasAnimal -> [Carta]
procurarSake cartas = filter (\x -> cartaTipo x == Sake) cartas
