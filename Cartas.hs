
module Cartas (
	baralho
	) where
	
import Tipos

cartaJaneiroPlain1 :: Carta 
cartaJaneiroPlain1 = (Janeiro, Plain, "cartaJaneiroPlain1.png", True, False)
cartaJaneiroPlain2 :: Carta 
cartaJaneiroPlain2 = (Janeiro, Plain, "cartaJaneiroPlain2.png", True,  False)
cartaJaneiroFaixa :: Carta
cartaJaneiroFaixa = (Janeiro, (Faixa FaixaEscrita), "cartaJaneiroFaixa.png", True,  False)
cartaJaneiroBrilhante :: Carta
cartaJaneiroBrilhante = (Janeiro, (Brilhante Geral), "cartaJaneiroBrilhante.png", True,  False)
cartaFevereiroPlain1 :: Carta
cartaFevereiroPlain1 = (Fevereiro, Plain, "cartaFevereiroPlain1.png", True,  False)
cartaFevereiroPlain2 :: Carta
cartaFevereiroPlain2 = (Fevereiro, Plain, "cartaFevereiroPlain2.png", True,  False)
cartaFevereiroFaixa :: Carta
cartaFevereiroFaixa = (Fevereiro, (Faixa FaixaEscrita), "cartaFevereiroFaixa.png", True,  False)
cartaFevereiroAnimal :: Carta
cartaFevereiroAnimal = (Fevereiro, (Animal AnimalGeral), "cartaFevereiroAnimal.png", True,  False)
cartaMarcoPlain1 :: Carta
cartaMarcoPlain1 = (Marco, Plain, "cartaMarcoPlain1.png", True,  False)
cartaMarcoPlain2 :: Carta
cartaMarcoPlain2 = (Marco, Plain, "cartaMarcoPlain2.png", True,  False)
cartaMarcoFaixa :: Carta
cartaMarcoFaixa = (Marco, (Faixa FaixaEscrita), "cartaMarcoFaixa.png", True,  False)
cartaMarcoBrilhante :: Carta
cartaMarcoBrilhante = (Marco, (Brilhante Geral), "cartaMarcoBrilhante.png", True,  False)
cartaAbrilPlain1 :: Carta
cartaAbrilPlain1 = (Abril, Plain, "cartaAbrilPlain1.png", True,  False)
cartaAbrilPlain2 :: Carta
cartaAbrilPlain2 = (Abril, Plain, "cartaAbrilPlain2.png", True,  False)
cartaAbrilFaixa :: Carta
cartaAbrilFaixa = (Abril, (Faixa FaixaVermelha), "cartaAbrilFaixa.png", True,  False)
cartaAbrilAnimal :: Carta
cartaAbrilAnimal = (Abril, (Animal AnimalGeral), "cartaAbrilAnimal.png", True,  False)
cartaMaioPlain1 :: Carta
cartaMaioPlain1 = (Maio, Plain, "cartaMaioPlain1.png", True,  False)
cartaMaioPlain2 :: Carta
cartaMaioPlain2 = (Maio, Plain, "cartaMaioPlain2.png", True,  False)
cartaMaioFaixa :: Carta
cartaMaioFaixa = (Maio, (Faixa FaixaVermelha), "cartaMaioFaixa.png", True,  False)
cartaMaioAnimal :: Carta
cartaMaioAnimal = (Maio, (Animal AnimalGeral), "cartaMaioAnimal.png", True,  False)
cartaJunhoPlain1 :: Carta
cartaJunhoPlain1 = (Junho, Plain, "cartaJunhoPlain1.png", True,  False)
cartaJunhoPlain2 :: Carta
cartaJunhoPlain2 = (Junho, Plain, "cartaJunhoPlain2.png", True,  False)
cartaJunhoFaixa :: Carta
cartaJunhoFaixa = (Junho, (Faixa FaixaRoxa), "cartaJunhoFaixa.png", True,  False)
cartaJunhoAnimal :: Carta
cartaJunhoAnimal = (Junho, (Animal InoShikaCho), "cartaJunhoAnimal.png", True,  False)
cartaJulhoPlain1 :: Carta
cartaJulhoPlain1 = (Julho, Plain, "cartaJulhoPlain1.png", True,  False)
cartaJulhoPlain2 :: Carta
cartaJulhoPlain2 = (Julho, Plain, "cartaJulhoPlain2.png", True,  False)
cartaJulhoFaixa :: Carta
cartaJulhoFaixa = (Julho, (Faixa FaixaVermelha), "cartaJulhoFaixa.png", True,  False)
cartaJulhoAnimal :: Carta
cartaJulhoAnimal = (Julho, (Animal InoShikaCho), "cartaJulhoAnimal.png", True,  False)
cartaAgostoPlain1 :: Carta
cartaAgostoPlain1 = (Agosto, Plain, "cartaAgostoPlain1.png", True,  False)
cartaAgostoPlain2 :: Carta
cartaAgostoPlain2 = (Agosto, Plain, "cartaAgostoPlain2.png", True,  False)
cartaAgostoAnimal :: Carta
cartaAgostoAnimal = (Agosto, (Animal AnimalGeral), "cartaAgostoAnimal.png", True,  False)
cartaAgostoBrilhante :: Carta
cartaAgostoBrilhante = (Agosto, (Brilhante Geral), "cartaAgostoBrilhante.png", True,  False)
cartaSetembroPlain1 :: Carta
cartaSetembroPlain1 = (Setembro, Plain, "cartaSetembroPlain1.png", True,  False)
cartaSetembroPlain2 :: Carta
cartaSetembroPlain2 = (Setembro, Plain, "cartaSetembroPlain2.png", True,  False)
cartaSetembroFaixa :: Carta
cartaSetembroFaixa = (Setembro, (Faixa FaixaRoxa), "cartaSetembroFaixa.png", True,  False)
cartaSetembroSake :: Carta
cartaSetembroSake = (Setembro, Sake, "cartaSetembroSake.png", True,  False)
cartaOutubroPlain1 :: Carta
cartaOutubroPlain1 = (Outubro, Plain, "cartaOutubroPlain1.png", True,  False)
cartaOutubroPlain2 :: Carta
cartaOutubroPlain2 = (Outubro, Plain, "cartaOutubroPlain2.png", True,  False)
cartaOutubroFaixa :: Carta
cartaOutubroFaixa = (Outubro, (Faixa FaixaRoxa), "cartaOutubroFaixa.png", True,  False)
cartaOutubroAnimal :: Carta
cartaOutubroAnimal = (Outubro, (Animal InoShikaCho), "cartaOutubroAnimal.png", True,  False)
cartaNovembroFaixa :: Carta
cartaNovembroFaixa = (Novembro, (Faixa FaixaVermelha), "cartaNovembroFaixa.png", True,  False)
cartaNovembroAnimal :: Carta
cartaNovembroAnimal = (Novembro, (Animal AnimalGeral), "cartaNovembroAnimal.png", True,  False)
cartaNovembroBrilhante :: Carta
cartaNovembroBrilhante = (Novembro, (Brilhante Ameman), "cartaNovembroBrilhante.png", True,  False)
cartaNovembroPlain :: Carta
cartaNovembroPlain = (Novembro, Plain, "cartaNovembroPlain.png", True,  False)
cartaDezembroPlain1 :: Carta
cartaDezembroPlain1 = (Dezembro, Plain, "cartaDezembroPlain1.png", True,  False)
cartaDezembroPlain2 :: Carta
cartaDezembroPlain2 = (Dezembro, Plain, "cartaDezembroPlain2.png", True,  False)
cartaDezembroPlain3 :: Carta
cartaDezembroPlain3 = (Dezembro, Plain, "cartaDezembroPlain3.png", True,  False)
cartaDezembroBrilhante :: Carta
cartaDezembroBrilhante = (Dezembro, (Brilhante Geral), "cartaDezembroBrilhante.png", True,  False)

baralho' :: Deck
baralho' = [cartaJaneiroPlain1, cartaJaneiroPlain2, cartaJaneiroFaixa, cartaJaneiroBrilhante, cartaFevereiroPlain1, cartaFevereiroPlain2, cartaFevereiroFaixa, cartaFevereiroAnimal, cartaMarcoPlain1, cartaMarcoPlain2, cartaMarcoFaixa, cartaMarcoBrilhante, cartaAbrilPlain1, cartaAbrilPlain2, cartaAbrilFaixa, cartaAbrilAnimal, cartaMaioPlain1, cartaMaioPlain2, cartaMaioFaixa, cartaMaioAnimal, cartaJunhoPlain1, cartaJunhoPlain2, cartaJunhoFaixa, cartaJunhoAnimal, cartaJulhoPlain1, cartaJulhoPlain2, cartaJulhoFaixa, cartaJulhoAnimal, cartaAgostoPlain1, cartaAgostoPlain2, cartaAgostoAnimal, cartaAgostoBrilhante, cartaSetembroPlain1, cartaSetembroPlain2, cartaSetembroFaixa, cartaSetembroSake, cartaOutubroPlain1, cartaOutubroPlain2, cartaOutubroFaixa, cartaOutubroAnimal, cartaNovembroFaixa, cartaNovembroAnimal, cartaNovembroBrilhante, cartaNovembroPlain, cartaDezembroPlain1, cartaDezembroPlain2, cartaDezembroPlain3, cartaDezembroBrilhante]

baralho :: Deck
baralho = [ let (a,b,c,d,e) = carta in (a,b,"Imagens/" ++ c,d,e) | carta <- baralho' ]
