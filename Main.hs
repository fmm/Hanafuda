
import Control.Monad

import Graphics.UI.WX
import Graphics.UI.WXCore

import Cartas
import GUI
import Eventos
import Util

main :: IO()
main = start gui

gui :: IO()
gui = do
	-- janela
	janela <- frameFixed [text := "Hanafuda"]	
	-- jogador 1
	pj <- panel janela [clientSize := sz comprimentoJanela 105]
	jogador <- variable [value := ([],[],pj,0)]
	-- mesa
	pm <- panel janela [clientSize := sz comprimentoJanela 250]
	mesa <- variable [value := (pm,[],[],baralho !! 0,0)]
	-- jogador 2
	pj' <- panel janela [clientSize := sz comprimentoJanela 105]
	jogador' <- variable [value := ([],[],pj',0)]
	-- quem comecou
	comecou <- variable [value := 0]
	-- proximo
	proximo <- variable [value := 0]
	-- layout
	set janela [layout := column 0 [ row 0 [widget pj], row 1 [widget pm], row 2 [widget pj'] ] ]	
	-- Estado do jogo
	estado <- return (janela,jogador,jogador',mesa,comecou,proximo)
	-- comeca jogo
	iniciar estado
	return ()
