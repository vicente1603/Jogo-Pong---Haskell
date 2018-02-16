module Main(main) where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Interface.Pure.Simulate
import Graphics.Gloss.Interface.Pure.Display
import Graphics.Gloss.Data.ViewPort
import Renderizacao
import Data
import Logica
import Controle
largura, altura, deslocamento :: Int
largura = 600
altura = 600
deslocamento = 5
type Localização = Float

window :: Display
window = InWindow "Projeto - LPSI" (largura, altura) (deslocamento, deslocamento)

corFundo :: Color
corFundo = white

fps :: Int
fps = 60

main :: IO ()
main = play window corFundo fps estadoInicial conversor evento atualizar

atualizar :: Float -> JogoPong -> JogoPong
atualizar segundos = puloGolD . puloGolE . puloGolB . puloParedeH . puloParedeV . puloParedeB . movJogador2 segundos . puloBastao . movBola segundos 


