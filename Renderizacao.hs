module Renderizacao(conversor) where

import Graphics.Gloss
import Data

campoL = 300   -- largura do campo
campo = 600    -- total campo 600X600
esp = 10       -- espessura das barras
diametro = 7.5 -- diametro da bola
gol = 180      -- altura da trave do gol


 -- Dados que descrevem o estado do jogo (x, y). 

   -- Converte um estado de jogo em uma imagem.
conversor :: JogoPong -> Picture
conversor game = pictures [textoD, textoE, bola, paredesH, paredesV, paredesV2, paredesGol, bastao rose (-(campoL-20)) $ jogador1 game, bastao orange (campoL-20) $ locJogador2 game] 
   where
    
     textoD = Scale 0.3 0.3 $ uncurry translate (locTexto game) $ color red $ text "GAME OVER - PLAYER Wins"

     textoE = Scale 0.3 0.3 $ uncurry translate (locTexto1 game) $ color red $ text "GAME OVER - CPU Wins"


     bola = uncurry translate (locBola game) $ color corBola $ circleSolid diametro
     corBola = black

     parede :: Float -> Picture
     parede deslocamento = translate 0 deslocamento $ color corParede $ rectangleSolid campo esp
     corParede = greyN 0.5
     paredesH = pictures[parede campoL, parede (-campoL)]

     paredeGol :: Float -> Picture
     paredeGol deslocamento = Rotate 90 $ translate (-10) deslocamento $ color corParedeGol $ rectangleSolid 140 10

     corParedeGol = white
     paredesGol = pictures [paredeGol campoL, paredeGol (-campoL)]

     paredeV :: Float -> Picture
     paredeV deslocamento = Rotate 90 $ translate gol deslocamento $ color corParedeV $ rectangleSolid campoL esp 

     corParedeV = greyN 0.5
     paredesV = pictures [paredeV campoL, paredeV (-campoL)]

     paredeV2 :: Float -> Picture
     paredeV2 deslocamento = Rotate 90 $ translate (-gol) deslocamento $ color corParedeV2 $ rectangleSolid campoL esp 

     corParedeV2 = greyN 0.5
     paredesV2 = pictures [paredeV2 campoL, paredeV2 (-campoL)]

     bastao :: Color -> Float -> Float -> Picture
     bastao col x y = pictures
        [
         translate x y $ color col $ rectangleSolid 10 70
        ]

     corBastão = light (light blue)


