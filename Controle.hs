module Controle(evento) where

import Graphics.Gloss.Interface.Pure.Game
import Data


evento :: Event -> JogoPong -> JogoPong

evento (EventKey (Char 'r') _ _ _) jogo =
  jogo { locBola = (-15, 50),
         velBola =  (2.5,   -4.5),
         locTexto = (-1000,1000),
         locTexto1 = (-1000,1000) }

evento (EventKey (SpecialKey KeyUp) _ _ _) jogo = jogo { jogador1 = (x)}
                                                  where
                                                   x = if (jogador1 jogo) <= 250 then ((jogador1 jogo) + 10) else (jogador1 jogo)
evento (EventKey (SpecialKey KeyDown) _ _ _) jogo = jogo { jogador1 = (x)}
                                                  where
                                                   x = if (jogador1 jogo) >= -250 then ((jogador1 jogo) - 10) else (jogador1 jogo)
evento _ jogo = jogo
