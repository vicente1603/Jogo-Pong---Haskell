module Data(JogoPong(..), estadoInicial) where

 -- Dados que descrevem o estado do jogo (x, y). 
data JogoPong = Jogo
  { locBola :: (Float, Float)  -- localização.
  , velBola :: (Float, Float)  -- velocidade. 
  , jogador1 :: Float           -- Altura do bastão do jogador esquerdo.                            -- Zero é o meio da tela. 
  , locJogador2 :: (Float)  -- localização.
  , velJogador2 :: (Float)  -- velocidade. 
  , lgolD ::(Float)
  , lgolE :: (Float)
  , locTexto :: (Float, Float)
  , velTexto :: (Float, Float)
  , locTexto1 :: (Float, Float)
  , velTexto1 :: (Float, Float)
  }
  deriving Show 

--campo = 600
--raioB = 15
--bastao = (70,10)
--altruraT= 180

estadoInicial :: JogoPong 
estadoInicial = Jogo
  { locBola =  (-15, 50)
  , velBola =  (2.5, -4.5)
  , jogador1 =       50
  , locJogador2 = (-100)
  , velJogador2 = 100
  , lgolD = (0)
  , lgolE = (0)
  , locTexto =  (-1000, 1000)
  , velTexto =  (-15, 50)
  , locTexto1 =  (-1000, 1000)
  , velTexto1 =  (-15, 50)
  }

