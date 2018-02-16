module Logica(movTexto, movTexto1, movBola, movJogador2, colisaoParedeH, colisaoParedeV, puloParedeH, puloParedeV, puloGolD, puloGolE, puloGolB, puloBastao, puloParedeB) where


import Data


type Raio = Float 
type Posicao = (Float, Float)

--campo = 600
--raioB = 15
--bastao = (70,10)
--altruraT= 180
campo = 300
largura, altura, deslocamento :: Int
largura = 300   
altura = 300        
raio = 7.5          --- raio da bola
deslocamento = 100
velJ2 = 10          --- velocidade jogador 2
velBol= 50
velText= 0          --- velocidade bola
alturaB = 35        --- altura do bastao
larguraB = 10       --- largura do bastao

movBola :: Float    -- O número em segundos desde a última atualização.
         -> JogoPong -- O etado inicial do jogo.
         -> JogoPong --  Um novo estado de jogo com uma posição de bola atualizada.
movBola segundos jogo = jogo { locBola = (x', y') }
  where
    (x, y) = locBola jogo
    (vx, vy) = velBola jogo
    x' = x + vx * (segundos*velBol) 
    y' = y + vy * (segundos*velBol)

movTexto :: Float    -- O número em segundos desde a última atualização.
         -> JogoPong -- O etado inicial do jogo.
         -> JogoPong --  Um novo estado de jogo com uma posição de bola atualizada.
movTexto segundos jogo = jogo { locTexto = (x', y') }
  where
    (x, y) = locTexto jogo
    (vx, vy) = velTexto jogo
    x' = x + vx * (segundos*velText) 
    y' = y + vy * (segundos*velText)

movTexto1 :: Float    -- O número em segundos desde a última atualização.
         -> JogoPong -- O etado inicial do jogo.
         -> JogoPong --  Um novo estado de jogo com uma posição de bola atualizada.
movTexto1 segundos jogo = jogo { locTexto1 = (x', y') }
  where
    (x, y) = locTexto1 jogo
    (vx, vy) = velTexto1 jogo
    x' = x + vx * (segundos*velText) 
    y' = y + vy * (segundos*velText)


movJogador2 :: Float  -> JogoPong -> JogoPong -- Um novo estado de jogo com uma posição de bola atualizada.
movJogador2 segundos jogo = jogo { locJogador2 = (x') }
  where

    (x) = locJogador2 jogo
    (vx) = velJogador2 jogo
    x' = x + vx * (segundos*velJ2) 
  



-- | Dada a posição e o raio da bola, devolve se ocorreu uma colisão.
colisaoParedeH :: Posicao -> Raio -> Bool 
colisaoParedeH (_, y) raio = colisãoTopo || colisãoChão 
  where
    colisãoTopo = y - raio <= -fromIntegral largura
    colisãoChão = y + raio >=  fromIntegral largura

colisaoParedeV :: Posicao -> Raio -> Bool 
colisaoParedeV (x, _) raio = colisãoE || colisãoD 
  where
    colisãoE = x - raio <= -fromIntegral largura
    colisãoD = x + raio >=  fromIntegral largura

colisaoGol :: Posicao -> Raio -> Float -> Float-> Bool
colisaoGol (x,y) raio golD golE = colisãoDireita || colisãoEsquerda
   where
      colisãoDireita = x - raio  >= (campo-20) && y <= (golE + 70 ) && y >= (golE - 70)
      colisãoEsquerda = x + raio <= (-(campo-20)) && y <= (golD + 70) && y >= (golD - 70)

colisaoGolD :: Posicao -> Raio -> Float -> Float-> Bool
colisaoGolD (x,y) raio golD golE = colisãoDireita
   where
      colisãoDireita = x - raio  >= (campo-20) && y <= (golE + 70 ) && y >= (golE - 70)

colisaoGolE :: Posicao -> Raio -> Float -> Float-> Bool
colisaoGolE (x,y) raio golD golE = colisãoEsquerda
   where
      colisãoEsquerda = x + raio <= (-(campo-20)) && y <= (golD + 70) && y >= (golD - 70)

puloGolD :: (JogoPong -> JogoPong)
puloGolD jogo = jogo { locTexto = (va', vb'), lgolD = golD, lgolE = golE'}
   where
      (va, vb) = locTexto jogo
      golE' = lgolE jogo
      golD = lgolD jogo

      (va', vb') = if colisaoGolD (locBola jogo) raio golD golE'
                   then (-980,500)
                   else (va, vb)

puloGolE :: (JogoPong -> JogoPong)
puloGolE jogo = jogo { locTexto1 = (vx', vy'), lgolD = golD, lgolE = golE'}
   where
      (vx, vy) = locTexto1 jogo
      golE' = lgolE jogo
      golD = lgolD jogo

      (vx', vy') = if colisaoGolE (locBola jogo) raio golD golE'
                   then (-850,500)
                   else (vx, vy)

puloGolB :: (JogoPong -> JogoPong)
puloGolB jogo = jogo { velBola = (vw', vz'), lgolD = golD, lgolE = golE'}
   where
      (vw, vz) = velBola jogo
      golE' = lgolE jogo
      golD = lgolD jogo

      (vw', vz') = if colisaoGol (locBola jogo) raio golD golE'
                   then (0,-1000)
                   else (vw, vz)

puloParedeH :: JogoPong -> JogoPong
puloParedeH jogo = jogo { velBola = (vx, vy') }
  where
    (vx, vy) = velBola jogo

    vy' = if colisaoParedeH (locBola jogo) raio
          then
             -vy
           else
            vy

puloParedeV :: JogoPong -> JogoPong
puloParedeV jogo = jogo { velBola = (vx', vy) }
  where
    (vx, vy) = velBola jogo

    vx' = if colisaoParedeV (locBola jogo) raio
          then
             -vx
           else
            vx

colisaoBastao :: Posicao -> Raio -> Float -> Float-> Bool
colisaoBastao (x,y) raio bastãoDireito bastãoEsquerdo = colisãoDireita || colisãoEsquerda
   where
      colisãoDireita = x - raio  >= (campo-40) && y <= (bastãoEsquerdo + alturaB ) && y >= (bastãoEsquerdo - alturaB)
      colisãoEsquerda = x + raio <= (-(campo-40)) && y <= (bastãoDireito + alturaB) && y >= (bastãoDireito - alturaB)

puloBastao :: JogoPong -> JogoPong
puloBastao jogo = jogo { velBola = (vx', vy'), jogador1 = bastãoDireito, locJogador2 = bastãoEsquerdo'}
   where
      (vx, vy) = velBola jogo
      bastãoEsquerdo' = locJogador2 jogo
      bastãoDireito = jogador1 jogo

      (vx', vy') = if colisaoBastao (locBola jogo) raio bastãoDireito bastãoEsquerdo'
                   then (-vx, vy)
                   else (vx, vy)

-- | Dada a posição do bastão, devolve se ocorreu uma colisão.
colisaoParedeB :: Float -> Float -> Bool 
colisaoParedeB (x) altura = colisãoTopo || colisãoChão
  where
    colisãoTopo    = (x - altura )<= -fromIntegral (largura -5)
    colisãoChão    = (x + altura )>=  fromIntegral (largura -5)



puloParedeB :: JogoPong -> JogoPong
puloParedeB jogo = jogo { velJogador2 = (vx) }
  where
    (vx') = velJogador2 jogo

    vx = if colisaoParedeB (locJogador2 jogo) alturaB
          then
             -vx'
           else
            vx'




