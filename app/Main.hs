import System.Random (randomRIO)
import Text.Read (readMaybe)

type Dado = Int
type Jogo = [Dado]

-- Sorteia uma face inicial para um dado
sorteiaFace :: IO Dado
sorteiaFace = randomRIO (1, 6)

-- Inicializa o jogo com a quantidade de dados fornecida
inicializaJogo :: Int -> IO Jogo
inicializaJogo n = sequence (replicate n sorteiaFace)

-- Exibe o estado atual do jogo
exibeJogo :: Jogo -> IO ()
exibeJogo jogo = putStrLn $ "Estado do jogo: " ++ unwords (map showDado jogo)

-- Converte o dado para uma String
showDado :: Dado -> String
showDado 0 = "0"
showDado x = show x

-- Função para rotacionar um dado para uma face menor
rotacionaDado :: Int -> Int
rotacionaDado 2 = 1
rotacionaDado 3 = 2
rotacionaDado 4 = 3
rotacionaDado 5 = 4
rotacionaDado 6 = 3 -- A face oposta ao 6 é 1, então a rotação mínima válida é 3
rotacionaDado _ = 1

-- Função para rotacionar um dado no jogo
rotacionaDadoNoJogo :: Jogo -> Int -> Jogo
rotacionaDadoNoJogo [] _ = []
rotacionaDadoNoJogo (0 : xs) n = 0 : rotacionaDadoNoJogo xs (n - 1)
rotacionaDadoNoJogo (dado : xs) 1 = rotacionaDado dado : xs
rotacionaDadoNoJogo (dado : xs) n = dado : rotacionaDadoNoJogo xs (n - 1)

-- Função para substituir um dado por "0" (representando que está finalizado)
substituiPorZero :: Jogo -> Int -> Jogo
substituiPorZero [] _ = []
substituiPorZero (_ : xs) 1 = 0 : xs
substituiPorZero (dado : xs) n = dado : substituiPorZero xs (n - 1)

-- Função para ler um número inteiro do usuário com validação
leInteiro :: String -> IO Int
leInteiro mensagem = do
  putStrLn mensagem
  entrada <- getLine
  case readMaybe entrada of
    Just n -> return n
    Nothing -> do
      putStrLn "Entrada inválida! Tente novamente."
      leInteiro mensagem

-- Função para o jogador fazer sua jogada
jogadaJogador :: Jogo -> IO Jogo
jogadaJogador jogo = do
  pos <- leInteiro "Escolha a posição do dado para jogar (posição iniciando de 1):"
  if pos < 1 || pos > length jogo
    then do
      putStrLn "Posição inválida! Tente novamente."
      jogadaJogador jogo
    else do
      let dado = jogo !! (pos - 1)
      if dado == 0
        then do
          putStrLn "Dado já finalizado! Escolha outra posição."
          jogadaJogador jogo
        else
          if dado == 1
            then return $ substituiPorZero jogo pos
            else return $ rotacionaDadoNoJogo jogo pos

-- Função para encontrar a jogada perfeita no nível difícil
encontraJogadaPerfeita :: Jogo -> Maybe Int
encontraJogadaPerfeita jogo = case [i | (dado, i) <- zip jogo [0 ..], ehVencedor dado] of
  [] -> Nothing
  (x:_) -> Just x

-- Função para determinar se um dado está em uma configuração vencedora
ehVencedor :: Dado -> Bool
ehVencedor 1 = True
ehVencedor 2 = False
ehVencedor 3 = True
ehVencedor 4 = True
ehVencedor 5 = False
ehVencedor 6 = True
ehVencedor _ = False

-- Função para o computador fazer sua jogada no nível difícil
jogadaComputadorDificil :: Jogo -> IO Jogo
jogadaComputadorDificil jogo = case encontraJogadaPerfeita jogo of
  Just idx -> do
    let dado = jogo !! idx
    if dado == 0
      then jogadaComputadorDificil jogo
      else
        if dado == 1
          then return $ substituiPorZero jogo (idx + 1)
          else return $ rotacionaDadoNoJogo jogo (idx + 1)
  Nothing -> jogadaComputadorFacil jogo -- Se não houver jogadas vencedoras, joga aleatoriamente

-- Função para o computador fazer sua jogada no nível fácil
jogadaComputadorFacil :: Jogo -> IO Jogo
jogadaComputadorFacil jogo = do
  idx <- randomRIO (0, length jogo - 1)
  let dado = jogo !! idx
  if dado == 0
    then jogadaComputadorFacil jogo
    else
      if dado == 1
        then return $ substituiPorZero jogo (idx + 1)
        else return $ rotacionaDadoNoJogo jogo (idx + 1)

-- Função para o computador fazer sua jogada conforme o nível de dificuldade
jogadaComputador :: Jogo -> Int -> IO Jogo
jogadaComputador jogo 1 = jogadaComputadorFacil jogo
jogadaComputador jogo 2 = jogadaComputadorDificil jogo
jogadaComputador _ _ = error "Nível de dificuldade inválido"

-- Loop principal do jogo
jogoLoop :: Jogo -> Int -> IO ()
jogoLoop jogo 1 = do
  exibeJogo jogo
  if all (== 0) jogo
    then putStrLn "Parabéns! Você venceu!"
    else do
      novoJogo <- jogadaJogador jogo
      exibeJogo novoJogo
      if all (== 0) novoJogo
        then putStrLn "Parabéns! Você venceu!"
        else do
          putStrLn "Computador jogando..."
          novoJogoComp <- jogadaComputador novoJogo 1
          exibeJogo novoJogoComp
          if all (== 0) novoJogoComp
            then putStrLn "O computador venceu!"
            else jogoLoop novoJogoComp 1

jogoLoop jogo 2 = do
  exibeJogo jogo
  if all (== 0) jogo
    then putStrLn "Parabéns! Você venceu!"
    else do
      putStrLn "Computador jogando..."
      novoJogoComp <- jogadaComputador jogo 2
      exibeJogo novoJogoComp
      if all (== 0) novoJogoComp
        then putStrLn "O computador venceu!"
        else do
          novoJogo <- jogadaJogador novoJogoComp
          exibeJogo novoJogo
          if all (== 0) novoJogo
            then putStrLn "Parabéns! Você venceu!"
            else jogoLoop novoJogo 2

jogoLoop _ _ = putStrLn "Nível de dificuldade inválido. O jogo será encerrado."

-- Função principal para iniciar o jogo
main :: IO ()
main = do
  putStrLn "Bem-vindo ao Jogo dos Dados!"
  dificuldade <- leInteiro "Escolha o nível de dificuldade (1 para fácil, 2 para difícil):"
  if dificuldade /= 1 && dificuldade /= 2
    then putStrLn "Nível de dificuldade inválido. O jogo será encerrado."
    else do
      n <- leInteiro "Digite a quantidade de dados:"
      jogo <- inicializaJogo n
      exibeJogo jogo
      if dificuldade == 2
        then do
          putStrLn "Computador começará a jogar no nível difícil..."
          jogoLoop jogo 2
        else do
          jogoLoop jogo 1
