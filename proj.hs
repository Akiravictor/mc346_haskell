pri l = head l
seg l = head (drop 4 l)
ter l = read (drop 8 l) :: Float

getVec [] = []
getVec (x:xs) = zip3 [pri x] [seg x] [ter x] ++ getVec xs

main = do
  entrada <- getContents
  let linhas = lines entrada
  let regras = getVec (take (length linhas - 2) linhas)
  let inicial = head (drop (length linhas -2) linhas)
  let final = last linhas
  print regras
  print inicial
  print final

