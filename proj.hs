pri l = head l
seg l = head (drop 4 l)
ter l = read (drop 8 l) :: Float
trd (_,_,x) = x

getVec [] = []
getVec (x:xs) = zip3 [pri x] [seg x] [ter x] ++ getVec xs

-- isPath [] _ _ = error "error"
-- isPath (x:[]) a b
--   | fst x == a && snd x == b = True
--   | otherwise = False
-- isPath 

imprime [] = return ()
imprime (x:xs) = do
  print x
  imprime xs
main = do
  entrada <- getContents
  let linhas = lines entrada
  let regras = getVec (take (length linhas - 2) linhas)
  let inicial = head (drop (length linhas -2) linhas)
  let final = last linhas
  imprime  regras

