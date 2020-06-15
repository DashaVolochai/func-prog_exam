-- Polymorphic sorting
sort :: Ord a => [a] -> [a]
sort xs = inserts xs []

-- Insert given elements in an emerging result
inserts :: Ord a => [a] -> [a] -> [a]
inserts [] r = r
inserts [x] r = r
inserts (x1:x2:xs) r = inserts (x2:xs) (insert x1 x2 r)

-- Insert a given element in a list
insert :: Ord a => a -> a -> [a] -> [a]
insert x1 x2 [] =
  if x1 == x2
    then [x1]
    else []
insert x1 x2 (y:ys) =
  if x1 == x2
    then (x1:y:ys)
    else (y:ys)

main = do
  let input = [2,2,4,3,3,3,3,3,1,5,5,4,8,9,9]
  print input
  print $ sort input