import Data.List (foldl1')

insertionSort :: Ord a => [a] -> [a]
insertionSort xs = foldr insert [] xs
    where insert x []     = [x]
          insert x (y:ys) = if x <= y then x:y:ys else y : insert x ys


selectionSort :: Ord a => [a] -> [a]
selectionSort [] = []
selectionSort xs =
    let minPair xp@(_,x) yp@(_,y) = if x < y then xp else yp
        leastIdx = fst $ foldl1' minPair $ zip [0..] xs
        (left, (least:right)) = splitAt leastIdx xs
    in least : selectionSort (left ++ right)


mergeSort :: Ord a => [a] -> [a]
mergeSort []  = []
mergeSort [x] = [x]
mergeSort xs  =
    let merge [] ys = ys
        merge xs [] = xs
        merge (x:xs) (y:ys) = if x < y then x : merge xs (y:ys)
                                       else y : merge (x:xs) ys
        (left, right) = splitAt (length xs `div` 2) xs
    in merge (mergeSort left) (mergeSort right)
