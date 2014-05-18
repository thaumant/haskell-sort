insertionSort :: Ord a => [a] -> [a]
insertionSort xs = foldr insert [] xs
    where insert x []     = [x]
          insert x (y:ys) = if x <= y then x:y:ys else y : insert x ys


selectionSort :: Ord a => [a] -> [a]
selectionSort [] = []
selectionSort xs =
    let leastIdx = iter fst rest
            where (fst:rest) = zip [0..] xs
                  iter (i,x) [] = i
                  iter (i,x) ((i',x'):rest) = iter (if x' < x then (i',x') else (i,x)) rest
        (left, right) = splitAt leastIdx xs
    in head right : selectionSort (left ++ tail right)


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
