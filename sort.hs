insertionSort :: Ord a => [a] -> [a]
insertionSort xs =
    let sort []     ys = ys
        sort (x:xs) [] = sort xs [x]
        sort (x:xs) ys = sort xs $ insert x ys
        insert x []     = [x]
        insert x (y:ys) = if x <= y then x:y:ys else y : insert x ys
    in sort xs []


selectionSort :: Ord a => [a] -> [a]
selectionSort [] = []
selectionSort xs =
    let smallestPos xs =
            let iter p current []     = p
                iter p current (y:ys) = let p' = if y < (xs !! p) then current else p
                                        in  iter p' (current + 1) ys 
            in iter 0 1 (tail xs) 
        (left, right) = splitAt (smallestPos xs) xs
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
