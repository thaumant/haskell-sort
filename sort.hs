insertionSort :: Ord a => [a] -> [a]
insertionSort xs =
    let sort []     ys = ys
        sort (x:xs) [] = sort xs [x]
        sort (x:xs) ys = sort xs $ insert x ys
        insert x []         = [x]
        insert x ys@(hd:tl) = if x <= hd then x:ys else hd : insert x tl
    in sort xs []


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
