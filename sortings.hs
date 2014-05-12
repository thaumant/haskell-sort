insertionSort :: [Int] -> [Int]
insertionSort xs =
    let sort []     ys = ys
        sort (x:xs) [] = sort xs [x]
        sort (x:xs) ys = sort xs $ insert x ys
        insert x []         = [x]
        insert x ys@(hd:tl) = if x <= hd then x:ys else hd : insert x tl
    in sort xs []