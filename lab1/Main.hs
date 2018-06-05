module Main where
    main::IO()
    main = return()
    sumProduct [] = (0,0)
    sumProduct xs = (sum,product)
        where (sum,product) = foldr(\x (y,n)->(x+y,n*x)) (0,1) xs
 
    foldlStrict f acc []     = acc
    foldlStrict f acc (x:xs) = let acc' = f acc x
	in seq acc' (foldlStrict f acc' xs)
 
   
    perms [] = [[]]
    perms (x:xs) = concat.(map $ \list -> inject x list) $ perms xs
        where
            inject x list = map (\p -> (take p list) ++ [x] ++ (drop p list)) [0..last]
                where
                    last = length list
