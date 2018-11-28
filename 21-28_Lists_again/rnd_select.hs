import System.Random

rnd_select :: [a] -> Int -> StdGen -> [a]
rnd_select l 0 gen = []
rnd_select l n gen = 
    (l !! r) : (rnd_select l (n - 1) nextGen)
        where (r, nextGen) = randomR (0, length l - 1) gen



main = do
    gen <- getStdGen
    putStrLn $ rnd_select "abcdefgh" 3 gen