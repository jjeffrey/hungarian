import Data.Matrix
import Hungarian
import System.Random
import Criterion.Main
import Control.DeepSeq (($!!))


main = do 
	putStrLn "Enter a number to seed random generator, or type 'get' to use getStdGen."
	response <- getLine
	globalStdGen <- getStdGen
	let stdGen = if response == "get"
			then globalStdGen
			else mkStdGen (read response :: Int)
	putStrLn "Run benchmarks up to what nxn size matrix?"
        size <- getLine
        defaultMain [
			bgroup "hungarianMin" $ generateBenchmarks (read size :: Int) stdGen
	            ]


randSquareMatrix :: Int -> Int -> StdGen -> Int -> Matrix Int
randSquareMatrix lowerLimit upperLimit g x = matrix x x $ \(i,j) -> (randomRs (lowerLimit, upperLimit) g) !! (j + i*x)

randTestMatrix = randSquareMatrix 0 1000

nthStdGen :: Int -> StdGen -> StdGen
nthStdGen x g = if x < 0
                        then g
			else nthStdGen (x - 1) $ snd $ next g

generateBenchmarks :: Int -> StdGen -> [Benchmark]
generateBenchmarks 0 g = []
generateBenchmarks x g = generateBenchmarks (x-1) (snd (next g)) ++ [bench (show x ++ "x" ++ show x) $ (whnf hungarianMin) $!! randTestMatrix g x]