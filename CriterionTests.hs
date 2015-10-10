import Data.Matrix
import Hungarian
import System.Random
import Criterion.Main
import Control.DeepSeq (($!!))

data NamedMatrix a = NMatrix String (Matrix a)

instance (Show a) => Show (NamedMatrix a) where 
        show (NMatrix string matr) = "\n\n" ++ string ++ "\n\n" ++ show matr

main = do 
	putStrLn "Enter a number to seed random generator, or type 'get' to use getStdGen."
	response <- getLine
	globalStdGen <- getStdGen
	let stdGen = if response == "get"
			then globalStdGen
			else mkStdGen (read response :: Int)
	putStrLn "Run benchmarks up to what nxn size matrix?"
        size <- getLine
        let matrixSize = (read size :: Int)
        putStrLn $ show $ generateSolvedRandomMatrices matrixSize stdGen
        defaultMain [
			bgroup "hungarianMin" $ generateBenchmarks matrixSize stdGen
	            ]


randSquareMatrix :: Int -> Int -> StdGen -> Int -> Matrix Int
randSquareMatrix lowerLimit upperLimit g x = matrix x x $ \(i,j) -> (randomRs (lowerLimit, upperLimit) g) !! (j + i*x)

randTestMatrix = randSquareMatrix 0 1000

nthStdGen :: Int -> StdGen -> StdGen
nthStdGen x g = if x < 0
                        then g
			else nthStdGen (x - 1) $ snd $ next g

generateBenchmarks :: Int -> StdGen -> [Benchmark]
generateBenchmarks 0 _ = []
generateBenchmarks x g = generateBenchmarks (x-1) (snd (next g)) ++ [bench (show x ++ "x" ++ show x) $ (whnf hungarianMin) $!! randTestMatrix g x]

generateSolvedRandomMatrices :: Int -> StdGen -> [NamedMatrix Int]
generateSolvedRandomMatrices 0 _ = []
generateSolvedRandomMatrices x g = 
        let randName = "Random " ++ sizeStr x ++ " Matrix"
            randMatrix = randTestMatrix g x
            solvedName = "Solved Random " ++ sizeStr x ++ " Matrix"
            solvedMatrix = hungarianMin randMatrix
            in generateSolvedRandomMatrices (x-1) (snd (next g)) ++ NMatrix randName randMatrix : [NMatrix solvedName solvedMatrix]

sizeStr :: Int -> String
sizeStr x = show x ++ "x" ++ show x