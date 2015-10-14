import Data.Matrix
import Hungarian
import System.Random
import Criterion.Main
import Control.DeepSeq (($!!))

data NamedMatrix a = NMatrix String (Matrix a)

instance (Show a) => Show (NamedMatrix a) where 
        show (NMatrix string matr) = "\n\n" ++ string ++ "\n\n" ++ show matr

main = do 
	putStrLn "Enter a number to seed random generator with mkStdGen, or type 'new' to use newStdGen."
	response <- getLine
	globalStdGen <- newStdGen
	let stdGen = if response == "new"
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
randSquareMatrix lowerLimit upperLimit g x = matrix x x $ \(i,j) -> randomNumbers !! (j + i*x)
                                                where randomNumbers = randomRs (lowerLimit, upperLimit) g

randTestMatrix = randSquareMatrix 0 1000


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