module Main where
import Data.List
import Expression
import Utils
import System.IO
import Control.Monad

allPossibleExpr :: [Float] -> [Expr]
allPossibleExpr xs = foldr (++) []
                    (map (foldr1' (\x -> [x]) frec)
                         (permutations (map (\x -> Const x) xs)))
            where frec = \x rs -> (map (\expr -> Sum x expr) rs) ++
                                  (map (\expr -> Dif x expr) rs) ++
                                  (map (\expr -> Dif expr x) rs) ++
                                  (map (\expr -> Mult x expr) rs) ++
                                  (map (\expr -> Div x expr) rs) ++
                                  (map (\expr -> Div expr x) rs)


exprThatSumsN :: [Float] -> Float -> [Expr]
exprThatSumsN xs n = withoutRepeations (thereIsEquivalent areEquivalents)(filter (\expr -> (exprResult expr) == Just n)
                                      (allPossibleExpr xs))

main =
    do
        putStrLn "Enter first number:"
        tmp <- getLine
        let x1 = (read tmp :: Float)
        putStrLn "Enter second number:"
        tmp <- getLine
        let x2 = (read tmp :: Float)
        putStrLn "Enter third number:"
        tmp <- getLine
        let x3 = (read tmp :: Float)
        putStrLn "Enter fourth number:"
        tmp <- getLine
        let x4 = (read tmp :: Float)
        putStrLn "Enter game's number:"
        tmp <- getLine
        let n = (read tmp :: Float)

        let solutions = exprThatSumsN [x1, x2, x3, x4]  n

        if (null solutions)
            then
                putStrLn "Doesn't have solution!"
            else
                do
                    putStrLn "Has solution! Print solutions? (y or n) "
                    s <- getLine --getChar has a bug
                    if (s == "y")
                        then
                            do
                                forM_ solutions (\expr -> do
                                                            putStrLn (show expr)
                                                            putStrLn "Press enter to continue..."
                                                            wait <- getChar
                                                            return()
                                                            )
                                putStrLn "That's all!"
                        else
                            putStrLn "That's all!"
