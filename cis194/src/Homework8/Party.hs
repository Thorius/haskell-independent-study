{-# OPTIONS_GHC -fno-warn-orphans #-}

module Homework8.Party where

import           Data.Monoid
import           Data.Tree
import           Homework8.Employee

-- Exercise 1

glCons :: Employee -> GuestList -> GuestList
glCons e (GL emps fun) = GL (e:emps) (fun + empFun e)

getFun :: GuestList -> Fun
getFun (GL _ fun) = fun

instance Monoid GuestList where
    mempty = GL [] 0
    mappend (GL emps1 fun1) (GL emps2 fun2) = GL (emps1 ++ emps2) (fun1 + fun2)

moreFun :: GuestList -> GuestList -> GuestList
moreFun gl1 gl2
        | gl1 < gl2  = gl2
        | otherwise  = gl1


-- Exercise 2

treeFold :: (a -> [b] -> b) -> b -> Tree a -> b
treeFold f init (Node root leafs) = f root (map (treeFold f init ) leafs )

-- Exercise 3

nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel boss bestGl = (bestGlBoss, bestGlNoBoss)
            where
                glsSubBosses    = map fst bestGl
                glsNoSubBosses  = map snd bestGl
                bestGlBoss      = maximumSafe glsSubBosses
                bestGlNoBoss    = maximumSafe $ map (glCons boss) glsNoSubBosses

maximumSafe :: (Monoid a, Ord a) => [a] -> a
maximumSafe [] = mempty
maximumSafe xs = maximum xs

-- Exercise 4

maxFun :: Tree Employee -> GuestList
maxFun = uncurry max . treeFold nextLevel (mempty, mempty)

-- Exercise 5

main :: IO ()
main = do
    companyStructure <- readFile  "src/Homework8/company.txt"
    putStrLn $ computeOptimalFun companyStructure

computeOptimalFun :: String -> String
computeOptimalFun = formatGl . maxFun . read

formatGl :: GuestList -> String
formatGl (GL emps fun) = show fun ++ '\n' : unlines (map empName emps)
