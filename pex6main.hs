-- pex6.hs 
-- unKnot Haskell

-- name: Morgen Barrozo

{- DOCUMENTATION: I used this website to help me understand how to write a function in Haskell specifically pattern matching and
recursion. I also learned how to use the /= operator on this website which I used in several of my functions. https://www.tutorialspoint.com/haskell/haskell_functions.htm. 
I also used this website to help me code more complex pattern matching functions for lists. https://piembsystech.com/mastering-pattern-matching-in-haskell-programming-language/#google_vignette.
I also used the pre-reading and powerpoint presentations from the course (Lessons 26-29).
-}


type1 :: [(Char, Char)] -> [(Char, Char)]
type1 [] = []
type1 xs 
   | fst(head xs) == fst(head(tail xs)) = tail(tail xs)
   | otherwise = head xs: type1(tail xs)


match :: Char -> Char -> Char -> Char -> Bool
match n1 n2 n3 n4 =
  (n1 == n3 && n2 == n4) || (n1 == n4 && n2 == n3)


dropsecond :: Char -> Char -> Char -> [(Char, Char)] -> [(Char, Char)]
dropsecond n1 n2 k1 [] = []
dropsecond n1 n2 k1 ((n3, k3):(n4, k4):xs) = 
   if k3 == k4 && k3 /= k1 && match n1 n2 n3 n4 
      then xs
      else (n3, k3) : dropsecond n1 n2 k1 xs


type2 ::[(Char, Char)] -> [(Char, Char)]
type2 [] = []
type2 ((n1, k1):(n2,k2):xs) =
   if k1 == k2 && dropsecond n1 n2 k1 xs /= xs
      then dropsecond n1 n2 k1 xs
      else (n1, k1) : type2((n2, k2):xs)


step :: [(Char, Char)] -> [(Char, Char)]
step xs = 
   if null xs
      then xs
      else
         if type1 xs /= xs
            then type1 xs
            else
               if type2 xs /= xs
                  then type2 xs
                  else xs


unKnot :: [(Char, Char)] -> String
unKnot tripCode
  | null tripCode = "not a knot"
  | step tripCode == tripCode =
      "tangle – resulting trip code: " ++ show tripCode
  | otherwise = unKnot (step tripCode)


main :: IO ()
main = do
   let t01 = [('a','o'),('a','u')]
   print("   test case t01 - tripcode: " )
   print(t01)
   print("   result:" ++ unKnot t01)

