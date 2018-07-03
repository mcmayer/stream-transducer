module Main where

import           Data.Monoid (mempty, (<>))
import           StreamTrans

sumS :: Num a => Stream a -> a
sumS = foldl'S (+) 0

concatS :: Monoid m => Stream m -> m
concatS = foldl'S (<>) mempty

-- just something to play with
fibo :: Int -> Stream Int
fibo n = Stream step (1,1,n) where
    step (_,_,0) = Done
    step (a,b,i) = Yield a (b, a+b, i-1)

-- | Take numeric difference of stream, drop first el.
diff :: Num a => Stream a -> Stream a
diff (Stream step s0) = Stream step' (s0, Nothing) where
    step' (s, x) = case step s of
        Done     -> Done
        Skip s' -> Skip (s', x)
        Yield a' s' -> case x of
            Nothing -> Skip (s', Just a')
            Just x  -> Yield (a'-x) (s', Just a')

-- | a dumb showList
showListS :: Show a => Stream a -> String
showListS = show . toList

sillyTransf :: (Ord a, Num a) => Stream a -> Stream a
sillyTransf str = aggregate str $ do
    a <- get
    b <- get
    if (b > 1000) then done
    else  yield $ a + b

main :: IO ()
main = do
  let fibStr = fibo 20
  print $ lengthS fibStr
  putStrLn $ "fibo=" <> showListS fibStr
  let fibStr2 = sillyTransf fibStr
  putStrLn $ "silly=" <> showListS fibStr2
  putStrLn $ "diff=" <> showListS (diff fibStr)
