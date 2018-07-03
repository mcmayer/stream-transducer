{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE InstanceSigs              #-}
{-# LANGUAGE RankNTypes                #-}

module StreamTrans (
    Step(..), Stream(..),
    empty, singleton, fromList, toList, foldlS, foldl'S, lengthS,
    aggregate, get, yield, skip, done
) where

data Step s a where
    Yield :: a -> s -> Step s a
    Skip  :: s -> Step s a
    Done  :: Step s a

instance Functor (Step s) where
    fmap f Done        = Done
    fmap f (Skip s)    = Skip s
    fmap f (Yield a s) = Yield (f a) s


data Stream a = forall s. Stream (s -> Step s a) s

data PreStream s a = PreStream (s -> Step s a) s

instance Functor Stream where
    fmap f (Stream step s0) = Stream step' s0 where
        step' s' = case (step s') of
            Done          -> Done
            Skip s''      -> Skip s''
            Yield a'' s'' -> Yield (f a'') s''

empty :: Stream a
empty = Stream (const Done) ()

singleton :: a -> Stream a
singleton x = Stream step True where
    step True  = Yield x False
    step False = Done

fromList :: [a] -> Stream a
fromList zs = Stream step zs where
    step (x:xs) = Yield x xs
    step []     = Done

foldlS :: (a -> b -> a) -> a -> Stream b -> a
foldlS f a0 (Stream step s) = go a0 s where
    go a s = case step s of
                 Yield b s' -> go (f a b) s'
                 Skip    s' -> go a       s'
                 Done       -> a

foldl'S :: (a -> b -> a) -> a -> Stream b -> a
foldl'S f a0 (Stream step s) = go a0 s where
    go a s = a `seq`
                case step s of
                    Yield b s' -> go (f a b) s'
                    Skip    s' -> go a       s'
                    Done       -> a

toList :: Stream a -> [a]
toList = foldl'S (\as' a'->as' ++ [a']) []

lengthS :: Stream a -> Int
lengthS = foldl'S (\n _ -> n+1) 0

ap :: (Monad m) => m (a -> b) -> m a -> m b
ap mf m = do
    f <- mf
    x <- m
    return (f x)

data Walk a b = Walk ( forall s. (s->Step s a) -> s -> Step s b)

instance Functor (Walk a) where
    fmap f (Walk t) = Walk ( \step s -> fmap f (t step s) )

untilNotSkip :: (s->Step s a) -> s -> Step s a
untilNotSkip step s = case step s of
    Done        -> Done
    Skip s'     -> untilNotSkip step s'
    Yield a' s' -> Yield a' s'

instance Monad (Walk a) where
    return a = Walk (\_ s -> Yield a s)
    Walk t >>= f =
        Walk (\step s -> case t (untilNotSkip step) s of
                Done        -> Done
                Skip _      -> error "Internal error."
                Yield b' s' -> case f b' of Walk t'' -> t'' step s'
        )

instance Applicative (Walk a) where
    pure = return
    (<*>) = ap

yield :: b -> Walk a b
yield = return

skip :: Walk a b
skip = Walk (\_ s->Skip s)

done :: Walk a b
done = Walk (\_ _->Done)

get :: Walk a a
get = Walk (\step s->step s)

aggregate :: Stream a -> Walk a b -> Stream b
aggregate (Stream step s) (Walk t) = Stream (t step) s
