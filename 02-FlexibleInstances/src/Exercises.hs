module Exercises where

class PopQuiz a

-- | Which of the following instances require 'FlexibleInstances'? Don't cheat
-- :D This is a tricky one, but look out for nested concrete types!

instance PopQuiz Bool

-- The following type would require an 'FlexibleInstances' since it is of the
-- form T a, where T = [] and a = Bool; however, a must always be type variable

-- instance PopQuiz [Bool]


instance PopQuiz [a]
instance PopQuiz (a, b)

-- The following type doesn't work either because instnace types must be of the
-- form ''T a1 .. an', but the following has the type 'T1 (T2 a1 .. a2)'
-- instance PopQuiz [(a, b)]

instance PopQuiz (IO a)

newtype RIO  r a = RIO (r -> IO a) -- Remember, this is a /new type/.
type    RIO' r a =      r -> IO a

-- Won't work because has reified type variable
-- instance PopQuiz (RIO Int a)

instance PopQuiz (RIO r a)

-- Won't work because 'RIO' r a' is a type synonym for (->) a b
-- instance PopQuiz (RIO' r a)

-- Won't work because the type is of the form (->) r (IO a) (nested type constructors)
-- instance PopQuiz (r -> IO a)

instance PopQuiz (a -> b) -- We can write (a -> b) as ((->) a b).

-- Won't work because the type is of the form (->) a ((->) b c) (nested type constructors)
-- instance PopQuiz (a -> b -> c)

-- This won't work because the construct (,) is essentially syntactic sugar
-- for a product type pair; suppose we represent this a P t1 t2; then (,,)
-- is of the form P t1 (P t2 t3); since this a nested type, this won't work
-- instance PopQuiz (a, b, c)
-- instance PopQuiz (a, b, c, a)

-- Won't work (nested type constructor)
-- instance PopQuiz (a, (b, c))

instance PopQuiz ()


data Pair  a = Pair  a  a
type Pair' a =      (a, a)

-- Won't work: same type variable appears more than once
-- instance PopQuiz (a, a)

instance PopQuiz (Pair a)

-- Won't work: type synonym
-- instance PopQuiz (Pair' a)
