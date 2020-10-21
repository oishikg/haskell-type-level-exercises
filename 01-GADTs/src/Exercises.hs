{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE LambdaCase        #-}
module Exercises where



{- ONE -}

-- | Let's introduce a new class, 'Countable', and some instances to match.
class Countable a where count :: a -> Int
instance Countable Int  where count   = id
instance Countable [a]  where count   = length
instance Countable Bool where count x = if x then 1 else 0

-- | a. Build a GADT, 'CountableList', that can hold a list of 'Countable'
-- things.

data CountableList where
    ClNil :: CountableList
    ClCons :: (Countable a) => a -> CountableList -> CountableList

-- Some test data to help us
cl0 :: CountableList
cl0 = ClNil

cl1 :: CountableList
cl1 = ClCons [1,2,3] ClNil

cl2 :: CountableList
cl2 = ClCons False ClNil

cl3 :: CountableList
cl3 = ClCons (5 :: Int) ClNil

cl4 :: CountableList
cl4 = ClCons ([1,2,3] :: [Int]) (ClCons (True :: Bool) (ClCons (7 :: Int) ClNil))

-- | b. Write a function that takes the sum of all members of a 'CountableList'
-- once they have been 'count'ed.

countList :: CountableList -> Int
countList = \case
    ClNil -> 0
    ClCons x xs' -> count x + countList xs'


-- | c. Write a function that removes all elements whose count is 0.

dropZero :: CountableList -> CountableList
dropZero = \case
    ClNil -> ClNil
    ClCons x xs' -> if count x == 0
                    then dropZero xs'
                    else ClCons x (dropZero xs')

-- | d. Can we write a function that removes all the things in the list of type
-- 'Int'? If not, why not?

-- Can't: to be able to recognize an int, there must be a guarantee that the count instance
-- of int is unique, and that all c

filterInts :: CountableList -> CountableList
filterInts = error "Contemplate me!"


{- TWO -}

-- | a. Write a list that can take /any/ type, without any constraints.

data AnyList where
    AlNil :: AnyList
    AlCons :: a -> AnyList -> AnyList

-- | b. How many of the following functions can we implement for an 'AnyList'?

reverseAnyList :: AnyList -> AnyList
reverseAnyList = reverseAnyListHelper AlNil
  where
    reverseAnyListHelper :: AnyList -> AnyList -> AnyList
    reverseAnyListHelper acc = \case
        AlNil -> acc
        AlCons x xs' -> reverseAnyListHelper (AlCons x acc) xs'

-- impossible; a is an existential type variable only accessible in the context of
-- AlCons; the filtering function can however only have a fixed type that it takes as an
-- input
filterAnyList :: (a -> Bool) -> AnyList -> AnyList
filterAnyList = undefined

lengthAnyList :: AnyList -> Int
lengthAnyList = \case
    AlNil -> 0
    AlCons _ xs' -> 1 + lengthAnyList xs'

-- impossible; the very notion of folding relies on the fact that all values in the foldable
-- context are of the same type, so that they may be reasonably be folded into some single
-- value of the same type
foldAnyList :: Monoid m => AnyList -> m
foldAnyList = undefined

isEmptyAnyList :: AnyList -> Bool
isEmptyAnyList = \case
    AlNil -> True
    _ -> False

-- Not possible; there is no guarantee that all of the different types, values of which are
-- contained in the list, have a show element
instance Show AnyList where
  show = error "What about me?"

{- THREE -}

-- | Consider the following GADT:

data TransformableTo output where
  TransformWith
    :: (input -> output)
    ->  input
    -> TransformableTo output

-- | ... and the following values of this GADT:

transformable1 :: TransformableTo String
transformable1 = TransformWith show 2.5

transformable2 :: TransformableTo String
transformable2 = TransformWith (uncurry (++)) ("Hello,", " world!")

showTransformation :: TransformableTo output -> output
showTransformation (TransformWith io i) = io i
-- | a. Which type variable is existential inside 'TransformableTo'? What is
-- the only thing we can do to it?

-- input is the existential type variable
-- the only thing we can do with it transform it to the output type

-- | b. Could we write an 'Eq' instance for 'TransformableTo'? What would we be
-- able to check?

-- Not possible; to be able to check for any equality, we would need both the input
-- and outputs to be instances of Eq

-- | c. Could we write a 'Functor' instance for 'TransformableTo'? If so, write
-- it. If not, why not?
instance Functor TransformableTo where
    fmap f (TransformWith io i) = TransformWith (f . io) i

{- FOUR -}

-- | Here's another GADT:

data EqPair where
  EqPair :: Eq a => a -> a -> EqPair

-- Some examples
ep0 :: EqPair
ep0 = EqPair 4 5

ep1 :: EqPair
ep1 = EqPair [1,2,3] [1,2,3]

newtype MyType = MyType Int

-- The following doesn't work since there is no Eq instance declared for MyType
-- ep2 :: EqPair
-- ep2 = EqPair (MyType 4) (MyType 5)

-- | a. There's one (maybe two) useful function to write for 'EqPair'; what is
-- it?
isEqual :: EqPair -> Bool
isEqual (EqPair v1 v2) = v1 == v2

-- and an isNotEqual ?


-- | b. How could we change the type so that @a@ is not existential? (Don't
-- overthink it!)

data EqPairAlt a where
  EqPairAlt :: Eq a => a -> a -> EqPairAlt a

epa0 :: EqPairAlt Int
epa0 = EqPairAlt 3 4

epa1 :: EqPairAlt String
epa1 = EqPairAlt "hello" "hallo"

-- The following doesn't work since there is no Eq instance declared for MyType
-- epa2 :: EqPairAlt MyType
-- epa2 = EqPairAlt (MyType 3) (MyType 5)


-- | c. If we made the change that was suggested in (b), would we still need a
-- GADT? Or could we now represent our type as an ADT?



{- FIVE -}

-- | Perhaps a slightly less intuitive feature of GADTs is that we can set our
-- type parameters (in this case @a@) to different types depending on the
-- constructor.

data MysteryBox a where
  EmptyBox  ::                                MysteryBox ()
  IntBox    :: Int    -> MysteryBox ()     -> MysteryBox Int
  StringBox :: String -> MysteryBox Int    -> MysteryBox String
  BoolBox   :: Bool   -> MysteryBox String -> MysteryBox Bool

-- Examples
mb0 :: MysteryBox ()
mb0 = EmptyBox

mb1 :: MysteryBox Int
mb1 = IntBox 2 EmptyBox

mb2 :: MysteryBox String
mb2 = StringBox "hello" (IntBox 3 EmptyBox)

mb3 :: MysteryBox Bool
mb3 = BoolBox False (StringBox "Hi" (IntBox 4 EmptyBox))


-- | When we pattern-match, the type-checker is clever enough to
-- restrict the branches we have to check to the ones that could produce
-- something of the given type.

getInt :: MysteryBox Int -> Int
getInt (IntBox int _) = int

-- | a. Implement the following function by returning a value directly from a
-- pattern-match:

getInt' :: MysteryBox String -> Int
getInt' (StringBox _ (IntBox x _)) = x

-- | b. Write the following function. Again, don't overthink it!

countLayers :: MysteryBox a -> Int
countLayers = \case
    EmptyBox -> 0
    IntBox _ nextBox -> 1 + countLayers nextBox
    StringBox _ nextBox -> 1 + countLayers nextBox
    BoolBox _ nextBox -> 1 + countLayers nextBox

-- | c. Try to implement a function that removes one layer of "Box". For
-- example, this should turn a BoolBox into a StringBox, and so on. What gets
-- in our way? What would its type be?

-- removeLayer :: MysteryBox a -> MysteryBox a
-- removeLayer = \case
--   EmptyBox -> EmptyBox
--   IntBox _ nextBox -> nextBox
--   StringBox _ nextBox -> nextBox
--   BoolBox _ nextBox -> nextBox

-- The above function does not work because the return type must be the same type as the input
-- ; however we know that with the excpetion of EmptyBox values, all other values
-- wrap a MysteryBox value parameterized by a different type!



{- SIX -}

-- | We can even use our type parameters to keep track of the types inside an
-- 'HList'!  For example, this heterogeneous list contains no existentials:

data HList a where
  HNil  :: HList ()
  HCons :: head -> HList tail -> HList (head, tail)

exampleHList :: HList (String, (Int, (Bool, ())))
exampleHList = HCons "Tom" (HCons 25 (HCons True HNil))

emptyHList :: HList ()
emptyHList = HNil

-- | a. Write a 'head' function for this 'HList' type. This head function
-- should be /safe/: you can use the type signature to tell GHC that you won't
-- need to pattern-match on HNil, and therefore the return type shouldn't be
-- wrapped in a 'Maybe'!
headHList :: HList (head, tail) -> head
headHList (HCons h _) = h

-- test :: HList (Int, String, Bool, ())
-- test = HCons

-- | b. Currently, the tuples are nested. Can you pattern-match on something of
-- type @HList (Int, String, Bool, ())@? Which constructor would work?

patternMatchMe :: HList (Int, String, Bool, ()) -> Int
patternMatchMe = undefined

-- | c. Can you write a function that appends one 'HList' to the end of
-- another? What problems do you run into?

-- The types can't be nested by the type-checker as required


{- SEVEN -}

-- | Here are two data types that may help:

data Empty
data Branch left centre right

-- | a. Using these, and the outline for 'HList' above, build a heterogeneous
-- /tree/. None of the variables should be existential.

data HTree a where
  HLeaf :: HTree Empty
  HBranch :: HTree left -> center -> HTree right -> HTree (Branch left center right)

emptyHTree :: HTree Empty
emptyHTree = HLeaf

oneElementHTree :: HTree (Branch Empty Int Empty)
oneElementHTree = HBranch HLeaf 3 HLeaf

twoElementHTree :: HTree (Branch (Branch Empty String Empty) Int Empty)
twoElementHTree = HBranch (HBranch HLeaf "Hello" HLeaf) 3 HLeaf

sameTwoElementHTree :: HTree (Branch (Branch Empty String Empty) Int Empty)
sameTwoElementHTree = HBranch (HBranch HLeaf "Hello" HLeaf) 3 HLeaf

diffTwoElementHTree :: HTree (Branch (Branch Empty String Empty) Int Empty)
diffTwoElementHTree = HBranch (HBranch HLeaf "Hello" HLeaf) 4 HLeaf

-- | b. Implement a function that deletes the left subtree. The type should be
-- strong enough that GHC will do most of the work for you. Once you have it,
-- try breaking the implementation - does it type-check? If not, why not?
deleteLeftSubtree :: HTree (Branch l c r) -> HTree (Branch Empty c r)
deleteLeftSubtree (HBranch l c r) = HBranch HLeaf c r

-- The implementation will break for an HTree that is a Leaf

-- | c. Implement 'Eq' for 'HTree's. Note that you might have to write more
-- than one to cover all possible HTrees. You might also need an extension or
-- two, so look out for something... flexible... in the error messages!
-- Recursion is your friend here - you shouldn't need to add a constraint to
-- the GADT!
instance Eq (HTree Empty) where
    (==) _ _ = True

instance (Eq (HTree left), Eq center, Eq (HTree right)) => Eq (HTree (Branch left center right)) where
  (==) (HBranch HLeaf c1 HLeaf) (HBranch HLeaf c2 HLeaf) = c1 == c2
  (==) (HBranch l1 c1 r1) (HBranch l2 c2 r2) =
    (c1 == c2) && ((l1 == l2) && (r1 == r2))

-- Test this with the example trees above

-- Note: To write these instance, we need the FlexibleInstances extension to define
-- different @Eq@ instances depending on the @HList@ constructor.
-- We also need the FlexibleContexts extension to use the @HTree@ type constructor




{- EIGHT -}

-- | a. Implement the following GADT such that values of this type are lists of
-- values alternating between the two types. For example:
--
-- @
--   f :: AlternatingList Bool Int
--   f = ACons True (ACons 1 (ACons False (ACons 2 ANil)))
-- @

data AlternatingList a b where
  ANil :: AlternatingList a b
  ACons :: a -> AlternatingList b a -> AlternatingList a b

xs0 :: AlternatingList Bool Int
xs0 = ANil

-- ACons takes a value of type 'a' and a value of type
-- 'AlternatingList b a' and returns a value of type
-- 'AlternatingList a b'; in the following case, the a = Bool,
-- so the list it is being cons-ed to should have type
-- ACons Int Bool; the interesting thing about 'ANil' is that the
-- type variables in its case can take on any values, since the
-- constructor definition does not involve those type variables
xs1 :: AlternatingList Bool Int
xs1 = ACons True ANil

-- The following list is not possible to define since it gives a
-- result of type ''AlternatingList Int Bool'

-- xs2 :: AlternatingList Bool Int
-- xs2 = ACons 22 (ACons True ANil)

-- The following list is possible to define since the outermost
-- 'ACons' constructor takes a 'Bool' and a 'AlternatingList Int Bool'
-- as arguments and gives an 'AlternatingList Bool Int' (as we need)
xs3 :: AlternatingList Bool Int
xs3 = ACons False (ACons 23 (ACons True ANil))

xs4 :: AlternatingList Bool Int
xs4 = ACons True (ACons 24 (ACons False (ACons 23 (ACons True ANil))))



-- | b. Implement the following functions.

getFirsts :: AlternatingList a b -> [a]
getFirsts = \case
  ANil -> []
  ACons a ANil -> [a]
  ACons a (ACons b als) -> a : getFirsts als

getSeconds :: AlternatingList a b -> [b]
getSeconds = \case
  ANil -> []
  ACons a ANil -> []
  ACons a (ACons b als) -> b : getSeconds als

foldableAltList :: AlternatingList String String
foldableAltList = ACons "True" (ACons "24" (ACons "False" (ACons "23" (ACons "True" ANil))))

-- | c. One more for luck: write this one using the above two functions, and
-- then write it such that it only does a single pass over the list.

foldValues :: (Monoid a, Monoid b) => AlternatingList a b -> (a, b)
foldValues as = ( foldMap id (getFirsts as)
                , foldMap id (getSeconds as))

foldValuesOnePass :: (Monoid a, Monoid b)
                  => AlternatingList a b
                  -> (a, b)
foldValuesOnePass = \case
  ANil -> (mempty, mempty)
  ACons a ANil -> (a, mempty)
  ACons a (ACons b als) ->
    let (aAppend, bAppend) = foldValuesOnePass als
    in (aAppend <> a, bAppend <> b)


{- NINE -}

-- | Here's the "classic" example of a GADT, in which we build a simple
-- expression language. Note that we use the type parameter to make sure that
-- our expression is well-formed.

data Expr a where
  Equals    :: Expr Int  -> Expr Int            -> Expr Bool
  Add       :: Expr Int  -> Expr Int            -> Expr Int
  If        :: Expr Bool -> Expr a   -> Expr a  -> Expr a
  IntValue  :: Int                              -> Expr Int
  BoolValue :: Bool                             -> Expr Bool

-- Test expressions
testExp0 :: Expr Int
testExp0 = IntValue 3

testExp1 :: Expr Bool
testExp1 = BoolValue False

testExp2 :: Expr Int
testExp2 = Add testExp0 testExp0

testExp3 :: Expr Bool
testExp3 = Equals (IntValue 4) (IntValue 4)

testExp4 :: Expr Int
testExp4 = If testExp3 testExp2 testExp0


-- | a. Implement the following function and marvel at the typechecker:

eval :: Expr a -> a
eval = \case
  Equals e1 e2 -> eval e1 == eval e2
  Add e1 e2 -> eval e1 + eval e2
  If e1 e2 e3 -> if eval e1 then eval e2 else eval e3
  IntValue x -> x
  BoolValue b -> b

-- | b. Here's an "untyped" expression language. Implement a parser from this
-- into our well-typed language. Note that (until we cover higher-rank
-- polymorphism) we have to fix the return type. Why do you think this is?

data DirtyExpr
  = DirtyEquals    DirtyExpr DirtyExpr
  | DirtyAdd       DirtyExpr DirtyExpr
  | DirtyIf        DirtyExpr DirtyExpr DirtyExpr
  | DirtyIntValue  Int
  | DirtyBoolValue Bool

data IntOrBool = IobInt Int
  | IobBool Bool

-- parse :: DirtyExpr -> Expr IntOrBool
-- parse = \case
--   DirtyIntValue x -> IntValue $ IobInt x
--   DirtyBoolValue b -> BoolValue $ IobBool b
--   DirtyEquals de1 de2 ->
--     let e1 = parse de1
--         e2 = parse de2
--     in Equals e1 e2
--   DirtyAdd de1 de2 -> do
--     let e1 = parse de1
--         e2 = parse de2
--     in Add e1 e2
--   DirtyIf de1 de2 de3 -> do
--     e1 <- parse de1
--     e2 <- parse de2
--     e3 <- parse de3
--     pure $ If e1 e2 e3


-- | c. Can we add functions to our 'Expr' language? If not, why not? What
-- other constructs would we need to add? Could we still avoid 'Maybe' in the
-- 'eval' function?





{- TEN -}

-- | Back in the glory days when I wrote JavaScript, I could make a composition
-- list like @pipe([f, g, h, i, j])@, and it would pass a value from the left
-- side of the list to the right. In Haskell, I can't do that, because the
-- functions all have to have the same type :(

-- | a. Fix that for me - write a list that allows me to hold any functions as
-- long as the input of one lines up with the output of the next.

data TypeAlignedList a b where
  TalNil :: TypeAlignedList b b
  TalCons :: (c -> a) -> TypeAlignedList a b -> TypeAlignedList c b

tal0 :: TypeAlignedList Int Bool
tal0 = TalCons (== 3) TalNil

tal1 :: TypeAlignedList String Bool
tal1 = TalCons
       (\s -> if s == "yes" then 3 else 2)
       (TalCons (== 3) TalNil)

tal2 :: TypeAlignedList Bool String
tal2 = TalCons
       (\b -> if b then "True" else "False")
       TalNil

-- Utility function to convert Tals to functions
talToFunction :: TypeAlignedList a b -> (a -> b)
talToFunction = \case
  TalNil -> id
  TalCons f fs -> talToFunction fs . f

-- | b. Which types are existential?
-- The types that the internal function map to (except the last function)
-- in the list

-- | c. Write a function to append type-aligned lists. This is almost certainly
-- not as difficult as you'd initially think.

composeTALs :: TypeAlignedList b c
            -> TypeAlignedList a b
            -> TypeAlignedList a c
composeTALs bc = \case
  TalNil -> bc
  TalCons f fs -> TalCons f (composeTALs bc fs)
