{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE GADTs          #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
module Exercises where

import Data.Kind (Type, Constraint)
import Data.Function ((&))





{- ONE -}

-- | One of the restrictions around classes that we occasionally hit is that we
-- can only have one instance for a type. There are, for example, two good
-- candidates for a monoid instance when we think about 'Integer':

data IntegerMonoid = Sum | Product

-- Because of the DataKinds extension, the 'IntegerMonoid' type can also be promoted to the kind level;
--'Sum and 'Product are the corresponding type level constructors of kind Natural


-- | a. Write a newtype around 'Integer' that lets us choose which instance we
-- want.

newtype SumOrProductInteger (m :: IntegerMonoid) = SumOrProductInteger Int deriving (Show)

-- | b. Write the two monoid instances for 'Integer'.

-- Monoid instance when we would like the integers to be summed

instance Semigroup (SumOrProductInteger 'Sum) where
  (<>) (SumOrProductInteger x1) (SumOrProductInteger x2) = SumOrProductInteger (x1 + x2)

instance Monoid (SumOrProductInteger 'Sum) where
  mempty = SumOrProductInteger 0

-- Test this instance
sumXs :: [SumOrProductInteger 'Sum]
sumXs = [SumOrProductInteger 3, SumOrProductInteger 4, SumOrProductInteger 5]

-- foldMap id sumXs =  SumOrProductInteger 12

-- Monoid instance when integers are to be multiplied

instance Semigroup (SumOrProductInteger 'Product) where
  (<>) (SumOrProductInteger x1) (SumOrProductInteger x2) = SumOrProductInteger (x1 * x2)

instance Monoid (SumOrProductInteger 'Product) where
  mempty = SumOrProductInteger 1

-- Test this instance
productXs :: [SumOrProductInteger 'Product]
productXs = [SumOrProductInteger 3, SumOrProductInteger 4, SumOrProductInteger 5]

-- foldMap id productXs =  SumOrProductInteger 60


-- | c. Why do we need @FlexibleInstances@ to do this?

-- Because we are defining multiple instances for the same type





{- TWO -}

-- | We can write a type that /is/ of kind 'Type', but has no value-level
-- members. We usually call this type 'Void':

data Void -- No constructors!

-- | a. If we promote this with DataKinds, can we produce any /types/ of kind
-- 'Void'?

-- No, it seems in fact that there are no types of kind Void?

-- | b. What are the possible type-level values of kind 'Maybe Void'?

-- 'Nothing :: Maybe Void
-- 'Just Void :: Maybe Void (?)

-- | c. Considering 'Maybe Void', and similar examples of kinds such as
-- 'Either Void Bool', why do you think 'Void' might be a useful kind?

-- (?)




{- THREE -}

-- | a. Write a GADT that holds strings or integers, and keeps track of how
-- many strings are present. Note that you might need more than 'Nil' and
-- 'Cons' this time...

data Nat = Z | S Nat

data StringAndIntList (stringCount :: Nat) where
  SailNil :: StringAndIntList 'Z
  SailIntCons :: Int -> StringAndIntList n -> StringAndIntList n
  SailStringCons :: String -> StringAndIntList n -> StringAndIntList ('S n)

-- | b. Update it to keep track of the count of strings /and/ integers.

data StringAndIntListNew (intCount :: Nat) (stringCount :: Nat) where
  SailnNil :: StringAndIntListNew 'Z 'Z
  SailnIntCons :: Int -> StringAndIntListNew iCount sCount -> StringAndIntListNew ('S iCount) sCount
  SailnStringCons :: String -> StringAndIntListNew iCount sCount -> StringAndIntListNew iCount ('S sCount)


-- | c. What would be the type of the 'head' function?

-- How would you even define a head function? The issue is, we don't know whether we have a string or an
-- integer at the head of the list; all we have is a count of integers and the strings; you would need
-- a sum type that wraps both integer and string values




{- FOUR -}

-- | When we talked about GADTs, we discussed existentials, and how we could
-- only know something about our value if the context told us:

data Showable where
  Showable :: Show a => a -> Showable

-- | a. Write a GADT that holds something that may or may not be showable, and
-- stores this fact in the type-level.

data MaybeShowable (isShowable :: Bool) where
  IsShowable :: (Show a) => a -> MaybeShowable 'True
  NotShowable :: a -> MaybeShowable 'False

-- Example of showable data
showableValue :: MaybeShowable 'True
showableValue = IsShowable 3

-- Example of non-showable data
data MyType = MyType
notShowableValue :: MaybeShowable 'False
notShowableValue = NotShowable MyType

-- The following won't work because we attempt to pass 'Showable' a non-showable value
-- wontWork :: MaybeShowable 'True
-- wontWork = NotShowable MyType

-- | b. Write a 'Show' instance for 'MaybeShowable'. Your instance should not
-- work unless the type is actually 'show'able.

instance Show (MaybeShowable 'True) where
  show (IsShowable s) = show s

-- | c. What if we wanted to generalise this to @Constrainable@, such that it
-- would work for any user-supplied constraint of kind 'Constraint'? How would
-- the type change? What would the constructor look like? Try to build this
-- type - GHC should tell you exactly which extension you're missing.

data ConstrainedType (meetsConstraint :: Bool) (constraint :: Constraint) where
  IsConstrained :: constraint => a -> ConstrainedType 'True constraint
  NotConstrained :: a -> ConstrainedType 'False constraint

testConstrainedType :: ConstrainedType 'True (Show Int)
testConstrainedType = IsConstrained 3

testConstrainedTypeNotConstrained :: ConstrainedType 'False (Show Int)
testConstrainedTypeNotConstrained = NotConstrained MyType


{- FIVE -}

-- | Recall our list type:

data List a = Nil | Cons a (List a)

-- Let us see what happens when we promote the type to a kind
-- Kind: List Type
-- Types with this kind:
-- 'Nil = List Type
-- 'Cons = Type -> List Type -> List Type

-- | a. Use this to write a better 'HList' type than we had in the @GADTs@
-- exercise. Bear in mind that, at the type-level, 'Nil' and 'Cons' should be
-- "ticked". Remember also that, at the type-level, there's nothing weird about
-- having a list of types!

data HList (types :: List Type) where
  HNil  :: HList 'Nil
  HCons :: head -> HList tail -> HList ('Cons head tail)

-- Some example HLists
emptyHlist :: HList 'Nil
emptyHlist = HNil

someHlist :: HList ('Cons Int ('Cons String 'Nil))
someHlist = HCons 3 (HCons "Hello" HNil)

-- | b. Write a well-typed, 'Maybe'-less implementation for the 'tail' function
-- on 'HList'.

tailHList :: HList ('Cons head tail) -> HList tail
tailHList (HCons _ xs) = xs

-- | c. Could we write the 'take' function? What would its type be? What would
-- get in our way?

-- No; we wouldn't be able to implement the general take function, because we have no information
-- about what the integer length might be, and so have no way of determining the return type



{- SIX -}

-- | Here's a boring data type:

data BlogAction
  = AddBlog
  | DeleteBlog
  | AddComment
  | DeleteComment

-- | a. Two of these actions, 'DeleteBlog' and 'DeleteComment', should be
-- admin-only. Extend the 'BlogAction' type (perhaps with a GADT...) to
-- express, at the type-level, whether the value is an admin-only operation.
-- Remember that, by switching on @DataKinds@, we have access to a promoted
-- version of 'Bool'!

-- First we define a sum type to capture the types of action
data ActionAccess = OnlyAdminAccess | OpenAccess

-- Now we define a safe type
data BlogActionSafe (access :: ActionAccess) where
   AddBlogSafe :: BlogActionSafe 'OpenAccess
   DeleteBlogSafe :: BlogActionSafe 'OnlyAdminAccess
   AddCommentSafe :: BlogActionSafe 'OpenAccess
   DeleteCommentSafe :: BlogActionSafe 'OnlyAdminAccess


-- | b. Write a 'BlogAction' list type that requires all its members to be
-- the same "access level": "admin" or "non-admin".

data BlogActionList (isSafe :: ActionAccess) where
  BalNil :: BlogActionList access
  BalCons :: BlogActionSafe access -> BlogActionList access -> BlogActionList access

-- Let's try and define some BlogAction lists
validBlogActionListAdmin :: BlogActionList OnlyAdminAccess
validBlogActionListAdmin = BalCons DeleteCommentSafe (BalCons DeleteBlogSafe BalNil)

validBlogActionListUser :: BlogActionList OpenAccess
validBlogActionListUser = BalCons AddBlogSafe (BalCons AddCommentSafe BalNil)

-- Note that the following won't work
-- invalidBlogActionList :: BlogActionList OpenAccess
-- invalidBlogActionList = BalCons DeleteCommentSafe BalNil

-- | c. Let's imagine that our requirements change, and 'DeleteComment' is now
-- available to a third role: moderators. Could we use 'DataKinds' to introduce
-- the three roles at the type-level, and modify our type to keep track of
-- this?





{- SEVEN -}

-- | When we start thinking about type-level Haskell, we inevitably end up
-- thinking about /singletons/. Singleton types have a one-to-one value-type
-- correspondence - only one value for each type, only one type for each value.
-- A simple example is '()', whose only value is '()'. 'Bool' is /not/ a
-- singleton, because it has multiple values.

-- We can, however, /build/ a singleton type for 'Bool':

data SBool (value :: Bool) where
  SFalse :: SBool 'False
  STrue  :: SBool 'True

-- | a. Write a singleton type for natural numbers:

data SNat (value :: Nat) where
  SNil :: SNat 'Z
  SCons :: SNat n -> SNat ('S n)

-- | Observe how each type corresponds to a unique value, and
-- vice-versa
sNat0 :: SNat 'Z
sNat0 = SNil

sNat1 :: SNat ('S 'Z)
sNat1 = SCons SNil

sNat2 :: SNat ('S ('S 'Z))
sNat2 = SCons (SCons SNil)

-- | b. Write a function that extracts a vector's length at the type level:

vectorLength :: Vector n a -> SNat n
vectorLength VNil = SNil
vectorLength (VCons _ xs') = SCons (vectorLength xs')

-- | c. Is 'Proxy' a singleton type?

data Proxy a = Proxy

-- No, because the Proxy type has only one data value, Proxy, but maps
--  to an infinite number of types 



{- EIGHT -}

-- | Let's imagine we're writing some Industry Haskell™, and we need to read
-- and write to a file. To do this, we might write a data type to express our
-- intentions:

data Program result isOpen
  = OpenFile (Program result 'False)
  | WriteFile String (Program result 'True)
  | ReadFile (String -> Program result 'True)
  | CloseFile (Program result 'True)
  | Exit result 

-- | We could then write a program like this to use our language:

-- myApp :: Program Bool
-- myApp
--   = OpenFile $ WriteFile "HEY" $ (ReadFile $ \contents ->
--       if contents == "WHAT"
--         then WriteFile "... bug?" $ Exit False
--         else CloseFile            $ Exit True)

-- | ... but wait, there's a bug! If the contents of the file equal "WHAT", we
-- forget to close the file! Ideally, we would like the compiler to help us: we
-- could keep track of whether the file is open at the type level!
--
-- - We should /not/ be allowed to open a file if another file is currently
-- open.
--
-- - We should /not/ be allowed to close a file unless a file is open.
--
-- If we had this at the type level, the compiler should have been able to tell
-- us that the branches of the @if@ have different types, and this program
-- should never have made it into production. We should also have to say in the
-- type of 'myApp' that, once the program has completed, the file will be
-- closed.

-- | Improve the 'Program' type to keep track of whether a file is open.  Make
-- sure the constructors respect this flag: we shouldn't be able to read or
-- write to the file unless it's open. This exercise is a bit brain-bending;
-- why? How could we make it more intuitive to write?

-- | EXTRA: write an interpreter for this program. Nothing to do with data
-- kinds, but a nice little problem.

interpret :: Program a isOpen -> IO a
interpret = error "Implement me?"





{- NINE -}

-- | Recall our vector type:

data Vector (n :: Nat) (a :: Type) where
  VNil  :: Vector 'Z a
  VCons :: a -> Vector n a -> Vector ('S n) a

-- | Imagine we want to write the '(!!)' function for this vector. If we wanted
-- to make this type-safe, and avoid 'Maybe', we'd have to have a type that can
-- only hold numbers /smaller/ than some type-level value.

-- | a. Implement this type! This might seem scary at first, but break it down
-- into Z and S cases. That's all the hint you need :)

data SmallerThan (limit :: Nat) where
  -- ...

-- | b. Write the '(!!)' function:

(!!) :: Vector n a -> SmallerThan n -> a
(!!) = error "Implement me!"

-- | c. Write a function that converts a @SmallerThan n@ into a 'Nat'.
