{-# LANGUAGE DeriveDataTypeable, FlexibleInstances, OverlappingInstances #-}
{-# LANGUAGE NoMonomorphismRestriction, UndecidableInstances, RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Matchers based on the 'Data' type class defined in @Data.Data@.
--
-- Examples below are based on a sample data type defined as
-- @data MyType = First | Second Int | Third String Bool deriving(Data,Show)@.

module Control.Rematch.Data (Constrable, isDataConstr, isDataValue) where

import Data.Data
import Control.Rematch

-- | class Constrable allows us to convert an algebraic data type constructor
-- function (e.g. 'Maybe') to a 'Constr' object that represents the values
-- it can create. Based on code provided by Tikhon Jelvis in a Stack Overflow
-- answer (see http://stackoverflow.com/a/25588663/441899)
class Constrable a where
  constr :: a -> Constr

instance Data a => Constrable a where
  constr = toConstr

instance Constrable a => Constrable (b -> a) where
  constr f = constr (f undefined)

-- | Match if and only if a value was constructed using a specific constructor
-- version, e.g. @isDataConstr Second@ produces a matcher for @MyType@ that
-- accepts values produced via @Second@ but rejects those produced via @First@
-- or @Third@
isDataConstr :: (Data d, Constrable c) => c -> Matcher d
isDataConstr f = Matcher match
                         ("value with constructor " ++ show expectedC)
                         (\v -> "had constructor " ++ (show $ toConstr v))
                 where
                   expectedC = constr f
                   match v = toConstr v == expectedC

-- | Match if and only if a value was specified using a specific constructor
-- and contains an argument at a specified zero-based index.  For example,
-- @isDataValue Second 0 (equalTo (2::Int))@ is a matcher for @MyType@ that will
-- first check that the value was constructed via the @Second@ constructor, then
-- that the type of value contained is an 'Int', and finally that it is equal to
-- 3.
--
-- Note that types of contained values are checked *dynamically* not
-- *statically*, as the 'Data' type class does not provide static type
-- information about constructor arguments.  For this reason, when checking
-- against literals that may have multiple types, it is important to
-- specify their types explicitly, otherwise they may be defaulted to the
-- wrong the type.
isDataValue :: forall c v d . (Data d, Constrable c, Data v, Show d) =>
               c -> Int -> Matcher v -> Matcher d
isDataValue f i argMatch =
    Matcher doMatch descr standardMismatch
    where
      expectedC = constr f
      descr = "value with constructor " ++ (show expectedC) ++ " with "
              ++ (description argMatch) ++ " at index " ++ (show i)
      doMatch v = toConstr v == expectedC &&
                gmapQi i doSubmatch v
      doSubmatch :: forall ad . Data ad => ad -> Bool
      doSubmatch av = case (cast av) :: Maybe v of
                        Just avx -> (match argMatch) avx
                        Nothing  -> False


-- FIXME need tests for both matchers defined in this module
