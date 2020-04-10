module Data.UndefinedOr
    ( UndefinedOr, isUndefined
    , fromUndefined, toUndefined, runUndefined
    ) where

import Prelude
import Data.Maybe (Maybe (Just, Nothing))
import Control.Alt (class Alt)
import Control.Plus (class Plus)


newtype UndefinedOr a = UndefinedOr a

unsafeFromUndefined :: forall a. UndefinedOr a -> a
unsafeFromUndefined (UndefinedOr x) = x

foreign import isUndefined :: forall a. UndefinedOr a -> Boolean
foreign import undefinedVal :: forall a. UndefinedOr a


fromUndefined :: forall a. UndefinedOr a -> Maybe a
fromUndefined u@(UndefinedOr x) =
    if isUndefined u
        then Nothing
        else Just x

toUndefined :: forall a. a -> UndefinedOr a
toUndefined = UndefinedOr

runUndefined :: forall a b. b -> (a -> b) -> UndefinedOr a -> b
runUndefined b f u@(UndefinedOr a) =
    if isUndefined u
        then b
        else f a


instance eqUndefined :: Eq a => Eq (UndefinedOr a) where
    eq ux@(UndefinedOr x) uy@(UndefinedOr y) = case unit of
      _ | isUndefined ux && isUndefined uy -> true
        | not (isUndefined ux) && not (isUndefined uy) -> eq x y
      _ -> false

instance functorUndefined :: Functor UndefinedOr where
    map f u@(UndefinedOr x) =
        if isUndefined u
            then undefinedVal
            else UndefinedOr (f x)

instance applyUndefined :: Apply UndefinedOr where
    apply uf@(UndefinedOr f) ux@(UndefinedOr x) =
        if isUndefined uf || isUndefined ux
            then undefinedVal
            else UndefinedOr (f x)

instance applicativeUndefined :: Applicative UndefinedOr where
    pure = toUndefined

instance altUndefined :: Alt UndefinedOr where
    alt x y =
        if isUndefined x
            then y
            else x

instance plusUndefined :: Plus UndefinedOr where
    empty = undefinedVal

instance showUndefined :: Show a => Show (UndefinedOr a) where
    show u@(UndefinedOr x) =
        if isUndefined u
            then "undefined"
            else show x
