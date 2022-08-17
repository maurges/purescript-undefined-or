-- Author: d86leader@mail.com
-- License: MIT License; you should have received a copy of the license text
--  with this source file; if you haven't, see:
--  https://github.com/d86leader/purescript-undefined-or/blob/master/LICENSE

-- | A lot of JS functions accept and return records where some fields may be
-- | missing. For missing fields in argument records you can use `Data.Options`
-- | module. For missing fields in return records you can either use `Foreign`
-- | and write parsers, or this module and trust the foreign code.
-- |
-- | A simple example of usage:
-- | ```
-- | // Test.js
-- | exports.typicalApi = function() {
-- |   if (Math.random() > 0.5) {
-- |     return {"numberData": 9431, "stringData": "atad"};
-- |   } else {
-- |     return {"numberData": 8752}
-- |   }
-- | }
-- |
-- | -- Test.purs
-- | type ApiRet = {numberData :: Boolean, stringData :: UndefinedOr String}
-- |
-- | foreign import typicalApi :: Effect ApiRet
-- |
-- | ...
-- |
-- | result <- typicalApi
-- | case fromUndefined result.stringData of
-- |     Just data -> Console.log $ "got a string: " <> data
-- |     Nothing   -> Console.log "no string"
-- | ```
-- |
-- | You should avoid using this library whenever possible, as it's a shortcut:
-- | it lifts the neccessity of data validation from you, the ffi-bindings
-- | writer, to the user of your api. Also 75% of the use cases can be covered
-- | by clever use of typeclasses.
-- |
-- | You should especially Not use this over Maybe.
-- |
-- | Please also note that this (althoug it seems like a instance of) is not Functor
-- | The implementation does not obey the functor-laws 
-- | (see the [discussion in this issue](https://github.com/d86leader/purescript-undefined-or/issues/4))
-- | For convenience this module exposes similary named functions as instances for `Functor`, `Apply`
-- | `Applicative`, `Alt` and `Plus` would.
module Data.UndefinedOr
  ( UndefinedOr
  , isUndefined
  , fromUndefined
  , toUndefined
  , runUndefined
  , map
  , apply
  , pure
  , alt
  , empty
  ) where

import Prelude
import Data.Maybe (Maybe(Just, Nothing))

-- | Wrapper for foreign values which may be undefined.
-- |
-- | Caution: it may misbehave if the wrapped value is `null` or `undefined`.
-- |
-- | All instances are the same as `Maybe`'s instances. The missing Bind, Monad
-- | and other advanced ones are omitted on purpose. If you want to use them,
-- | run `fromUndefined` and use Maybe's instances.
newtype UndefinedOr a = UndefinedOr a

-- | Check if the value is present
foreign import isUndefined :: forall a. UndefinedOr a -> Boolean
foreign import undefinedVal :: forall a. UndefinedOr a

-- | Convert to `Maybe`, returning `Nothing` if the value is missing
fromUndefined :: forall a. UndefinedOr a -> Maybe a
fromUndefined u@(UndefinedOr x) =
  if isUndefined u then Nothing
  else Just x

-- | Wrap a value. Useful for equality checks with a foreign value without
-- | having to unwrap it.
-- |
-- | Is a synonym for `pure`. Use that or this function based on the style
-- | preference or if the name will disambiguate intent.
toUndefined :: forall a. a -> UndefinedOr a
toUndefined = UndefinedOr

-- | Like `maybe` but for undefined
runUndefined :: forall a b. b -> (a -> b) -> UndefinedOr a -> b
runUndefined b f u@(UndefinedOr a) =
  if isUndefined u then b
  else f a

instance eqUndefined :: Eq a => Eq (UndefinedOr a) where
  eq ux@(UndefinedOr x) uy@(UndefinedOr y) = case unit of
    _
      | isUndefined ux && isUndefined uy -> true
      | not (isUndefined ux) && not (isUndefined uy) -> eq x y
    _ -> false

-- | unlawful version of `map` - caution does not obey the functor laws
map :: forall a b. (a -> b) -> UndefinedOr a -> UndefinedOr b
map f u@(UndefinedOr x) =
  if isUndefined u then undefinedVal
  else UndefinedOr (f x)

-- | unlawful version of `apply` - caution does not obey the functor laws
apply :: forall a b. UndefinedOr (a -> b) -> UndefinedOr a -> UndefinedOr b
apply uf@(UndefinedOr f) ux@(UndefinedOr x) =
  if isUndefined uf || isUndefined ux then undefinedVal
  else UndefinedOr (f x)

-- | alias for `toUndefined`
pure :: forall a. a -> UndefinedOr a
pure = toUndefined

-- | unlawful version of `alt` - caution does not obey the functor laws
alt :: forall a. UndefinedOr a -> UndefinedOr a -> UndefinedOr a
alt x y =
  if isUndefined x then y
  else x

-- | alias for `undefinedVal`
empty :: forall a. UndefinedOr a
empty = undefinedVal

instance showUndefined :: Show a => Show (UndefinedOr a) where
  show u@(UndefinedOr x) =
    if isUndefined u then "undefined"
    else show x
