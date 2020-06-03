# Generic validation of nested data

## TLDR

This technique helps validate arbitrary conditions in deeply nested structures without writing
additional code &mdash; with the help of Haskell Geneircs.

It boils down to this pattern:

```haskell ignore
validate :: a 'Raw -> Validation (ErrorMap es) (a 'Valid)
getError :: HasError e es => ErrorMap es -> e
```

If you are interested how to achieve this, read on.

## Introduction

Say you are developing a REST API that accepts some business-data in JSON. Of course, this data
must be checked for correctness. There are actually three levels to this:

- Correctness of JSON _syntax_ &mdash; i.e quotes, brackets and so on;
- Correctness of JSON _schema_ &mdash; i.e objects with required fields, arrays of required item
  type;
- Correctness of _data itself_ &mdash; constraints like "this number should be positive" or "this
  string should have exactly these possible values", that are not expressible in JSON specification
  alone.

First two levels are already handled by `aeson` library and its `FromJSON` typeclass. How could we
incorporate third one there as well?

Let's focus on a simple constraint: positie integers. We could define a newtype with `FromJSON`
instance like this:

```haskell ignore
newtype PositiveInt = PositiveInt Int deriving (Eq, Show)

instance FromJSON PositiveInt where
  parseJSON v = do
    n <- parseJSON v
    if n > 0
      then pure $ PositiveInt n
      else fail "Expected positive integer!"
```
Here we offload all work to existing `Int` parser and then check its result.

At first glance, we're good. However, let's look at the error message in case there are two
`PositiveInt`s in our business type:

```haskell ignore
data Business
  = Business
      { foo :: PositiveInt
      , bar :: PositiveInt
      , baz :: String
      }
  deriving (Eq, Show, Generic)

instance FromJSON Business

wrong :: ByteString
wrong = "{ \"foo\": -42, \"bar\": -256, \"baz\": \"noes\" }"
```

Now what happens when we try to parse it?

```
λ> eitherDecodeStrict @Business wrong
Left "Error in $.foo: Expected positive integer!"
```

We get an error, but it reports only the first field with a problem. There are two improvements we
would like to make over this scenario:

- Collect all validation errors that are present in the data;
- Represent errors with something more structured than strings.

In this post I'll present a possible solution that covers both this points and at the same time is
sufficiently generic in both data and error types.

## About this file

This is a literal Haskell file. Here's how to load in GHCi (adapt for other build tools if not using stack):

```console
$ stack ghci generic-validation.md --package markdown-unlit --package typerep-map --package validation --package containers --ghci-options='-pgmL markdown-unlit'
```

<details class="code-details">
<summary>Some extensions and imports</summary>

```haskell
{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DefaultSignatures          #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE UndecidableSuperClasses    #-}

module Validation where

import Data.Coerce        (coerce)
import Data.Kind
import Data.Map.Strict    (Map, singleton, unionWith)
import Data.Maybe         (mapMaybe)
import Data.Proxy         (Proxy (..))
import Data.Type.Bool
import Data.Type.Equality
import GHC.Generics
import GHC.TypeLits
import Type.Reflection    (Typeable, typeRep)

import           Data.TMap       (TMap)
import qualified Data.TMap       as TMap
import           Data.Validation (Validation (..))
```
</details>

## Representation of the validation process

Simplest way to represent validation would be to define function like this one:

```haskell ignore
validate :: a -> Bool
```

It is obvious that this is not good Haskell &mdash; this function can't guarantee that un-validated
data is not passed to business-logic functions later on.

Instead, we turn to the spirit of the great ["Parse, don't validate"](https://lexi-lambda.github.io/blog/2019/11/05/parse-don-t-validate/)
post. Following it, we must discriminate "raw" and "valid" data in our types. One obvious way is to
use type-level tags through `DataKinds` mechanism:

```haskell
data Validity = Raw | Valid
```

To store errors we must embed our data into some `Functor`. To be able to independently collect
errors from all locations we need an `Applicative`. Standard choice is
[`Data.Validation`](http://hackage.haskell.org/package/validation-1.1/docs/Data-Validation.html).

So, assuming our errors are of some type `e`, we may write this type for validation function:

```haskell ignore
-- Here 'a' has kind "Validity -> Type".
validate :: a 'Raw -> Validation e (a 'Valid)
```

Then, if business logic only accepts `a 'Valid`, then code that glues transport layer and business
layer would have to pattern-match on `Validation` constructors and do some error handling in case
the data was invalid. This ensures that business code never gets invalid data.

But how do we choose this type "`e`"? As we are developing a generic mechanism, we would not like to
limit user's ability to use custom error types. Moreover, we would like to combine validations of
different types in the same data type.

Looks like our `e` has to store values of arbitrary number of unknown types. There is a nice
package that provides just that interface: [`typerep-map`](http://hackage.haskell.org/package/typerep-map).
We can use the `TMap` type defined there. It is a map whose keys are types and values are values of
those types. In other words, it can map `Int` to `42`, `String` to `"foo"` and so on &mdash; all in
one map.

Next take on our `validate`:

```haskell ignore
validate :: a 'Raw -> Validation TMap (a 'Valid)
```

There is still a problem &mdash; now we don't know what types of errors the validation may produce,
but that information should be perfectly recoverable from the type "`a`".

Let's deal with this problem in two steps. First of all, we need to annotate the `TMap` with
type-level tag of types that are actually 100% present in the map. For that we use a `newtype`:

```haskell
newtype ErrorMap (es :: [Type]) = ErrorMap TMap
  deriving (Show)
```

Next, we define a type class for all things that can be validated:

```haskell ignore
class Validable (es :: [Type]) (a :: Validity -> Type) | a -> es where
  validate :: a 'Raw -> Validation (ErrorMap es) (a 'Valid)
```

Crucial moment here is the functional dependency "`a -> es`". It specifices that for every type `a`
there is only one possible set of errors `es`. Using this information GHC will always be able to
infer correct types just from the call to `validate`.

A side note &mdash; to be able to use `Applicative` instance for `Validation` type, our `ErrorMap es`
must have a `Semigroup` intance. And to write such an instance we require each `e` in `es` to be
`Semigroup` as well. The code for the instance is not very interesting:

<details class="code-details">
<summary>instance Semigroup (ErrorMap es)</summary>

```haskell
-- | Match the head of the list of error types and require it to be 'Semigroup' (to combine) and
-- 'Typeable' (to be able to store in a 'TMap')
instance (Semigroup (ErrorMap ts), Semigroup t, Typeable t) => Semigroup (ErrorMap (t ': ts)) where
  (<>) (ErrorMap l) (ErrorMap r) = coerce $ (<>) @(ErrorMap ts) (coerce res) (coerce r)
    where
      -- Here we combine the two values, always push the result into first argument and then
      -- recurse to the rest of types with the second argument unchanged.
      values = mapMaybe (TMap.lookup @t) [l, r]
      -- We can't do just @foldr (<>) mempty values@ because we only have a Semigroup.
      res = case values of
        [] -> l
        (e:[]) -> TMap.insert e l
        (e1:e2:es) -> TMap.insert (e1 <> foldl (<>) e2 es) l

-- | Base case -- both maps should be empty here.
instance Semigroup (ErrorMap '[]) where
  (<>) l _ = l
```
</details>

And lastly, we need a function to extract one type of error from the map:

```haskell
-- HasErrorC will be defined later.
getError :: forall (e :: Type) (es :: [Type]). (Typeable e, HasErrorC e es) => ErrorMap es -> e
getError (ErrorMap em) =
  case TMap.lookup @e em of
    Just x -> x
    Nothing -> error $ "ErrorMap.getError: impossible missing type " <> show (typeRep @e)
```

## Writing `Validable` instances

Let's first make a couple of simple instances for `Validable` type class. These instances will have
to store their errors in an `ErrorMap`, so we introduce `one` as a wrapper of `TMap.one`:

```haskell
one :: Typeable e => e -> ErrorMap '[e]
one e = ErrorMap $ TMap.one e
```


Start with `PositiveInt`s. For
that we must choose how to represent errors. For the purpose of this post a simple `Map` with field
names will suffice:

```haskell
-- Int tagged with its field name.
newtype PositiveInt (field :: Symbol) (v :: Validity) = PositiveInt Int deriving (Eq, Show, Generic)
-- Collect wrong numbers per field name.
newtype PositiveIntErrors = PositiveIntErrors (Map String [Int]) deriving (Eq, Show)

-- To accumulate all wrong numbers in one list.
instance Semigroup PositiveIntErrors where
  (<>) = coerce $ (unionWith @String @[Int]) (<>)

instance KnownSymbol field => Validable '[PositiveIntErrors] (PositiveInt field) where
  validate (PositiveInt n)
    | n > 0 = pure $ PositiveInt n
    | otherwise = Failure $ one $ PositiveIntErrors $ singleton (symbolVal @field Proxy) [n]
```

Let's see how it works:

```haskell
pn1 = PositiveInt @"age" 42
pn2 = PositiveInt @"age" (-5)
pn3 = PositiveInt @"age" (-10)
pn4 = PositiveInt @"height" (-256)
```

```
λ> validate pn1
Success (PositiveInt 42)
λ> let Failure es = validate pn2 in getError @PositiveIntErrors es
PositiveIntErrors (fromList [("age",[-5])])
```

As expected, one number is validated without problems. Now let's try to validate a list of them:

```
λ> let Failure es = traverse validate [pn1, pn2, pn3] *> validate pn4 in getError @PositiveIntErrors es
PositiveIntErrors (fromList [("age",[-5,-10]),("height",[-256])])
```

Notice that our grouping worked: we have separate reports for "age" and "height" fields. And that
all errors are collected.

Now we can write validation for a String with fixed number of possibilities in the same spirit:

```haskell
newtype EnumString (v :: Validity) = EnumString String deriving (Eq, Show, Generic)

instance Validable '[[String]] EnumString where
  validate (EnumString s)
    -- For demonstration we keep the list of errors static, but of course it can be on type-level as
    -- well.
    | s `elem` ["foo", "bar", "baz", "enterprise"] = pure $ EnumString s
    | otherwise = Failure $ one [s]
```

```haskell
es1 = EnumString "foo"
es2 = EnumString "noes"
```

Confirm that it works:
```
λ> let Failure es = traverse validate [es1, es2] in getError @[String] es
["noes"]
```

## Generic validation

Now let's use GHC's `Generic` mechanism to automatically make validators for compound data types. In
our Generic code we would need to change the tag from `'Raw` to `'Valid`, therefore we actually need
a typeclass called `Generic1`, which is the same as `Generic`, but for types of kind `k -> Type`.

First we make a type class `GValidable`, which is `Validable` for Generic data:

```haskell
class GValidable (es :: [Type]) (g :: Validity -> Type) | g -> es where
  gvalidate :: g 'Raw -> Validation (ErrorMap es) (g 'Valid)
```

Here `g` is expected to be `Rep1 a` for some type `a :: Validity -> Type`. Using that, we can add
default implementation for our `Validable` class:

```haskell
class Validable (es :: [Type]) (a :: Validity -> Type) | a -> es where
  validate :: a 'Raw -> Validation (ErrorMap es) (a 'Valid)

  default validate :: (Generic1 a, GValidable es (Rep1 a)) => a 'Raw -> Validation (ErrorMap es) (a 'Valid)
  validate = fmap to1 . gvalidate . from1
```

Next comes the usual Generic stuff: matching on constructors and fields and leveraging already
existing `Validable` instances.

Here's the code, with an explanation for each instance:

```haskell
-- 'Rec1 a' is a field with kind `Validity -> Type`. To validate it, we must turn to its `Validable`
-- instance.
instance (Validable es a) => GValidable es (Rec1 (a :: Validity -> Type)) where
  gvalidate (Rec1 a) = Rec1 <$> validate a

-- ':.:' denotes composition of two types, 'a :: Validity -> Type' and 'f :: Type -> Type'. This
-- instance enables validation of 'GValidable' fields embedded in any 'Traversable' functor like lists.
instance (Traversable f, GValidable es a, Semigroup (ErrorMap es)) => GValidable es (f :.: a) where
  gvalidate (Comp1 as) = Comp1 <$> traverse gvalidate as

-- 'Rec0' is a field of "simple" type, which requires no validation.
instance GValidable '[] (Rec0 (a :: Type)) where
  gvalidate (K1 a) = K1 <$> pure a

-- The next three instances are to unwrap Generic's metadata wrappers for data types and fields.
instance GValidable e a => GValidable e (S1 meta a) where
  gvalidate (M1 a) = M1 <$> gvalidate a

instance GValidable e a => GValidable e (C1 meta a) where
  gvalidate (M1 a) = M1 <$> gvalidate a

instance GValidable e a => GValidable e (D1 meta a) where
  gvalidate (M1 a) = M1 <$> gvalidate a
```

There is only one instance left to do &mdash; the one for `:*:`, data type fields in Generic
representation. But there is a problem: we can't just write it like this:

```haskell ignore
instance (GValidable es a, GValidable es b) => GValidable es (a :*: b) where
```

since such instance will force error types of all fields to be the same. Instead, we would like to
have an union of two independent error type sets. For that we turn to Type Families.

This one will check if error set `es` contains the type `e`:

```haskell
-- O(n) in the length of 'es'.
type family HasError (e :: Type) (es :: [Type]) :: Bool where
  HasError e (e ': _) = 'True
  HasError e (_ ': es) = HasError e es
  HasError e '[] = 'False

-- 'HasError' packed as as 'Constraint'.
type family HasErrorC (e :: Type) (es :: [Type]) :: Constraint where
  HasErrorC e es =
    If (HasError e es)
       ( () :: Constraint )
       (TypeError ('Text "No error of type " :<>: 'ShowType e :<>: 'Text " in the list " :<>: 'ShowType es))
```

And this one will merge two lists of errors, keeping the result set deduplicated:

```haskell
-- O(n*m)
type family MergeErrors (e1 :: [Type]) (e2 :: [Type]) :: [Type] where
  MergeErrors '[] e2 = e2
  MergeErrors (e ': es) e2 =
    If (HasError e e2)
       (MergeErrors es e2)
       (e ': MergeErrors es e2)
```

And now we are ready to make the instance:
```haskell
instance (GValidable es1 a, GValidable es2 b, es3 ~ MergeErrors es1 es2, Semigroup (ErrorMap es3)) => GValidable es3 (a :*: b) where
  gvalidate (l :*: r) = (:*:) <$> coerce (gvalidate @es1 l) <*> coerce (gvalidate @es2 r)
```

The trick is to use `coerce` to "widen" error set of `gvalidate @es1` to `es3`, and the same for
`es2`. Since we define `es3 ~ MergeErrors es1 es2`, we always know that `es1` is a subset of `es3`
and coercion is safe &mdash; we don't lose any data in the map.

## Validation of records

Now it's time to put our Generic implementation to test. Let's define a record using `PositiveInt`s
and `EnumString`s:

```haskell
data Example (v :: Validity)
  = Example
      { age :: PositiveInt "age" v
      , height :: PositiveInt "height" v
      , tags :: [EnumString v]
      }
  deriving (Eq, Show, Generic, Generic1)

deriving instance Validable '[PositiveIntErrors, [String]] Example

ex :: Example 'Raw
ex = Example (PositiveInt (-42)) (PositiveInt (-23)) [EnumString "foo", EnumString "noes", EnumString "lala"]
```

Now we can validate this type and see that errors of all types are collected correctly:
```
λ> let Failure es = validate ex
λ> getError @PositiveIntErrors es
PositiveIntErrors (fromList [("age",[-42]),("height",[-23])])
λ> getError @[String] es
["noes","lala"]
```

## Usage with `aeson`

As I said in the beginning, the purpose of this library is to validate input data in JSON. For that
I propose this workflow:

1. Make `FromJSON` instance for the type with `'Raw` tag:
```haskell ignore
deriving instance FromJSON (Example 'Raw)
```
2. Make `Validable` instance for that type.
3. Use `a 'Valid` in business logic functions.

## Conclusion

We've created a validation framework that is generic both in the types of errors that can be
reported and in the types of data that can be validated.

If this approach gains some intereset, I'll package it as a library and upload to Hackage.
