---
title: Duck-typing with GHC Generics
draft: false
description: An exploration of structural typing in a nominal world
tags: haskell, generics, GHC
---

If you come to Haskell from a more run-of-the-mill programming background, some
of the terminology can be downright obscure and cryptic. One piece of the
Haskell (or specifically GHC) land-scape that is more confusing than cryptic is
the [GHC Generics][1] system. Here I'm going to go through what this is, why you
might want to use it, and how it can let you do things that would otherwise
appear impossible within the constraints of the Haskell type system.

Generics /= Parametric Polymorphism
===========================

Unfortunately, the term 'Generics' is rather overloaded in the software
development community. Starting with Java, the term has spread to many language
communities (including perhaps the most influential and widespread type system
in use today, TypeScript) with the meaning of 'parametric polymorphism', i.e.
the ability to construct types that have one or more type parameters, and are
thus generic in the sense that one structure can be inhabited by many different
types. Examples of this ubiquitious in Haskell, including all the common
suspects such as `[a]`, `Maybe a`, `Tree a`, `Map k v`, `(a,b)` and even `IO a`.

So given these two types:

```haskell
data A = A Int Bool Char
data B a b c = B a b c
```

`A` is monomorphic, i.e. it has only one inhabitant, and `B` is polymorphic, and
can be inhabited by all kinds of different things, such as `B Word Word Word`,
`B String (Set Int) (IO ())` etc, even `B Int Bool Char`, which is isomorphic to
`A`. We can also say that `A` is of kind `*` and `B` is of kind `* -> * -> * ->
*`.

But even though `A` and `B Int Bool Char` have the same shape (1 constructor, 3
positional fields) and each of the their fields has the same shape, they are
distinct types, and we cannot directly write a single function that can, say,
extract the second position field as a `Bool` from either an `A` or a `B Int
Bool Char`, not to mention a `B a Int b`, without writing some kind of type class.

Compare this with dynamically typed languages like Python, or Clojure, where it
is straightforward to write such functions, that manipulate objects in terms of
their structure, not their names:

```python
def foo(x):
  print x.foo
```

```clojure
(defn foo [x]
  (print (:foo x)))
```

This different approach to data-types, so called Structural typing, as opposed
to nominal typing, is often referred to as _Duck Typing_, as in if it quacks
like a duck, and looks like a duck, then to all intents and purposes, it is a
duck. In structural typing terms, if it has a field of that name, and that field
has that type, then as far as we are concerned, we can treat it as the same kind
of thing (as though there was a `HasFoo` class).

GHC generics allow us to consider data in terms not of its names, but its
shapes. And so we will see how we can go about bringing a python-like object
system to Haskell.

<!--
```haskell
{-# LANGUAGE DeriveGeneric #-} -- derive Generic instances for us
{-# LANGUAGE TypeOperators #-} -- use the :+: and :*: type names
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-} -- allow the different instances for
                                  -- the different M1s
{-# LANGUAGE DefaultSignatures #-} -- use the default keyword in classes

import GHC.Generics
import Data.Typeable
import Data.Dynamic -- dynamically typed objects, with safe cast
import Data.Semigroup
import Data.Maybe
```
-->

What is an object, anyway?
==================

That all requires some sense of what an object is. A python object is really
just a mapping from names to fields, which are all dynamically typed. So that is
what our objects will be too.

```haskell
import qualified  Data.Map.Strict as Map

type Map = Map.Map

data Object = Object
  { objectMeta             :: Map String String
  , objectNamedFields      :: Map String Dynamic
  , objectPositionalFields :: [Dynamic]
  }
  deriving (Show, Typeable)
```

In addition to the named fields, we will also store special meta-data about the
object, such as its type and its constructor. Fields either have names (as in
records) or they are placed positionally.

We want to then extract fields and meta-data of course, so we need to
say how to get fields out:

```haskell
data Key a = Named String | Positional Int
  deriving (Show, Eq)

get :: Typeable a => Key a -> Object -> Maybe a
get (Named s) = getField s
get (Positional i) = getFieldAt i

getField :: Typeable a => String -> Object -> Maybe a
getField k (Object _ fs _) = Map.lookup k fs >>= fromDynamic

getFieldAt :: Typeable a => Int -> Object -> Maybe a
getFieldAt k (Object _ _ fs) = listToMaybe (drop k fs) >>= fromDynamic

getType :: Object -> Maybe String
getType (Object m _ _) = Map.lookup "__type" m

mkObject :: Typeable a => String -> a -> Object
mkObject k v = Object mempty (Map.singleton k (toDyn v)) []
```

This lets us fetch individual fields, where the user specifies the name and
implicitly requires it to be of a specific type. We return it if we can.

Lets take these objects out for a spin:

```haskell
let os = [mkObject "foo" 'a'
         ,mkObject "foo" (10 :: Int)
         ,mkObject "foo" 'b'
         ,mkObject "bar" 'c'
         ]
mapM_ print . catMaybes $ fmap (get (Named "foo" :: Key Char)) os
> 'a'
> 'b'
```

Notice we have to tell the compiler what we are expecting back, and we can do
that by annotating the keys. We can even have a function like the useful
`get-in` from clojure that walks a chain of fields to find the thing we want
at the end:

```haskell
getIn :: Typeable a => Key a -> [Key Object] -> Object -> Maybe a
getIn k ks o = foldr (\k mo -> mo >>= get k) (pure o) (reverse ks) >>= get k
```

Which works as follows:

```haskell
let o = mkObject "a" (mkObject "b" (mkObject "c" (mkObject "d" True)))
print $ getIn (Named "d" :: Key Bool) [Named "a", Named "b", Named "c"] o
> Just True
```

Well that isn't very useful (yet)
============================

So far all we have is a wonky map-based obejct system. It would be neither
ergonomic nor practical to do any real programming using the `Object` type.
Instead, we will want to use normal Haskell ADTs, and be able to transform them
into Objects to be queried structurally.

This is where Generics come in handy. They are based on the observation that the
algebra of Alegbraic Data Types means that we can identify a minimal subset of
shapes we can use to represent all data types:

* `V1` - this is the data type without constructors.
  There aren't lots of these,
  but some examples include `Void`, and the phantom data types used as tags in
  some systems.
* `U1` - this is the value without any fields.
  This includes common things like
  `True` and `False` as well as enumerations like `data XS = A | B | C`. In this
  case the value being represented is `True` or `A`.
* `a :+: b` is where there is a choice of two constructors.
  This includes things like `Bool = True | False`,
  or `data Tree a = Tip | Branch (Tree a) (Tree a)`
  or `data Either a b = Left a | Right b`. In this case the value being
  represented is _a Bool_, or _Either an a or a b_, not the side itself.
  Where there are more than two options, the expressions nest,
  as `(a :+: b :+: c :+: ...)` and so on.
* `a :*: b` represents the combination of two things.
  This happens all the time when a value has more than one field, either named
  or positional. In `data Foo = F Int Char Word` there are three fields, and all
  are present, so this might be come as `(int :*: (char :*: word))`. The
  simplest example is `(a,b)`, which is just that, at combination of two things.
* `K1` represents a leaf value itself.
* `M1` holds the meta-data
  This includes data-type name, constructor names, and field names. Rather than
  repeating this in several places, this is unified into one representation.

Our job then is to use the GHC Generics machinery to turn normal Haskell ADTs
into these Generic data-types, and then transform these into Objects. Since all
these six things are distinct types, we need a type-class to dispatch over them:

From Rep to Object
====================

The normal pattern is to have two type classes, one for the normal Haskell ADTs,
and one for the structural representation. We will call the two classes here
`ToObject` and `ToObject'`

```haskell
class ToObject a where
  object :: a -> Object
  default object :: (Generic a, ToObject' (Rep a)) => a -> Object
  object x = toObject (from x)
```

In the outer wrapper class, we allow types to specify a custom instance, but
then say, well, if you can transform yourself into one of those types up above,
then I can handle you myself, in a uniform, generic manner.

This implementation lives in Generic land, where every constructor has a phantom
parameter, so we expect all the types in this class to be of kind `* -> *`. This
means the structural type-class is:

```haskell
class ToObject' f where
  toObject :: f p -> Object
```

Then we need to handle each of the six possible representations. The first
few are entirely straightforward:

```haskell
instance ToObject' V1 where
  toObject x = error "impossible"
```

For the uninhabited type, we will never be passed any instances, since they can
never be constructed. We can just error out here.


```haskell
instance ToObject' U1 where
  toObject U1 = Object Map.empty Map.empty []
```

For empty values, return an empty object.

```haskell
instance ( ToObject' lhs , ToObject' rhs) => ToObject' (lhs :+: rhs) where
           toObject (L1 lhs) = toObject lhs
           toObject (R1 rhs) = toObject rhs
```

If we have a LHS, then handle that, otherwise handle the RHS.

```haskell
instance ( ToObject' lhs
         , ToObject' rhs) => ToObject' (lhs :*: rhs) where
           toObject (lhs :*: rhs) = toObject lhs <> toObject rhs
```

If we have both a RHS and a LHS, then combine them in some way. To do this, it
is helpful to define a Semigroup instance for our objects:

```haskell
instance Semigroup Object where
  (Object m1 a fs) <> (Object m2 b fs') = Object (m1 <> m2) (a <> b) (fs <> fs')
```

Where it gets interesting is when we get down to values and names, so `K1` and
`M1`. For `K1` we know the value, but we don't know its name, if it has one, so
we will package it up into an object under a special key which is not a legal 
Haskell identifier, so we can detect it later. Also we want to be careful to
distinguish between values like `10 :: Int`, `'a' :: Char`, which we want to
stick in the object directly as as leaf, and ADTs like `Tree a`, which we
want to decompose further if possible. So we will introduce a new type class
here to dispatch between primitive and composite values:

```haskell
class ToValue a where
  toVal :: a -> Dynamic

  default toVal :: (ToObject a) => a -> Dynamic
  toVal x = toDyn $ object x

instance (ToValue x) => ToObject' (K1 i x) where
  toObject (K1 x) = Object Map.empty (Map.singleton "$value" (toVal x)) []
```

Now when we get to the `M1` nodes, we need to handle them differently based on
whether they hold constructor, datatype or selector (_field_) names. Constructor
and Datatype are simple - extract the names and tag the objects:

```haskell
metaCon, metaType :: String
metaCon  = "tag"
metaType = "type"

instance (Constructor t, ToObject' f) => ToObject' (M1 C t f) where
  toObject m =
    let o = toObject (unM1 m)
     in o { objectMeta = Map.insert metaCon (conName m) (objectMeta o) }

instance (Datatype t, ToObject' f) => ToObject' (M1 D t f) where
  toObject m =
    let o = toObject (unM1 m)
     in o { objectMeta = Map.insert metaType (datatypeName m) (objectMeta o) }
```

When we get a selector, we need to behave slightly differently depending whether
we get a leaf node (a value) or another kind of object and depending on whether
the selector has a name (is a record field) or is empty (is positional):

```haskell
instance (Selector t, ToObject' f) => ToObject' (M1 S t f) where
  toObject m =
    let val = toObject (unM1 m)
     in case Map.toList (objectNamedFields val) of
         [("$value", v)] -> case selName m of
                               [] -> val { objectNamedFields = mempty
                                         , objectPositionalFields = [v]
                                         }
                               k -> val { objectNamedFields = Map.singleton k v }
         _ -> case selName m of
                [] -> Object mempty mempty [toDyn val]
                k  -> Object mempty (Map.singleton k (toDyn val)) []
```

Putting this into action
============================

So now we need some ADTs to use this with, for example:

```haskell
data Foo = Foo { fooA :: Int, fooB :: Bool, fooC :: String }
  deriving (Show, Eq, Generic)

data Bar = Bar Double Int deriving (Show, Eq, Generic)

data WhatIDid = Veni | Vidi | Vici deriving (Show, Eq, Generic)

data Nested = N { nInt :: Int, nFoo :: Foo, nBar :: Bar }
  deriving (Show, Eq, Generic)

data Tree a = Tip | Branch { treeNode :: a, treeLHS :: (Tree a), treeRHS :: (Tree a) }
  deriving (Show, Eq, Generic)
```

For everything we want to handle structurally, we need to opt in to the
`ToObject` and `ToValue` classes. Thankfully the implementations are not
onerous, but they are a bit boilerplatey:

```haskell
instance ToValue Int where toVal = toDyn
instance ToValue Double where toVal = toDyn
instance (Typeable a) => ToValue [a] where toVal = toDyn
instance ToValue Bool where toVal = toDyn
instance ToValue Char where toVal = toDyn

instance ToValue Foo
instance ToObject Foo

instance ToValue Bar
instance ToObject Bar

instance ToValue WhatIDid
instance ToObject WhatIDid

instance ToValue Nested
instance ToObject Nested

instance (Typeable a, ToValue a) => ToValue (Tree a)
instance (Typeable a, ToValue a) => ToObject (Tree a)
```

And we can give it a whirl:

```haskell
let t_char = Branch 'a' Tip
                        (Branch 'b' (Branch 'e' Tip Tip) 
                                    (Branch 'd' Tip Tip))
getIn (Named "treeNode" :: Key Char) [Named "treeRHS", Named "treeLHS"]
      (object t_char)
> Just 'e'

let t_bar = Branch (Bar 1.0 1)
                   Tip
                   (Branch (Bar 2.0 2)
                           (Branch (Bar pi 0) Tip Tip)
                           (Branch (Bar 42 3) Tip Tip))
getIn (Positional 0 :: Key Double) [Named "treeRHS", Named "treeLHS", Named "treeNode"]
      (object t_bar)
> Just 3.141592653589793
```

Wrapping it all up
===================

This is not a serious example, necessarily, although it does point to the kinds
of things that GHC Generics are good for. When you want to handle data according
to its shape, not its meaning, then this is a good way to reduce duplicated
effort, and allow 3rd party tools to opt-in. Examples of this are often to do
with interacting with the outside world, such as JSON encoding/decoding, or
binary serialization. You can write your own `FromJSON` and `ToJSON` instances,
but if you don't have very finicky needs, why not get the compiler to write it
for you? And then you ensure that you never forget to add a new field to the
encoder.

Even this toy example has interesting applications, such as very
lightweight reflection (there are better reflection tools, but the API here is
dead simple), or it could describe in a configuration file the path through a
graph of values to a configuration node. Obviously that is not a Haskelly
approach, since it involves throwing away the type-safety of the nominal type
system in return for the flexibility of structural typing, but it goes to show
that with only a small amount of effort, Haskell has the same flexibility as
even the most unityped language.

[1]: http://hackage.haskell.org/package/base-4.12.0.0/docs/GHC-Generics.html

<!--
# ```haskell
# foorep = from $ Foo 123 True "wibble"
# barep = from $ Bar 1.23 12
# nrep = from $ N 72 (Foo 456 False "wobble") (Bar pi 0)
# achievementrep = from Vidi
# ```
-->
