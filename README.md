# Typson

## Introduction

Typson uses type level programming to enable writing JSON PostgreSQL queries
that are type safe. There are integrations available for the `esqueleto`,
`beam`, and `selda` libraries.

The ability to store and query arbitrary JSON in a database can be very
beneficial, however, writing these queries with raw SQL is particularly bug
prone because the correctness of the path keys, the nullability of fields, and
the resulting type must all be accounted for. Typson solves these issues by
describing the JSON structure of the data at the type level so that any errors
will manifest as compile time exceptions rather than runtime bugs.

Typson allows you to define a single schema value which then functions as
both an encoder and decoder for JSON representations. The actual encoding and
decoding is handled by the `aeson` library. This same schema is also what's used
to provide type safety for JSON SQL queries.

## Example

Now for a rather contrived demonstration of how the library works and what you
can do with it.

### Defining our types

Let's say we want to store information about various life forms. We have
different attributes depending on if the subject is a plant or an animal, which
we model with the `Classifier` sum type.

```hs
data LifeForm =
  LifeForm
    { classifier :: Classifier
    , name :: T.Text
    } deriving (Eq, Show)

data Classifier
  = Flora Plant
  | Fauna Animal
  deriving (Eq, Show)

data Plant =
  Plant
    { isAquatic :: Bool
    , bearsFruit :: Bool
    } deriving (Eq, Show)

data Animal =
  Animal
    { favoriteFoods :: [T.Text]
    , isGoodPet :: Bool
    } deriving (Eq, Show)
```

### Defining schemas

Now we'll define the schemas that Typson will use to type check our SQL queries.

```hs
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE PartialTypeSignatures #-}

import Typson

plantJ :: JsonSchema _ Plant
plantJ = object "Plant" $
  Plant
    <<$> field (key @"isAquatic") isAquatic prim
    <<*> field (key @"bearsFruit") bearsFruit prim

animalJ :: JsonSchema _ Animal
animalJ = object "Animal" $
  Animal
    <<$> field (key @"favoriteFoods") favoriteFoods (list prim)
    <<*> field (key @"isGoodPet") isGoodPet prim
```

A few things to note here:
- `JsonSchema` is just a rank-N type synonym for a set of constraints. It's
first argument is the type level data structure that defines the JSON structure.
It's not very useful or readable to have this parameter written out in the code
and the compiler will generate it for you, so it is recommended to use the
`PartialTypeSignatures` extension to hide it. The last argument is the type that
is being described.
- Record types are described using the `object` function and the fields are
defined using syntax that is akin to the standard Applicative form but with
`<<$>` instead of `<$>` and `<<*>` instead of `<*>`. This is to support the
type-level machinery, which the normal `Applicative` type class cannot do.
- There are several combinators for defining fields. `field` denotes a required
attribute which translates to a non-null field in the JSON.
- `list` wraps a schema for some type `a`, turning it into a schema for `[a]`.
This will allow us to query for specific indices of the list.
- The first argument of the field combinators is the key to be used in the JSON.
It is supplied using `key` which is just a synonym for `Proxy`. A type error
occurrs if a key appears more than once in the same object.
- The second argument is the field accessor of the record.
- The final argument is the schema associated with the type of the field. If the
field's type is some sort of primative that has predefined `ToJSON` and
`FromJSON` instances, then the `prim` schema should be used.

Now for the remaining two schemas. Of special interest is the schema for the
`Classifier` sum type.

```hs
classifierJ :: JsonSchema _ Classifier
classifierJ = union "Classifier" $
  classifierTags
    <<$> tag (key @"flora") Flora plantJ
    <<*> tag (key @"fauna") Fauna animalJ
  where
    classifierTags withPlant withAnimal = \case
      Flora plant  -> withPlant plant
      Fauna animal -> withAnimal animal

lifeFormJ :: JsonSchema _ LifeForm
lifeFormJ = object "LifeForm" $
  LifeForm
    <<$> field (key @"classifier") classifier classifierJ
    <<*> field (key @"name") name prim
```

Here we see the somewhat peculiar `union` function which is how schemas for sum
types are defined. We define a function that takes handlers for the contents of
each of the sum's constructors which then pattern matches on the sum and applies
the corresponding handlers to the pattern matched values. The function is
applied over `tag` definitions, one for each branch of the sum, using the same
specialized `Applicative` syntax as before.

Also noteworthy is that we are now referencing other schemas in the last
argument of some of these `field` and `tag` definitions instead of `prim`.

### Writing `aeson` instances

You should always define `FromJSON` and `ToJSON` instances using the Typson
schema for a given type, which is as simple as using the `decodeObject` and
`encodeObject` functions from `typson-core`:

```hs
import Data.Aeson

instance FromJSON LifeForm where
  parseJSON = decodeObject lifeFormJ

instance ToJSON LifeForm where
  toJSON = encodeObject lifeFormJ

instance FromJSON Plant where
  parseJSON = decodeObject plantJ

instance ToJSON Plant where
  toJSON = encodeObject plantJ

instance FromJSON Animal where
  parseJSON = decodeObject animalJ

instance ToJSON Animal where
  toJSON = encodeObject animalJ

instance FromJSON Classifier where
  parseJSON = decodeObject classifierJ

instance ToJSON Classifier where
  toJSON = encodeObject classifierJ
```

### Setting up the database table

We'll be using the `esqueleto` library, which sits on top of `persistent`, to
demonstrate some database interactions. Integrations are also available for the
`beam` and `selda` libraries.

First we'll define a table that has a single `JSONB` column to hold the JSON of
our `LifeForm` type (in addition to the implicit 'id' column).

```hs
import           Database.Esqueleto
import qualified Database.Persist.Postgresql as P
import qualified Database.Persist.TH as P

P.share [P.mkPersist P.sqlSettings, P.mkMigrate "migrateAll"] [P.persistLowerCase|
LifeFormEntity
  object (JSONB LifeForm)
  deriving Show Eq
|]

-- sample data
lifeFormEntities :: [LifeFormEntity]
lifeFormEntities
  = [ LifeFormEntity $ JSONB lion
    , LifeFormEntity $ JSONB dog
    , LifeFormEntity $ JSONB kelp
    , LifeFormEntity $ JSONB bananaTree
    ]

main :: IO ()
main = do
  Just connString <- lookupEnv "CONN_STRING"
  runStderrLoggingT . P.withPostgresqlConn (BS.pack connString) $ \be ->
    (`P.runSqlConn` be) $ do
      P.runMigration migrateAll

      void $ P.insertMany lifeFormEntities
```

The `main` function simply connects to the database, creates the table if it
doesn't exist, and populates it with some sample data (omitted here).

### Querying against a JSON column

Now that we've created our table and populated it with the sample data, we're
ready to write some type safe JSON queries!

```hs
import Data.Proxy
import Typson.Esqueleto

-- Select the names of all life forms.
selectNames :: P.SqlPersistT (LoggingT IO) [T.Text]
selectNames
  = fmap coerce -- unwrap newtypes
  . select . from
  $ \e ->
      pure . jsonPath (Proxy @"name") lifeFormJ
        $ e ^. LifeFormEntityObject

-- Select just the animals
selectAnimals :: P.SqlPersistT (LoggingT IO) [Animal]
selectAnimals
  = fmap (mapMaybe coerce)
  . select . from
  $ \e -> do
      pure . jsonPath (Proxy @("classifier" :-> "fauna")) lifeFormJ
        $ e ^. LifeFormEntityObject
```

The key component here is the `jsonPath` function, which takes these arguments:
- A `Proxy` for the path to the field that we want to select. The `:->` type
operator is used to construct a path from multiple keys, akin to how paths are
written in raw SQL queries using `->`.
- Our schema value
- The `esqueleto` identifier for the JSON column.

If we had typo'ed the keys or constructed an invalid path, we'd get a helpful
type error about it, such as

```
/code/example/src/Main.hs:88:5: error:
    • JSON key not present in Classifier: "fawna"
```

Typson is able to calculate what the resulting type should be given a path into
the object, so our query is well typed!

Here are some more example queries:

```hs
-- Selects all animals that make good pets
selectGoodPets :: P.SqlPersistT (LoggingT IO) [P.Entity LifeFormEntity]
selectGoodPets
  = select . from
  $ \e -> do
      let goodPetPath =
            jsonPath (Proxy @("classifier" :-> "fauna" :-> "isGoodPet")) lifeFormJ

      where_ $ goodPetPath (e ^. LifeFormEntityObject)
                 ==. val (NullableJSONB (Just True))
      pure e

-- Selects the names of all the aquatic plants
selectAquaticPlantNames :: P.SqlPersistT (LoggingT IO) [T.Text]
selectAquaticPlantNames
  = fmap coerce -- unwrap newtypes
  . select . from
  $ \e -> do
      let isAquaticPath =
            jsonPath (Proxy @("classifier" :-> "flora" :-> "isAquatic")) lifeFormJ
      where_ $ isAquaticPath (e ^. LifeFormEntityObject)
                 ==. val (NullableJSONB (Just True))
      pure $ jsonPath (Proxy @("name")) lifeFormJ (e ^. LifeFormEntityObject)

-- Select pairs of names to the 1st item in the list of favorite foods if the
-- entity is an animal.
selectFavoriteFood :: P.SqlPersistT (LoggingT IO) [(T.Text, Maybe T.Text)]
selectFavoriteFood
  = fmap coerce -- unwrap newtypes
  . select . from
  $ \e -> do
      let o = e ^. LifeFormEntityObject
          name' = jsonPath (Proxy @"name") lifeFormJ o
          favoriteFood =
            jsonPath (Proxy @("classifier" :-> "fauna" :-> "favoriteFoods" :-> 0)) lifeFormJ o
      pure (name', favoriteFood)
```

Here we have `where` clauses that reference specific JSON fields.
There is also an instance of using a numeric literal as part of the path to
query a particular index of an array field.

In conclusion, as long as the JSON in your database was produced using the same
schema that's being used for querying, then you can rest assured that your JSON
queries won't result in runtime bugs!
