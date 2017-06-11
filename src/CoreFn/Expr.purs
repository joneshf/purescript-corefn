-- |
-- The core functional representation
--
module CoreFn.Expr
  ( Bind(..)
  , Binder(..)
  , CaseAlternative(..)
  , Expr(..)
  , Literal(..)
  , readBind
  , readBindJSON
  , readExpr
  , readExprJSON
  , readLiteral
  , readLiteralJSON
  ) where

import Prelude

import Control.Alt ((<|>))
import CoreFn.Ident (Ident(..), readIdent)
import CoreFn.Names (ProperName, Qualified, readQualified)
import CoreFn.Util (objectProps)
import Data.Either (Either(..))
import Data.Foreign (F, Foreign, ForeignError(..), fail, readArray, readBoolean, readChar, readInt, readNumber, readString)
import Data.Foreign.Index (readIndex, readProp)
import Data.Foreign.JSON (parseJSON)
import Data.Foreign.Keys as K
import Data.Generic (class Generic, gShow)
import Data.Newtype (wrap)
import Data.Traversable (sequence, traverse)
import Data.Tuple (Tuple(..))

-- |
-- Data type for literal values. Parameterised so it can be used for Exprs and
-- Binders.
--
data Literal a
  -- |
  -- A numeric literal
  --
  = NumericLiteral (Either Int Number)
  -- |
  -- A string literal
  --
  | StringLiteral String
  -- |
  -- A character literal
  --
  | CharLiteral Char
  -- |
  -- A boolean literal
  --
  | BooleanLiteral Boolean
  -- |
  -- An array literal
  --
  | ArrayLiteral (Array a)
  -- |
  -- An object literal
  --
  | ObjectLiteral (Array (Tuple String a))

derive instance eqLiteral :: Eq a => Eq (Literal a)
derive instance ordLiteral :: Ord a => Ord (Literal a)
derive instance genericLiteral :: Generic a => Generic (Literal a)

instance showLiteral :: (Generic a, Show a) => Show (Literal a) where
  show = gShow

readLiteral :: Foreign -> F (Literal (Expr Unit))
readLiteral x = do
  label <- readIndex 0 x >>= readString
  readLiteral' label x

  where

  readValues :: Array Foreign -> F (Array (Expr Unit))
  readValues = traverse readExpr

  readPair :: Foreign -> String -> F (Tuple String (Expr Unit))
  readPair obj key = Tuple key <$> (readProp key obj >>= readExpr)

  readPairs :: Foreign -> Array String -> F (Array (Tuple String (Expr Unit)))
  readPairs obj = sequence <<< (map <<< readPair) obj

  readLiteral' :: String -> Foreign -> F (Literal (Expr Unit))
  readLiteral' "IntLiteral" v = do
    value <- readIndex 1 v
    NumericLiteral <$> Left <$> readInt value
  readLiteral' "NumberLiteral" v = do
    value <- readIndex 1 v
    NumericLiteral <$> Right <$> readNumber value
  readLiteral' "StringLiteral" v = do
    value <- readIndex 1 v
    StringLiteral <$> readString value
  readLiteral' "CharLiteral" v = do
    value <- readIndex 1 v
    CharLiteral <$> readChar value
  readLiteral' "BooleanLiteral" v = do
    value <- readIndex 1 v
    BooleanLiteral <$> readBoolean value
  readLiteral' "ArrayLiteral" v = do
    array <- readIndex 1 v >>= readArray
    ArrayLiteral <$> readValues array
  readLiteral' "ObjectLiteral" v = do
    obj <- readIndex 1 v
    keys <- K.keys obj
    ObjectLiteral <$> readPairs obj keys
  readLiteral' label _ = fail $ ForeignError $ "Unknown literal: " <> label

readLiteralJSON :: String -> F (Literal (Expr Unit))
readLiteralJSON = parseJSON >=> readLiteral

-- |
-- Data type for expressions and terms
--
data Expr a
  -- |
  -- A literal value
  --
  = Literal a (Literal (Expr a))
  -- |
  -- Function introduction
  --
  | Abs a Ident (Expr a)
  -- |
  -- Function application
  --
  | App a (Expr a) (Expr a)
  -- |
  -- Variable
  --
  | Var a (Qualified Ident)
  -- |
  -- Case
  --
  | Case a (Array (Expr a)) (Array (CaseAlternative a))

derive instance eqExpr :: Eq a => Eq (Expr a)
derive instance ordExpr :: Ord a => Ord (Expr a)
derive instance genericExpr :: Generic a => Generic (Expr a)

instance showExpr :: (Generic a, Show a) => Show (Expr a) where
  show = gShow

readExpr :: Foreign -> F (Expr Unit)
readExpr x = do
  label <- readIndex 0 x >>= readString
  readExpr' label x

  where

  readExpr' :: String -> Foreign -> F (Expr Unit)
  readExpr' "Literal" y = do
    value <- readIndex 1 y
    Literal unit <$> readLiteral value
  readExpr' "Abs" y = do
    ident <- readIndex 1 y
    expr <- readIndex 2 y
    Abs unit <$> readIdent ident <*> readExpr expr
  readExpr' "App" y = do
    expr1 <- readIndex 1 y
    expr2 <- readIndex 2 y
    App unit <$> readExpr expr1 <*> readExpr expr2
  readExpr' "Var" y = do
    value <- readIndex 1 y
    Var unit <$> readQualified Ident value
  readExpr' "Case" y = do
    foreigns1 <- readIndex 1 y >>= readArray
    foreigns2 <- readIndex 2 y >>= readArray
    Case unit <$> traverse readExpr foreigns1 <*> traverse readCaseAlternative foreigns2
  readExpr' label _ = fail $ ForeignError $ "Unknown expression: " <> label

readExprJSON :: String -> F (Expr Unit)
readExprJSON = parseJSON >=> readExpr

data CaseAlternative a
  = CaseAlternative a (Array Binder) (Expr a)

derive instance eqCaseAlternative :: Eq a => Eq (CaseAlternative a)
derive instance ordCaseAlternative :: Ord a => Ord (CaseAlternative a)
derive instance genericCaseAlternative :: Generic a => Generic (CaseAlternative a)

readCaseAlternative :: Foreign -> F (CaseAlternative Unit)
readCaseAlternative x = do
  foreigns <- readIndex 0 x >>= readArray
  expr <- readIndex 1 x
  CaseAlternative unit <$> traverse readBinder foreigns <*> readExpr expr

-- |
--  A let or module binding.
--
data Bind a = Bind (Array (Tuple (Tuple a Ident) (Expr a)))

derive instance eqBind :: Eq a => Eq (Bind a)
derive instance ordBind :: Ord a => Ord (Bind a)
derive instance genericBind :: Generic a => Generic (Bind a)

instance showBind :: (Generic a, Show a) => Show (Bind a) where
  show (Bind x) = "(Bind " <> show x <> ")"

readBind :: Foreign -> F (Bind Unit)
readBind x = do
  pairs <- objectProps x
  bindings <- traverse fromPair pairs
  pure $ Bind bindings

  where

  fromPair
    :: { key :: String, value :: Foreign }
    -> F (Tuple (Tuple Unit Ident) (Expr Unit))
  fromPair pair = do
    expr <- readExpr pair.value
    let ident = Ident pair.key
    pure $ Tuple (Tuple unit ident) expr

readBindJSON :: String -> F (Bind Unit)
readBindJSON = parseJSON >=> readBind

data Binder
  = NullBinder
  | LiteralBinder (Literal (Expr Unit))
  | VarBinder Ident
  | ConstructorBinder (Qualified ProperName) (Qualified ProperName) (Array Binder)

derive instance eqBinder :: Eq Binder
derive instance ordBinder :: Ord Binder
derive instance genericBinder :: Generic Binder

readBinder :: Foreign -> F Binder
readBinder x =
  nullBinder x
    <|> literalBinder x
    <|> varBinder x
    <|> constructorBinder x
    where
    nullBinder y = do
      readString y >>= case _ of
        "NullBinder" -> pure NullBinder
        label -> fail $ ForeignError $ "Unknown binder: " <> label
    literalBinder y = do
      readIndex 0 y >>= readString >>= case _ of
        "LiteralBinder" -> do
          LiteralBinder <$> (readIndex 1 y >>= readLiteral)
        label -> fail $ ForeignError $ "Unknown binder: " <> label
    varBinder y = do
      readIndex 0 y >>= readString >>= case _ of
        "VarBinder" -> do
          VarBinder <$> (readIndex 1 y >>= readIdent)
        label -> fail $ ForeignError $ "Unknown binder: " <> label
    constructorBinder y = do
      readIndex 0 y >>= readString >>= case _ of
        "ConstructorBinder" -> do
          name1 <- readIndex 1 y
          name2 <- readIndex 2 y
          foreigns <- readIndex 3 y >>= readArray
          ConstructorBinder <$> readQualified wrap name1 <*> readQualified wrap name2 <*> traverse readBinder foreigns
        label -> fail $ ForeignError $ "Unknown binder: " <> label
