{-# LANGUAGE OverloadedStrings #-}

import Data.Aeson
import Control.Applicative ((<$>), (<*>))
import Control.Monad (mzero)
import qualified Data.Text as T

data Ingredient = Ingredient { amount :: Float
                             , unit :: String
                             , item :: String
                             } deriving (Eq)
instance Show Ingredient where
  show (Ingredient a u i) = printf "%.2f" a ++ " " ++ u ++ " " ++ i

instance FromJSON Ingredient where
  parseJSON (Object o) = Ingredient <$>
                          o .: "amount"      <*>
                          o .: "measurement" <*>
                          o .: "ingredient"  
  parseJSON _          = mzero 

data Recipe = Recipe { name        :: Maybe T.Text 
                     , current     :: Maybe Float
                     , desired     :: Maybe Float
                     , ingredients :: Maybe Array 
                     } deriving (Eq, Show)
instance FromJSON Recipe where
  parseJSON (Object v) = Recipe <$> 
                         v .:? "recipeName"      <*>
                         v .:? "currentServings" <*>
                         v .:? "desiredServings" <*>
                         v .:? "ingredients"      
  parseJSON _          = mzero
