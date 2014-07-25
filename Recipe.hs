{-# LANGUAGE OverloadedStrings #-}

module Recipe
    (
      Ingredient(..)
    , Recipe(..)
    , scaleRecipe
    , decodeRecipe
    , encodeRecipe
    ) where
---------------------------------------
--Imports

import Control.Applicative (liftA, (<$>), (<*>))
import Control.Monad       (mzero)
import Data.List           (isPrefixOf)
import Data.Char           (toLower)
import Numeric             (showGFloat)
import qualified Data.Text as T
import qualified Data.ByteString.Lazy.Internal as BS
import Text.Printf (printf)
import Data.Aeson

---------------------------------------
--Data & Types

data Ingredient = Ingredient { amount :: Float
                             , unit   :: String
                             , item   :: String
                             } deriving (Eq)
instance Show Ingredient where
  show (Ingredient a u i) = printf "%.2f" a ++ " " ++ u ++ " " ++ i

instance FromJSON Ingredient where
  parseJSON (Object o) = Ingredient <$>
                          o .: "amount" <*>
                          o .: "unit"   <*>
                          o .: "ingredient" 
  parseJSON _          = mzero 

instance ToJSON Ingredient where
  toJSON (Ingredient amount unit item) = object ["amount" .= showGFloat (Just 2) amount "", "unit" .= unit, "item" .= item] 

data Recipe = Recipe { name        :: T.Text 
                     , current     :: Float
                     , desired     :: Float
                     , ingredients :: [Ingredient] 
                     } deriving (Eq, Show)

instance FromJSON Recipe where
  parseJSON (Object v) = do
            recipeName <- parseJSON =<< (v .: "recipeName")
            cServings   <- parseJSON =<< (v .: "currentServings")
            dServings   <- parseJSON =<< (v .: "desiredServings")
            ingredients <- parseJSON =<< (v .: "ingredients")
            return $ Recipe recipeName cServings dServings ingredients 
  parseJSON _          = mzero

instance ToJSON Recipe where
  toJSON (Recipe name current desired ingredients) = 
                    object ["recipeName" .= name, "currentServings" .= current, "desiredServings" .= desired, "ingredients" .= ingredients]


type ConversionFactor = (Float, Float)
type RecipeName = String
type RecipeInfo = (RecipeName, ConversionFactor, [Ingredient])
---------------------------------------
--Functions
scaleRecipe :: Recipe -> Recipe
scaleRecipe r = Recipe n c d i'
  where n     = name r
        c     = current r
        d     = desired r
        scale = d / c
        i' = map (\x -> Ingredient (amount x * scale) (unit x) (item x)) $ ingredients r

decodeRecipe :: BS.ByteString -> Maybe Recipe
decodeRecipe = decode

encodeRecipe :: Maybe Recipe -> Maybe BS.ByteString 
encodeRecipe = liftA encode

convertUnits :: Ingredient -> Ingredient
convertUnits ingredient@(Ingredient a u i) 
    | a < 1          = case unit of
                          "kg" -> Ingredient (a * 1000) "g" i
                          "l"  -> Ingredient (a * 1000) "mL" i
                          otherwise -> ingredient
    | a > 1 && a < 8 = case unit of
                          "tsp" -> Ingredient (a / 3) "Tbsp" i
                          otherwise -> ingredient
    | a > 1000       = case unit of
                          "g"  -> Ingredient (a / 1000) "kg" i
                          "ml" -> Ingredient (a / 1000) "L" i
                          otherwise -> ingredient 
    | otherwise = ingredient
    where unit = map toLower . filter (`notElem` ".,;:?!") $ u

-----------------------------------------
--Deprecated 
decomposeRecipe :: String -> [String]
decomposeRecipe = filter ((&&) <$> (not . null) <*> (not . isPrefixOf "--")) . lines

handleLines :: [String] -> RecipeInfo
handleLines (n:c:i) = (n, calcConversion c, ingredients i)
  where ingredients = map (buildIngredient' (snd . calcConversion $ c) . words) 

calcConversion :: String -> ConversionFactor
calcConversion x = (wanted x, wanted x / given x)
    where given  = read . head . words
          wanted = read . last . words

buildIngredient :: [String] -> Ingredient
buildIngredient (a:u:i) = Ingredient (read a) u (unwords i)

buildIngredient' :: Float -> [String] -> Ingredient
buildIngredient' c (v:u:i) = convertUnits $ Ingredient (read v * c) u (unwords i)
