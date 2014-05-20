module Backend.Recipe
    (
      Ingredient(..)
    , RecipeInfo
    , decomposeRecipe
    , handleLines
    ) where
---------------------------------------
--Imports

import Control.Applicative ((<$>), (<*>))
import Data.List (isPrefixOf)
import Data.Char (toLower)
import Text.Printf (printf)

---------------------------------------
--Data & Types

data Ingredient = Ingredient { amount :: Float
                             , unit :: String
                             , item :: String
                             } deriving (Eq)
instance Show Ingredient where
  show (Ingredient a u i) = printf "%.2f" a ++ " " ++ u ++ " " ++ i

type ConversionFactor = (Float, Float)
type RecipeName = String
type RecipeInfo = (RecipeName, ConversionFactor, [Ingredient])
---------------------------------------
--Functions

decomposeRecipe :: String -> [String]
decomposeRecipe = filter ((&&) <$> (not . null) <*> (not . isPrefixOf "--")) . lines

handleLines :: [String] -> RecipeInfo
handleLines (n:c:i) = (n, calcConversion c, ingredients i)
  where ingredients = map (buildIngredient' (snd . calcConversion $ c) . words) 

calcConversion :: String -> ConversionFactor
calcConversion x = (wanted x, wanted x / given x    )
    where given  = read . head . words
          wanted = read . last . words

buildIngredient :: [String] -> Ingredient
buildIngredient (a:u:i) = Ingredient (read a) u (unwords i)

buildIngredient' :: Float -> [String] -> Ingredient
buildIngredient' c (v:u:i) = convertUnits $ Ingredient (read v * c) u (unwords i)

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