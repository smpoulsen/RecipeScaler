module Backend.Recipe
    (
      Ingredient(..)
    , decomposeRecipe
    , handleLines
    , formatOutput
    ) where
---------------------------------------
--Imports

import Control.Applicative ((<$>), (<*>))
import Data.List (isPrefixOf)
import Data.Char (toLower)

---------------------------------------
--Data & Types

data Ingredient = Ingredient { amount :: Float
                             , unit :: String
                             , item :: String
                             } deriving (Eq)
instance Show Ingredient where
  show (Ingredient a u i) = show a ++ " " ++ u ++ " " ++ i

type ConversionFactor = (Float, Float)
type RecipeName = String
type RecipeInfo = (RecipeName, ConversionFactor, [Ingredient])
---------------------------------------
--Functions

decomposeRecipe :: String -> [String]
decomposeRecipe = filter ((&&) <$> (not . null) <*> (not . isPrefixOf "--")) . lines

handleLines :: [String] -> RecipeInfo
handleLines (n:c:i) = (n, conversion c, ingredients i)
  where ingredients = map (buildIngredient' (snd . conversion $ c) . words) 
        conversion  = calcConversion 

formatOutput :: RecipeInfo -> String
formatOutput (n,c,i) = "--Recipe\n" ++ n ++ "\n--Servings:\nScaled original by " ++ scaling ++ " to get " ++ servings ++" serving(s).\n--Ingredients\n" ++ ingredients
  where ingredients = unlines . map show $ i
        scaling     = show . snd $ c
        servings    = show . fst $ c
calcConversion :: String -> ConversionFactor
calcConversion x = (wanted x, wanted x / given x    )
    where given  = read . head . words
          wanted = read . last . words

buildIngredient :: [String] -> Ingredient
buildIngredient (a:u:i) = Ingredient (read a) u (unwords i)

buildIngredient' :: Float -> [String] -> Ingredient
buildIngredient' c (v:u:i) = convertUnits $ Ingredient (read v * c) u (unwords i)

convertUnits :: Ingredient -> Ingredient
convertUnits (Ingredient a u i) 
    | a < 1 = case map toLower u of
                "kg" -> Ingredient (a * 1000) "g" i
                "l"  -> Ingredient (a * 1000) "mL" i
    | a > 1000 = case map toLower u of
                  "g"  -> Ingredient (a / 1000) "kg" i
                  "ml" -> Ingredient (a / 1000) "L" i
    | otherwise = Ingredient a u i
