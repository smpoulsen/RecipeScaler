module Backend.Recipe
    (
      Ingredient(..)
    , decomposeRecipe
    , handleLines
    , formatOutput
    ) where
---------------------------------------
--Imports

import Data.List (isPrefixOf)
import Data.Char (toLower)

---------------------------------------
--Data & Types

data Ingredient = 
    Solid { mass :: Float
          , unit :: String
          , item :: String
          }
  | Liquid { volume :: Float
           , unit   :: String
           , item   :: String
           }
  deriving (Eq)
instance Show Ingredient where
  show x = case x of
    Solid m u i  -> show m ++ " " ++ u ++ " " ++ i
    Liquid m u i -> show m ++ " " ++ u ++ " " ++ i

type ConversionFactor = Float
type RecipeName = String
type RecipeInfo = (RecipeName, ConversionFactor, [Ingredient])
---------------------------------------
--Functions

decomposeRecipe :: String -> [String]
decomposeRecipe = filter (not . null) . filter (not . isPrefixOf "--") . lines

handleLines :: [String] -> RecipeInfo
handleLines (n:c:i) = (n, conversion c, ingredients i)
  where ingredients = map (buildIngredient' (conversion c) . words) 
        conversion  = calcConversion 

formatOutput :: RecipeInfo -> String
formatOutput (n,c,i) = "--Recipe\n" ++ n ++ "\n--Scaled by\n" ++ show c ++ "\n--Ingredients\n" ++ ingredients
  where ingredients = unlines . map show $ i

calcConversion :: String -> ConversionFactor
calcConversion x = given x / wanted x
    where given  = read . head . words
          wanted = read . last . words

buildIngredient :: [String] -> Ingredient
buildIngredient (v:u:i)  
  | map toLower u `elem` ["g", "gram", "kg", "kilogram"] = Solid (read v) u (unwords i)
  | map toLower u `elem` ["l", "litre", "liter"]         = Liquid (read v) u (unwords i)

buildIngredient' :: Float -> [String] -> Ingredient
buildIngredient' c (v:u:i)  
  | map toLower u `elem` ["g", "gram", "kg", "kilogram"] = Solid (read v / c) u (unwords i)
  | map toLower u `elem` ["l", "litre", "liter"]         = Liquid (read v / c) u (unwords i)
