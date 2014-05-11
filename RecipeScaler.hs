import System.Environment
import Backend.Recipe
import Text.Printf (printf)

main = do
    (inputFile:outputFile:_) <- getArgs
    recipe <- readFile inputFile
    writeFile outputFile $ formatOutput . handleLines . decomposeRecipe $ recipe
    putStrLn $ "Scaled your recipe and saved results as " ++ show outputFile ++ "."

formatOutput :: RecipeInfo -> String
formatOutput (n,c,i) = "--Recipe\n" ++ n ++ "\n--Servings:\nScaled original by ~" 
                     ++ scaling ++ " to get " ++ servings ++" serving(s).\n--Ingredients\n" ++ ingredients
  where ingredients = unlines . map show $ i
        scaling     = printf "%.3f" . snd $ c
        servings    = show . fst $ c