import System.Environment
import Backend.Recipe

main = do
    (inputFile:outputFile:_) <- getArgs
    recipe <- readFile inputFile
    writeFile outputFile $ formatOutput . handleLines . decomposeRecipe $ recipe
    putStrLn $ "Scaled your recipe and saved results as " ++ show outputFile ++ "."