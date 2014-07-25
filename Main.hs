{-# LANGUAGE OverloadedStrings #-}
import Web.Scotty
import Text.Blaze.Html5
import Text.Blaze.Html5.Attributes
import qualified Web.Scotty as S
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html.Renderer.Text
import Network.Wai.Middleware.Static
import Data.Monoid
import Control.Applicative (liftA)
import Data.Aeson
import Data.Text.Lazy.Encoding

import Recipe

main = scotty 3000 $ do
    middleware $ staticPolicy (noDots >-> addBase "static")
    get "/" $ do
        S.html . renderHtml $ do homePage
    post "/recipe_scale" $ do
        handleRecipe             

handleRecipe :: ActionM ()
handleRecipe = do
    recipe <- S.body 
    let convertedRecipe = encodeRecipe . (liftA scaleRecipe) . decode $ recipe 
    case convertedRecipe of
        Just r  -> S.html $ decodeUtf8 r
        Nothing -> S.json $ Data.Aeson.object ["error" .= ("Error" :: String)]

homePage :: Html
homePage = do       
        H.head $ do
            H.title "Recipe Scaler 0.1.0"
            link ! rel "stylesheet" ! type_ "text/css" ! href "style.css"
            script ! type_ "text/javascript" ! src "jquery-2.0.3.min.js" $ mempty
            script ! type_ "text/javascript" ! src "recipes.js" $ mempty

        H.body $ do
            H.div ! class_ "page" $ do
                H.div ! A.id "header" $ do
                    h1 "Reciper Scaler Beta"
            H.div ! A.id "content" $ do
                H.div ! class_ "recipe" ! A.id "initial_recipe" $ do
                    table $ do
                        tr $ do
                            td "Recipe Name:"
                            td $ do
                                input ! A.type_ "text" ! A.id "recipeName" ! A.placeholder "Recipe Name" ! autofocus "" ! required ""
                        tr $ do
                            td "Current Servings:"
                            td $ do
                                input ! A.type_ "number" ! A.id "current" ! A.min "0" ! A.max "99" ! A.placeholder "1"
                            td "Desired Servings"
                            td $ do
                                input ! A.type_ "number" ! A.id "desired" ! A.min "0" ! A.max "99" ! A.placeholder "2"
                        tr $ do
                            td "Ingredients"
                            td ! colspan "3" $ do
                                textarea ! A.id "ingredients" ! A.rows "15"  ! A.cols "40" ! A.placeholder "Measurement Unit Ingredient" $ do
                                    ""
                        tr $ do
                            td ""
                            td $ do
                                H.a ! href "#" ! A.class_ "button" ! A.id "submit_recipe" $ do
                                    "Submit"
                            td $ do
                                H.a ! href "#" ! A.class_ "button" ! A.id "clear_recipe" $ do
                                    "Clear"
                H.div ! class_ "recipe" ! A.id "scaled_recipe" $ do
                    ""

