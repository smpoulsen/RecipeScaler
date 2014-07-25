{-# LANGUAGE OverloadedStrings #-}

module Templates.Home
    ( homePage
    ) where

import Web.Scotty
import Text.Blaze.Html5
import Text.Blaze.Html5.Attributes
import qualified Web.Scotty as S
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html.Renderer.Text
import Data.Monoid (mempty)

homePage :: Html
homePage = do       
        H.head $ do
            H.title "Recipe Scaler 0.1.0"
            link ! rel "stylesheet" ! type_ "text/css" ! href "style.css"
            script ! type_ "text/javascript" ! src "jquery-2.0.3.min.js" $ mempty
            script ! type_ "text/javascript" ! src "recipes.js" $ mempty

        H.body $ do
            H.div ! class_ "page" $ 
                H.div ! A.id "header" $ 
                    h1 "Reciper Scaler Beta"
            H.div ! A.id "content" $ do
                H.div ! class_ "recipe" ! A.id "initial_recipe" $ 
                    table $ do
                        tr $ do
                            td "Recipe Name:"
                            td $ input ! A.type_ "text" ! A.id "recipeName" ! A.placeholder "Recipe Name" ! autofocus "" ! required ""
                        tr $ do
                            td "Current Servings:"
                            td $ input ! A.type_ "number" ! A.id "current" ! A.min "0" ! A.max "99" ! A.placeholder "1"
                            td "Desired Servings"
                            td $ input ! A.type_ "number" ! A.id "desired" ! A.min "0" ! A.max "99" ! A.placeholder "2"
                        tr $ do
                            td "Ingredients"
                            td ! colspan "4" $ textarea ! A.id "ingredients" ! A.rows "15"  ! A.cols "40" ! A.placeholder "Measurement Unit Ingredient" $ ""
                        tr $ do
                            td ""
                            td $ H.a ! href "#" ! A.class_ "button" ! A.id "submit_recipe" $ "Submit"
                            td $ H.a ! href "#" ! A.class_ "button" ! A.id "clear_recipe" $ "Clear"
                H.div ! class_ "recipe" ! A.id "scaled_recipe" $ ""
