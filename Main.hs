{-# LANGUAGE OverloadedStrings #-}
import Web.Scotty
import Text.Blaze.Html5
import Text.Blaze.Html5.Attributes
import qualified Web.Scotty as S
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html.Renderer.Text
import Network.Wai.Middleware.Static
import Control.Applicative (liftA)
import Data.Aeson
import Data.Text.Lazy.Encoding

import Recipe
import Templates.Home     (homePage)

main = scotty 3000 $ do
    middleware $ staticPolicy (noDots >-> addBase "static")
    get "/" $ S.html . renderHtml $ homePage
    post "/recipe_scale" handleRecipe

handleRecipe :: ActionM ()
handleRecipe = do
    recipe <- S.body 
    let convertedRecipe = encodeRecipe . liftA scaleRecipe . decode $ recipe 
    case convertedRecipe of
        Just r  -> S.html $ decodeUtf8 r
        Nothing -> S.json $ Data.Aeson.object ["error" .= ("Error" :: String)]

