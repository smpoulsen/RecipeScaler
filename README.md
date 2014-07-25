Migrating from Flask to Scotty

Goal: Move pre-existing web application written in Python to Haskell.

A while ago, I wrote RecipeScaler to convert/scale ingredients for cooking.

I wanted to make a web-application based around it, and the quickest way I knew how to do so was to use Flask.

I'm now going back to make the frontend Haskell based as well.

Using: Scotty and Blaze.Html

Problems:
    Aeson - poorly typed json coming in wouldn't type check; took a while to debug (floats as strings)

   
Links: 
    https://hackage.haskell.org/package/aeson-0.8.0.0/docs/Data-Aeson.html
    http://the-singleton.com/2012/02/parsing-nested-json-in-haskell-with-aeson/
