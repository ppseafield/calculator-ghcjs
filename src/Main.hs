{-# LANGUAGE OverloadedStrings #-}
module Main where

import Prelude hiding (div, id)

import Control.Monad (forM_, when)
import Control.Monad.Trans.Reader 
import Control.Monad.IO.Class

import GHCJS.DOM
import GHCJS.DOM.Document
import GHCJS.DOM.HTMLElement
import GHCJS.DOM.Element
import GHCJS.DOM.HTMLInputElement
import GHCJS.DOM.HTMLButtonElement
import GHCJS.DOM.EventM
import GHCJS.DOM.Node
import GHCJS.DOM.Types

import Text.Blaze.XHtml5 hiding (main)
import qualified Text.Blaze.XHtml5 as H
import Text.Blaze.XHtml5.Attributes
import qualified Text.Blaze.XHtml5.Attributes as A
import Text.Blaze.Html.Renderer.String


-- | List of binary operators for our calculator.
-- Let's stick with floats to keep it simple.
operators :: [( (Float -> Float -> Float), String )]
operators = [ ( (+), "plus" )
            , ( (-), "minus" )
            , ( (*), "times" )
            , ( (/), "divide" )
            ]


main :: IO ()
main = runWebGUI $ \webView -> do
  Just doc  <- webViewGetDomDocument webView
  Just body <- documentGetBody doc

  -- Set the body text to our rendered blaze template
  htmlElementSetInnerHTML body $ renderHtml template

  Just calc  <- fmap castToHTMLInputElement <$> documentGetElementById doc ("calc-input" :: String)
  Just total <- fmap castToHTMLElement <$> documentGetElementById doc ("total" :: String)

  -- Handle number button clicks
  forM_ [0..9] $ \number -> do 
    let num = show number
    Just btnN <- documentGetElementById doc ("btn-" ++ num :: String)
    elementOnclick btnN $ clickNumber calc num

  -- Math Operations
  forM_ operators $ \(operator, name) -> do
    return ()

  -- Backspace and clear all buttons
  Just btnBSP <- documentGetElementById doc ("btn-backspace" :: String)
  elementOnclick btnBSP $ clickBackspace calc

  Just btnCC <- documentGetElementById doc ("btn-clear-all" :: String)
  elementOnclick btnCC $ clickClearAll total calc

  return ()



-- Event handlers

-- | Appends the number to the current calc-input value.
clickNumber :: HTMLInputElement -> String -> EventM MouseEvent Element ()
clickNumber calc number = liftIO $ do
  current <- (htmlInputElementGetValue calc :: IO String)
  htmlInputElementSetValue calc (current ++ number :: String)

-- | Remove the rightmost digit in the current calc-input value.
clickBackspace :: HTMLInputElement -> EventM MouseEvent Element ()
clickBackspace calc = liftIO $ do
  current <- (htmlInputElementGetValue calc :: IO String)
  when (not . null $ current) $  do
    htmlInputElementSetValue calc $ init current


clickClearAll :: HTMLElement -> HTMLInputElement -> EventM MouseEvent Element ()
clickClearAll total calc = liftIO $ do
  current <- (htmlInputElementGetValue calc :: IO String)
  if null current
    then htmlElementSetInnerHTML total ("0.00" :: String)
    else htmlInputElementSetValue calc ("" :: String)




-- | The template for the whole page.
template :: Html
template = do
  H.style "#operators { padding: 10px 0px; }"
  H.style "#calc-input { text-align: right; }"
  H.style ".btn { margin: 2px; padding: 8px; }"
  
  h1 "Calculator-GHCJS"
  div ! id "total" $ "0.00"
  div $ do
    input ! type_ "text" ! id "calc-input" ! disabled "disabled"
    button ! class_ "btn" ! type_ "button" ! id "btn-backspace" $ "<"
    button ! class_ "btn" ! type_ "button" ! id "btn-clear-all" $ "CC"  

  div ! id "operators" $ do
    button ! class_ "btn" ! type_ "button" ! id "btn-plus" $ "+"  
    button ! class_ "btn" ! type_ "button" ! id "btn-minus" $ "-"  
    button ! class_ "btn" ! type_ "button" ! id "btn-times" $ "*"  
    button ! class_ "btn" ! type_ "button" ! id "btn-divide" $ "/"  
  div $ do
    button ! class_ "btn" ! type_ "button" ! id "btn-7" $ "7"  
    button ! class_ "btn" ! type_ "button" ! id "btn-8" $ "8"  
    button ! class_ "btn" ! type_ "button" ! id "btn-9" $ "9"  
  div $ do
    button ! class_ "btn" ! type_ "button" ! id "btn-4" $ "4"  
    button ! class_ "btn" ! type_ "button" ! id "btn-5" $ "5"  
    button ! class_ "btn" ! type_ "button" ! id "btn-6" $ "6"  
  div $ do
    button ! class_ "btn" ! type_ "button" ! id "btn-1" $ "1"  
    button ! class_ "btn" ! type_ "button" ! id "btn-2" $ "2"  
    button ! class_ "btn" ! type_ "button" ! id "btn-3" $ "3"  
  div $ do
    button ! class_ "btn" ! type_ "button" ! id "btn-point" $ "."  
    button ! class_ "btn" ! type_ "button" ! id "btn-0" $ "0"  
