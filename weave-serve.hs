{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Main where

import Happstack.Lite
import Happstack.Server.RqData (lookRead)
import Control.Monad (guard)
import qualified Data.ByteString.Lazy.Char8 as B
import Weave

main :: IO ()
main = serve Nothing myApp

myApp :: ServerPart Response
myApp = msum [dir "weave" $ weave,
      	      dir "weave" $ weaveFail,
              dir "shannon" $ weaveShannonDefaults,
--               dir "5gram" $ fivegram,
--               dir "4gram" $ fourgram,
              root,
              e404]

root = serveDirectory DisableBrowsing ["index.html"] "public"

e404 = ok $ toResponse ("you step in the stream / but the water has moved on / 404 not found" :: B.ByteString)

weave = do
           barHeight <- lookRead "barHeight"
           displaySegments <- lookRead "displaySegments"
	   guard (displaySegments > 5 && displaySegments < 500) -- not too choppy, not too compute-intensive
	   ifWidth <- lookRead "ifWidth"
	   ifHeight <- lookRead "ifHeight"
	   visSize <- lookRead "visSize"
	   lineLength <- lookRead "lineLength"
           grams <- lookRead "grams"
           ok $ toResponseBS "image/svg+xml" $ wordsToSVG ifWidth ifHeight displaySegments visSize lineLength barHeight grams

weaveShannonDefaults = ok $ toResponseBS "image/svg+xml" $ wordsToSVG 17 23 200 0.001 12 0.1 "The fundamental problem of communication is that of reproducing at one point either exactly or approximately a message selected at another point . Frequently the messages have meaning ; that is they refer to or are correlated according to some system with certain physical or conceptual entities . These semantic aspects of communication are irrelevant to the engineering problem . The significant aspect is that the actual message is one selected from a set of possible messages ."

weaveFail = ok $ toResponse ("some of your parameters were incorrect" :: B.ByteString)
