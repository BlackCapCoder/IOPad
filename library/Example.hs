{-# LANGUAGE LambdaCase, GADTSyntax #-}
module Example where

import TrackFingers
import Control.Monad


main :: IO ()
main = do
  fs <- trackFingers "/dev/input/event10"

  forM_ fs $ putStrLn
       . unlines
       . map (\f -> show (trackingID f) ++ ": " ++ show (x f) ++ ", " ++ show (y f))
