{- Implements this: https://www.kernel.org/doc/Documentation/input/multi-touch-protocol.txt

   I have only tested this on one machine, but presumably it is somewhat standard.
   It may or may not work on mac, but defenitly works on Linux.
   Windows is shooting yourself in the leg and just plain lazy; go away!
-}

{-# LANGUAGE LambdaCase, GADTSyntax #-}
module TrackFingers
  ( Finger (..)
  , trackFingers
  ) where

import System.IO
import Control.Monad
import qualified Data.ByteString as B
import Foreign.Storable (sizeOf)
import Foreign.C.Types
import Data.Bits (shiftL)
import Data.Time.Clock.POSIX
import Data.List
import qualified Data.Map as M
import Control.Monad.State
import System.IO.Unsafe (unsafeInterleaveIO)
import Data.Default


data Event where
  SYN_REPORT :: Event
  EV_KEY     :: KEY -> CInt -> Event
  EV_ABS     :: ABS -> CInt -> Event
  deriving (Show, Eq)

data KEY = BTN_LEFT
         | BTN_TOOL_FINGER
         | BTN_TOOL_QUINTTAP
         | BTN_TOUCH
         | BTN_TOOL_DOUBLETAP
         | BTN_TOOL_TRIPLETAP
         | BTN_TOOL_QUADTAP
         deriving (Show, Eq)

data ABS = ABS_X
         | ABS_Y
         | ABS_MT_SLOT
         | ABS_MT_POSITION_X
         | ABS_MT_POSITION_Y
         | ABS_MT_TRACKING_ID
         deriving (Show, Eq)

data Finger = Finger
  { trackingID :: Int
  , x :: Int
  , y :: Int
  } deriving Show

data Model = Model
  { activeSlot :: Int
  , fingers    :: M.Map Int Finger
  } deriving Show


instance Default Model where
  def = Model
    { activeSlot = 0
    , fingers    = mempty
    }

keyFromCode = \case
  272 -> BTN_LEFT
  330 -> BTN_TOUCH
  325 -> BTN_TOOL_FINGER
  333 -> BTN_TOOL_DOUBLETAP
  334 -> BTN_TOOL_TRIPLETAP
  335 -> BTN_TOOL_QUADTAP
  328 -> BTN_TOOL_QUINTTAP

absFromCode = \case
  0  -> ABS_X
  1  -> ABS_Y
  47 -> ABS_MT_SLOT
  53 -> ABS_MT_POSITION_X
  54 -> ABS_MT_POSITION_Y
  57 -> ABS_MT_TRACKING_ID

isPosX = \case
  EV_ABS ABS_MT_POSITION_X _ -> True
  _ -> False
isPosY = \case
  EV_ABS ABS_MT_POSITION_Y _ -> True
  _ -> False


parseInt :: B.ByteString -> Integer
parseInt bs
  | arr <- fromIntegral <$> B.unpack bs
  = sum $ zipWith shiftL (reverse arr) [0, 8..]

parseInt' :: B.ByteString -> Integer
parseInt' bs
  | arr <- fromIntegral <$> B.unpack bs
  = sum $ zipWith shiftL arr [0, 8..]

readEvent :: Handle -> IO Event
readEvent h = do
  time  <- fmap (realToFrac . parseInt') . B.hGetSome h $ sizeOf (0 :: CTime) :: IO POSIXTime
  x     <- B.hGetSome h 5
  type_ <- parseInt <$> B.hGetSome h 4
  y     <- B.hGetSome h 2
  z     <- B.hGetSome h 1
  val   <- fromIntegral . parseInt' <$> B.hGetSome h 4 :: IO CInt

  let code = parseInt $ z `mappend` B.tail y

  return $ case type_ of
    0 -> SYN_REPORT
    1 -> EV_KEY (keyFromCode code) val
    3 -> EV_ABS (absFromCode code) val
    _ -> error "Unknown event type"

readRaport :: Handle -> IO [Event]
readRaport h = do
  ev <- readEvent h
  if ev == SYN_REPORT
    then return []
    else (ev:) <$> readRaport h


-- Infinite list of finger positions
trackFingers :: FilePath -> IO [[Finger]]
trackFingers pth = do
  h  <- openFile pth ReadMode
  fs <- flip fix def $ \rec m -> do
    rs <- liftIO $ readRaport h

    fmap (fingers m:) . unsafeInterleaveIO . rec
                  . snd . flip runState m
                  . forM_ rs $ \case
      EV_ABS ABS_MT_SLOT s ->
        modify $ \st -> st { activeSlot = fromIntegral s }
      EV_ABS ABS_MT_TRACKING_ID (-1) ->
        modify $ \st -> st { fingers = M.delete (activeSlot st) $ fingers st }
      EV_ABS ABS_MT_TRACKING_ID tid
        | Just (EV_ABS _ x) <- find isPosX rs
        , Just (EV_ABS _ y) <- find isPosY rs
        , f <- Finger { trackingID = fromIntegral tid
                      , x          = fromIntegral x
                      , y          = fromIntegral y }
       -> modify $ \st -> st { fingers = M.insert (activeSlot st) f $ fingers st }
      EV_ABS ABS_MT_POSITION_X x ->
        modify $ \st -> st { fingers = M.update (
            \f -> Just $ f { x = fromIntegral x }
          ) (activeSlot st) (fingers st) }
      EV_ABS ABS_MT_POSITION_Y y ->
        modify $ \st -> st { fingers = M.update (
            \f -> Just $ f { y = fromIntegral y }
          ) (activeSlot st) (fingers st) }
      _ -> return ()

  return $ map snd . M.toList <$> fs

