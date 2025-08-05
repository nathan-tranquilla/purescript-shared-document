module App where

import Data.Either
import Data.Maybe
import Effect
import Prelude

import Data.Array (filter, foldl)
import Data.String (length)
import Effect.Aff (Aff, runAff_, throwError, try)
import Effect.Class (class MonadEffect)
import Effect.Console (log, logShow)
import Effect.Exception (throw, throwException)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Web.Event.Event (target)
import Web.Event.Internal.Types (EventTarget)
import Web.HTML.HTMLTextAreaElement (selectionStart, fromEventTarget, value)
import Web.UIEvent.KeyboardEvent (KeyboardEvent, key, toEvent)
import WebCrypto (computeSHA256)

type State 
  = { input :: Maybe String, pos :: Maybe Int, version :: Int }

data Action
  = Input String (Maybe EventTarget)

component :: forall q i o m. MonadEffect m => H.Component q i o m
component =
  H.mkComponent
    { initialState: \_ -> { input: Nothing, pos: Nothing, version: 0 }
    , render
    , eval: H.mkEval H.defaultEval { handleAction = handleAction }
    }

render :: forall cs m. State -> H.ComponentHTML Action cs m
render state =
  HH.div_
    [ HH.textarea
        [ HE.onKeyDown \event -> Input (key event) (target $ toEvent event)
        ]
      , HH.div_ [HH.text (showState state) ]
    ]

handleAction :: forall cs o m. MonadEffect m => Action -> H.HalogenM State Action cs o m Unit
handleAction = case _ of
  Input k maybeTarget -> do
    H.liftEffect $ log $ "Key pressed: " <> k -- Log the key pressed
    case maybeTarget >>= fromEventTarget of
      Just el -> do
        pos       <- H.liftEffect $ selectionStart el
        H.liftEffect $ logShow $ "Cursor position: " <> show pos -- Log the cursor position
        H.modify_ \st -> st { input = Just k, pos = Just pos, version = st.version + 1 }
      Nothing -> do
        H.liftEffect $ log "No valid target found." -- Log if no valid target is found
        pure unit
  
showState :: State -> String 
showState state = foldl (\acc txt -> acc <> " " <> txt) "" 
  $ filter (\str -> length str > 0) [showText "Input" state.input,showPos state.pos, "Version: " <> show state.version]

showText :: String -> Maybe String -> String 
showText txt mInput = case mInput of 
  Just inpt -> txt <> ": " <> inpt
  Nothing   -> ""

showPos :: Maybe Int -> String 
showPos mInt = case mInt of 
  Just int  -> "Position: " <> (show int)
  Nothing   -> ""