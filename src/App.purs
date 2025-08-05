module App where

import Data.Maybe
import Effect
import Prelude

import Data.Array (filter, foldl)
import Data.String (length)
import Effect.Class (class MonadEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Web.Event.Event (target)
import Web.Event.Internal.Types (EventTarget)
import Web.HTML (HTMLTextAreaElement)
import Web.HTML.HTMLTextAreaElement (selectionStart, fromEventTarget)
import Web.UIEvent.KeyboardEvent (KeyboardEvent, key, toEvent)

type State 
  = { input :: Maybe String, pos :: Maybe Int }

data Action
  = Input String (Maybe EventTarget)

component :: forall q i o m. MonadEffect m => H.Component q i o m
component =
  H.mkComponent
    { initialState: \_ -> { input: Nothing, pos: Nothing }
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
  Input k maybeTarget -> case maybeTarget >>= fromEventTarget of
    Just el -> do
      pos <- H.liftEffect $ selectionStart el
      H.modify_ \st -> st { input = Just k, pos = Just pos }
    Nothing -> pure unit

getCursorPosition :: HTMLTextAreaElement -> Effect Int
getCursorPosition = selectionStart 
  
showState :: State -> String 
showState state = foldl (\acc txt -> acc <> " " <> txt) "" 
  $ filter (\str -> length str > 0) [showInput state.input,showPos state.pos]

showInput :: Maybe String -> String 
showInput mInput = case mInput of 
  Just inpt -> "Input: " <> inpt
  Nothing   -> ""

showPos :: Maybe Int -> String 
showPos mInt = case mInt of 
  Just int  -> "Position: " <> (show int)
  Nothing   -> ""