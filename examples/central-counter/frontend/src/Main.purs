-- | TODO: This example could use a rewrite ;-)
module Main where

import Effect (Effect)
import Effect.Aff
import Control.Monad.Except.Trans
import Control.Monad.Reader.Trans
import Counter.ServerTypes
import Counter.WebAPI
-- import Data.Argonaut.Generic.Aeson
import Data.Either
-- import Data.Generic
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe
import Prelude
import Servant.PureScript.Ajax
import Servant.PureScript.Settings
-- import Counter.WebAPI.MakeRequests as MakeReq
import Data.Array as Array
import Control.Bind ((<=<))
-- import DOM.Node.Node (baseURI)
import Data.Argonaut.Core (Json)
import Data.Argonaut.Parser (jsonParser)
import Data.Bifunctor (lmap)
import Data.Foldable (foldr, fold, traverse_)
import Data.List (List(Nil, Cons))
import Data.Tuple (Tuple(Tuple))
import Foreign.Generic (defaultOptions)
import Pux (start, EffModel, noEffects)
import Pux.DOM.HTML (HTML)
import Pux.DOM.Events (onClick)
import Pux.Renderer.React (renderToDOM)
import Text.Smolder.HTML (button, span, div, p)
import Text.Smolder.Markup ((#!), text)
-- import Signal (Signal)
-- import Signal.Channel (Channel, subscribe, send, channel, CHANNEL)


data Action = Increment
            | Decrement
            | Update Int
            | ReportError AjaxError
            | SubscriberLog String
            | Nop

type State = {
    counter :: Int
  , lastError :: Maybe AjaxError
  , settings :: MySettings
  , subscriberLog :: List String
  }

type MySettings = SPSettings_ SPParams_

type APIEffect a = ReaderT MySettings (ExceptT AjaxError Aff) a

foldp :: Action -> State -> EffModel State Action
foldp Increment state = runEffectActions state [Update <$> putCounter (CounterAdd 1)]
foldp Decrement state = runEffectActions state [Update <$> putCounter (CounterAdd (-1))]
foldp (Update val) state  = noEffects state { counter = val }
foldp (ReportError err ) state = noEffects state { lastError = Just err }
foldp (SubscriberLog msg) state = noEffects state { subscriberLog = Cons msg state.subscriberLog }
foldp Nop state = noEffects state

view :: State -> HTML Action
view state =
  div do
    div do
      button #! onClick (const Increment) $ text "+"
      span $ text (show state.counter)
      button #! onClick (const Decrement) $text "-"
    div do
      span $ text $ "Error: " <> maybe "Nothing" errorToString state.lastError
      div do
        text "Log: "
        div do
          (traverse_ (p <<< text) $ state.subscriberLog)

runEffectActions :: State -> Array (APIEffect Action) -> EffModel State Action
runEffectActions state effects = { state : state, effects : map (runEffect state.settings) effects }

runEffect :: MySettings -> APIEffect Action -> Aff (Maybe Action)
runEffect settings m = do
    er <- runExceptT $ runReaderT m settings
    case er of
      Left err -> pure $ Just $ ReportError err
      Right v -> pure $ Just v

-- type SubscriberData = {
--   subscriber :: Subscriber Action
-- , messages :: Signal Action
-- }

-- initSubscriber :: forall eff. MySettings -> SubscriberEff (channel :: CHANNEL | eff) (SubscriberData (channel :: CHANNEL | eff))
-- initSubscriber settings = do
--   ch <- channel Nop
--   let
--     c :: Config (channel :: CHANNEL | eff) Action
--     c = {
--         url : "ws://localhost:8081/subscriber"
--       , notify : send ch <<< SubscriberLog <<< gShow
--       , callback : send ch
--       }
--   sub <- makeSubscriber c
--   let sig = subscribe ch
--   pongReq <- flip runReaderT settings $ MakeReq.putCounter (CounterAdd 1) -- | Let's play a bit! :-)
--   closeReq <- flip runReaderT settings $ MakeReq.putCounter (CounterSet 100)
--   subs <- flip runReaderT settings $ Sub.getCounter (maybe Nop Update)
--   let c = Subscriber.getConnection sub
--   C.setPongRequest pongReq c -- |< Hihi :-)
--   C.setCloseRequest closeReq c
--   Subscriber.deploy subs sub
--   pure $ { subscriber : sub, messages : sig }

main :: Effect Unit
main = do
  let settings = defaultSettings $ SPParams_ { authToken : VerySecret "topsecret"
                                             , baseURL : "http://localhost:8081/"
                                             }
  let initState = { counter : 0, settings : settings, lastError : Nothing, subscriberLog: Nil }
  app <- start
    { initialState: initState
    , foldp: foldp
    , view: view
    , inputs: []
    }
  renderToDOM "#app" app.markup app.input