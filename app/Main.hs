{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

module Main where

import Configuration.Dotenv
import Control.Monad (void, when)
import Control.Monad.IO.Class (liftIO)
import Data.Char
import Data.Functor ((<&>))
import Data.Maybe (fromJust)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as TIO
import Discord
import Discord.Requests qualified as R
import Discord.Types
import UnliftIO.Concurrent (threadDelay)

main :: IO ()
main = do
  token <-
    parseFile ".env"
      <&> Text.pack
        . fromJust
        . lookup "DISCORD_TOKEN"
  TIO.putStrLn
    =<< runDiscord
      def
        { discordToken = token,
          discordOnEvent = eventHandler,
          discordOnLog = TIO.putStrLn
        }

eventHandler :: Event -> DiscordHandler ()
eventHandler = \case
  MessageCreate m -> do
    when (isPing m && not (fromBot m)) do
      void $ restCall (R.CreateReaction (messageChannelId m, messageId m) "eyes")
      threadDelay (2 * 10 ^ 6)
      void $ restCall (R.CreateMessage (messageChannelId m) "Pong!")
    when (isCapClair m && not (fromBot m)) do
      void $ restCall (R.CreateMessage (messageChannelId m) glucksmannLink)
  _ -> pure ()

fromBot :: Message -> Bool
fromBot = userIsBot . messageAuthor

isPing :: Message -> Bool
isPing = ("ping" `Text.isPrefixOf`) . Text.toLower . messageContent

glucksmannLink :: Text
glucksmannLink = "https://cdn.discordapp.com/attachments/1264010718563270708/1264680541521838131/An_uAjxiF2sxvurM-hiLui2FhOYsJjVTd7yEwDZuOY2JFj9rtHHK1rX0_YIDAM5hQXNdvapmfRGHLmMACp6uJZmj.mp4?ex=66a40709&is=66a2b589&hm=d36cea0d7ba5fb612c65a22050b2914e929006d26fea30b00850628dd1b68303&"

isCapClair :: Message -> Bool
isCapClair =
  ("capclair" `Text.isSuffixOf`)
    . Text.filter isLetter
    . Text.toLower
    . messageContent
