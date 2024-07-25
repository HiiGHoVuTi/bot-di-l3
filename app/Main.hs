{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

module Main where

import Configuration.Dotenv
import Control.Monad (void, when)
import Data.Functor ((<&>))
import Data.Maybe (fromJust)
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
  MessageCreate m -> when (isPing m && not (fromBot m)) do
    void $ restCall (R.CreateReaction (messageChannelId m, messageId m) "eyes")
    threadDelay (2 * 10 ^ 6)
    void $ restCall (R.CreateMessage (messageChannelId m) "Pong!")
  _ -> pure ()

fromBot :: Message -> Bool
fromBot = userIsBot . messageAuthor

isPing :: Message -> Bool
isPing = ("ping" `Text.isPrefixOf`) . Text.toLower . messageContent
