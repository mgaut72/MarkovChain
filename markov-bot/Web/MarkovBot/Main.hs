module Web.MarkovBot.Main where
import Data.List
import Network
import System.IO
import System.Exit
import Control.Monad.State
import Control.Exception
import Text.Printf
import Prelude hiding (catch)
import Data.Monoid
import Data.MarkovChain

server = "irc.freenode.org"
port   = 6667
chan   = "#tutbot-testing"
nick   = "MarkovBot"
ownerNick = "mgaut72"

--
-- The 'Net' monad, a wrapper over IO, carrying the bot's immutable state.
-- A socket and the markov chain structure
--
type Net = StateT Bot IO
data Bot = Bot { socket :: Handle
               , mChain :: MarkovChain String
               }

--
-- Set up actions to run on start and end, and run the main loop
--
main :: IO ()
main = bracket connect disconnect loop
  where
    disconnect = hClose . socket
    loop st    = catch (evalStateT run st) doNothing
    doNothing :: IOException -> IO ()
    doNothing = const $ return ()

--
-- Connect to the server and return the initial bot state
--
connect :: IO Bot
connect = notify $ do
    h <- connectTo server (PortNumber (fromIntegral port))
    hSetBuffering h NoBuffering
    return (Bot h mempty)
  where
    notify a = bracket_
        (printf "Connecting to %s ... " server >> hFlush stdout)
        (putStrLn "done.")
        a

--
-- We're in the Net monad now, so we've connected successfully
-- Join a channel, and start processing commands
--
run :: Net ()
run = do
    write "NICK" nick
    write "USER" (nick++" 0 * :tutorial bot")
    write "JOIN" chan
    bot <- get
    listen $ socket bot

--
-- Process each line from the server
--
listen :: Handle -> Net ()
listen h = forever $ do
    s <- init `fmap` io (hGetLine h)
    io (putStrLn s)
    if ping s
       then pong s
       else if (':':ownerNick) `isPrefixOf` s then eval (clean s) else return ()
  where
    forever a = a >> forever a
    clean     = drop 1 . dropWhile (/= ':') . drop 1
    ping x    = "PING :" `isPrefixOf` x
    pong x    = write "PONG" (':' : drop 6 x)

--
-- Dispatch a command
--
eval :: String -> Net ()
eval     "!MBquit"               = write "QUIT" ":Exiting" >> io (exitWith ExitSuccess)
eval     "!MBspeak"              = speak
eval x | "!MBid " `isPrefixOf` x = privmsg (drop 6 x)
eval x                           = expandVocabulary x

expandVocabulary :: String -> Net ()
expandVocabulary x = modify updateBot
  where updateBot b = b { mChain = mappend newChain (mChain b) }
        newChain = markovChain $ words x

speak :: Net ()
speak = do
  bot <- get
  privmsg $ concat $ traverse' (mChain bot) 15

--
-- Send a privmsg to the current chan + server
--
privmsg :: String -> Net ()
privmsg s = write "PRIVMSG" (chan ++ " :" ++ s)

--
-- Send a message out to the server we're currently connected to
--
write :: String -> String -> Net ()
write s t = do
    bot <- get
    let h = socket bot
    io $ hPrintf h "%s %s\r\n" s t
    io $ printf    "> %s %s\n" s t

--
-- Convenience.
--
io :: IO a -> Net a
io = liftIO
