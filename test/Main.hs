{-# LANGUAGE OverloadedStrings #-}

import           Control.Exception
import           Control.Exception       as E
import           Control.Lens            hiding ((.=))
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.State
import           Control.Monad.State
import           Data.Aeson
import qualified Data.ByteString.Char8   as BC
import qualified Data.ByteString.Lazy    as BL
import           Data.Openshift
import           Network.Connection      (TLSSettings (..))
import qualified Network.HTTP.Client     as C
import           Network.HTTP.Client.TLS (mkManagerSettings)
import           Network.HTTP.Types
import qualified Network.Wreq            as W
import qualified Network.Wreq.Session    as S
import           System.Environment
import           System.Exit
import           System.IO


data OOSession = NoSession
               | OOSession { session :: S.Session
                           } deriving Show


handler :: S.Session -> String -> C.HttpException -> IO (W.Response BL.ByteString)
handler session url e@(C.StatusCodeException status headers cookies) =
    do let bearer = BC.unpack $
                    BC.takeWhile (/= '&') $
                    BC.tail $
                    BC.dropWhile (/= '=') $
                    snd . head $ filter (\(a, b) -> a == hLocation) headers
           opts = W.defaults & W.header "Authorization" .~ [BC.pack $ concat ["Bearer ", bearer]]
       r <- S.deleteWith opts session (url ++ "/oapi/v1/oauthaccesstokens/" ++ bearer)


       let d = (decode (r ^. W.responseBody)) :: Maybe UnversionedStatus
       print d
       print $ (r ^. W.responseBody)


       return r




login :: String -> String -> String -> StateT OOSession IO S.Session
login user password url =
  do session <- lift $ S.withSessionControl Nothing
                  (mkManagerSettings (TLSSettingsSimple True False False) Nothing)
                  (\session ->
                     do let opts = W.defaults & W.auth ?~ W.basicAuth (BC.pack user) (BC.pack password)
                                              & W.header "X-CSRF-Token" .~ ["xxx"]
                                              & W.param "client_id" .~ ["openshift-challenging-client"]
                                              & W.param "response_type" .~ ["token"]
                                              & W.redirects .~ 0

                        r <- S.getWith opts session (url ++ "/oauth/authorize")
                                 `E.catch` (handler session url)

                        return session
                  )

     put (OOSession session)
     lift $ return session



getUsername :: IO String
getUsername = do
    putStr "Username: "
    hFlush stdout
    pass <- withEcho True getLine
    putChar '\n'
    return pass

getPassword :: IO String
getPassword = do
    putStr "Password: "
    hFlush stdout
    pass <- withEcho False getLine
    putChar '\n'
    return pass

withEcho :: Bool -> IO a -> IO a
withEcho echo action = do
    old <- hGetEcho stdin
    bracket_ (hSetEcho stdin echo) (hSetEcho stdin old) action


main :: IO ()
main = do
    args <- getArgs

    when (length args /= 1) $ do
        putStrLn "Usage: oo-cli host"
        exitFailure

    let host = head args

    username <- getUsername
    password <- getPassword


    session <- execStateT (login username password ("https://" ++ host)) NoSession

    putStrLn "Closing openshift client."
    exitSuccess
