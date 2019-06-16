-- | Result and state handling
--
-- @since 0.1.0
module ParserResult ( ParserResult, amount, initParserStep, parseStepIO, send ) where

import           Control.Exception          ( displayException, try )

import           Data.Aeson                 ( encodeFile )
import           Data.ByteString.Lazy.Char8 as C ( unpack )

import           GoParser                   ( parse )

import           Log                        ( debug, err, notice, noticeR )

import           Model                      ( Benchmark, Environment, ReqBody )

import           Network.HTTP.Simple
                 ( HttpException, Request, getResponseBody
                 , getResponseStatusCode, httpLBS, parseRequest
                 , setRequestBodyJSON )

import           Parser                     ( State(Failure, Init, Ok) )

import           System.Directory           ( removePathForcibly )
import           System.Exit                ( exitFailure )
import           System.IO.Temp             ( emptySystemTempFile )

import           Text.Printf                ( printf )

-- | The result of the complete run
type ParserResult = [Benchmark]

-- | A single parser step consists of an intermediate state and result
type ParserStep = (State, ParserResult)

-- | Initial parser step for convenience
initParserStep :: ParserStep
initParserStep = (Init, [])

-- | Go one step forward and log output
parseStepIO :: ParserStep -> String -> IO ParserStep
parseStepIO s line = do
    noticeR line
    let r = parseStep s line
    debugStep r
    return r

-- | Go one step forward by parsing the input String
parseStep :: ParserStep -> String -> ParserStep
parseStep (s, r) i = let ns = parse s i in (ns, appendBenchmark ns r)

-- | Append the succeeding result if possible
appendBenchmark :: State -> ParserResult -> ParserResult
appendBenchmark (Ok b) r = r ++ pure b
appendBenchmark _ r = r

-- | Retrieve the amount of benchmark results for the provided ParserStep
amount :: ParserStep -> Int
amount (_, r) = length r

-- | Print a debug message for the current step
debugStep :: ParserStep -> IO ()
debugStep (Failure f, r) = do
    debug $ printf "Parse error: %s" f
    debugResult r
debugStep (_, r) = debugResult r

-- | Print a debug message for the current result
debugResult :: ParserResult -> IO ()
debugResult r = debug . printf "Current result: %s" $ show r

-- | Store the current result on disk
toDisk :: ReqBody -> IO FilePath
toDisk b = do
    f <- emptySystemTempFile "result-.json"
    debug $ printf "Writing to temp file: %s" f
    encodeFile f b
    return f

-- | Sen the provided data to the given url including the environment
send :: ParserStep -> String -> Environment -> IO ()
send (_, r) u e = do
    let body = (e, r)
    p <- toDisk body
    request <- buildRequest u p
    debug . printf "Doing HTTP request:\n%s" $ show request
    doRequest request body p

-- | Do the provided request
doRequest :: Request -> ReqBody -> FilePath -> IO ()
doRequest r b p = do
    response <- try . httpLBS $ setRequestBodyJSON b r
    case response of
        Right res -> case getResponseStatusCode res of
            200 -> do
                notice "Successfully sent"
                removePathForcibly p
                debug "Removed temp file"
            code -> do
                err $ printf "Got wrong HTTP status code: %d" code
                debug . printf "Got sesponse:\n%s" . C.unpack $
                    getResponseBody res
                logFilePath p
        Left exception -> do
            err "Unable to do HTTP request"
            debug . printf "Exception details:\n%s" $
                displayException (exception :: HttpException)
            logFilePath p

-- | Build the HTTP post request from the given URL and path. The function does
-- earily exit on failure.
buildRequest :: String -> FilePath -> IO Request
buildRequest u p = do
    r <- try . parseRequest $ printf "POST %s" u
    case r :: Either HttpException Request of
        Right req -> return req
        Left _ -> do
            err $ printf "Invalid URL provided: %s" u
            logFilePath p
            exitFailure

-- | Log the file path convenience function
logFilePath :: FilePath -> IO ()
logFilePath p = notice $ printf "You can try to resend by using the file %s" p
