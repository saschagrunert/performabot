-- | Github related actions
--
-- @since 0.1.0
module Github ( baseCommit, comment ) where

import           Control.Lens                     ( (^.) )

import           Data.ByteString.Char8            as C ( pack )
import           Data.List                        ( find )
import           Data.Text                        as T ( Text, isInfixOf, pack )
import           Data.Vector                      ( Vector, toList )

import           Environment
                 ( Environment, owner, pullRequest, repository, token )

import           GitHub                           ( Auth(OAuth) )
import           GitHub.Data.Comments             ( Comment, commentHtmlUrl )
import           GitHub.Data.Definitions
                 ( Error, IssueNumber(IssueNumber), Owner )
import           GitHub.Data.Id                   ( Id(Id) )
import           GitHub.Data.Issues
                 ( IssueComment, issueCommentBody, issueCommentId )
import           GitHub.Data.Name                 ( Name(N) )
import           GitHub.Data.PullRequests
                 ( pullRequestBase, pullRequestCommitSha )
import           GitHub.Data.Repos                ( Repo )
import           GitHub.Data.URL                  ( getUrl )
import           GitHub.Endpoints.Issues.Comments
                 ( comments, createComment, editComment )
import           GitHub.Endpoints.PullRequests    ( pullRequest' )

import           Log                              ( debug, err, notice )

import           Pretty                           ( header )

import           System.Exit                      ( exitFailure )

import           Text.Printf                      ( printf )

-- | Authenticate from string token
getAuth :: Environment -> Auth
getAuth e = OAuth . C.pack $ e ^. token

-- | Comment the provided string
comment :: Environment -> String -> IO ()
comment e x = do
    p <- maybeInt $ e ^. pullRequest
    let a = getAuth e
        o = N . T.pack $ e ^. owner
        r = N . T.pack $ e ^. repository
        i = IssueNumber p
        l = T.pack x

    cs <- getComments o r i
    case isFromPerformabot $ toList cs of
        Just s -> do
            debug . printf "Found issue to edit: %s" $ show s
            cc <- editComment a o r (Id $ issueCommentId s) l
            handleCommentErr cc "edit"
        Nothing -> do
            debug "No issue found, creating new"
            cc <- createComment a o r i l
            handleCommentErr cc "create"

-- | Retrieve all comments
getComments
    :: Name Owner -> Name Repo -> IssueNumber -> IO (Vector IssueComment)
getComments o r i = do
    mcs <- comments o r i
    case mcs of
        Left f -> do
            err $ printf "Unable to retrieve comments"
            debug $ show f
            exitFailure
        Right cs -> return cs

-- | General comment error handling
handleCommentErr :: Either Error Comment -> String -> IO ()
handleCommentErr x t = case x of
    Left f -> do
        err $ printf "Unable to %s comment" t
        debug $ show f
        exitFailure
    Right c -> notice $
        printf "Comment %s successful: %s" t (urlOf $ commentHtmlUrl c)
  where
    urlOf Nothing = "Not found."
    urlOf (Just u) = getUrl u

-- | Finds comments from performabot
isFromPerformabot :: [IssueComment] -> Maybe IssueComment
isFromPerformabot = find $ T.isInfixOf (T.pack header) . issueCommentBody

-- | Retrieve the corresponding base commit for the pull request
baseCommit :: Environment -> IO T.Text
baseCommit e = do
    p <- maybeInt $ e ^. pullRequest
    i <- pullRequest' (Just $ getAuth e)
                      (N . T.pack $ e ^. owner)
                      (N . T.pack $ e ^. repository)
                      (IssueNumber p)
    case i of
        Left f -> do
            err . printf "Unable to retrieve pull request #%s" $
                e ^. pullRequest
            debug $ show f
            exitFailure
        Right pr -> do
            let c = pullRequestCommitSha $ pullRequestBase pr
            debug $ printf "Found PR base commit: %s" c
            return c

-- | Try to read the integer from the string and fail early if not possible
maybeInt :: String -> IO Int
maybeInt s = case reads s of
    [ (x, "") ] -> return x
    _ -> do
        err "Unable to parse pull request number"
        exitFailure
