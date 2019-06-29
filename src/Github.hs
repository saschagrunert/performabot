-- | Github related actions
--
-- @since 0.1.0
module Github ( baseCommit, comment, retrieveRemoteFile, updateRemoteFile ) where

import           Control.Lens                     ( (^.) )

import           Data.ByteString.Base64           as B ( decode, encode )
import           Data.ByteString.Char8            as C
                 ( pack, readFile, writeFile )
import           Data.List                        ( find )
import           Data.Text                        as T
                 ( Text, filter, isInfixOf, pack, unpack )
import           Data.Text.Encoding               ( decodeUtf8, encodeUtf8 )
import           Data.Vector                      ( Vector, toList )

import           Default                          ( db, repo )

import           Environment
                 ( Environment, owner, pullRequest, repository, token )

import           GitHub                           ( Auth(OAuth) )
import           GitHub.Data.Comments             ( Comment, commentHtmlUrl )
import           GitHub.Data.Content
                 ( Content(ContentFile), ContentFileData, CreateFile(CreateFile)
                 , UpdateFile(UpdateFile), contentFileContent, contentFileInfo
                 , contentSha, createFileAuthor, createFileBranch
                 , createFileCommitter, createFileContent, createFileMessage
                 , createFilePath, updateFileAuthor, updateFileBranch
                 , updateFileCommitter, updateFileContent, updateFileMessage
                 , updateFilePath, updateFileSHA )
import           GitHub.Data.Definitions
                 ( Error, IssueNumber(IssueNumber), Owner, simpleOwnerLogin )
import           GitHub.Data.Id                   ( Id(Id) )
import           GitHub.Data.Issues
                 ( IssueComment, issueCommentBody, issueCommentId )
import           GitHub.Data.Name                 ( Name(N) )
import           GitHub.Data.PullRequests
                 ( pullRequestBase, pullRequestCommitSha )
import           GitHub.Data.Repos
                 ( Repo, RepoPublicity(RepoPublicityOwner), newRepo, repoName
                 , repoOwner )
import           GitHub.Data.URL                  ( getUrl )
import           GitHub.Endpoints.Issues.Comments
                 ( comments, createComment, editComment )
import           GitHub.Endpoints.PullRequests    ( pullRequest' )
import           GitHub.Endpoints.Repos
                 ( createRepo', currentUserRepos )
import           GitHub.Endpoints.Repos.Contents
                 ( contentsFor, createFile, updateFile )

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
    urlOf Nothing = "Not found"
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

-- | Update or create the remote database file
updateRemoteFile :: Environment -> (Either Error Content, Name Owner) -> IO ()
updateRemoteFile e (contents, o) = do
    file <- C.readFile $ T.unpack db
    let b64Content = decodeUtf8 $ B.encode file
    case contents of
        Left _ -> createDatabaseFile (getAuth e) o b64Content
        Right (ContentFile ff) ->
            updateDatabaseFile (getAuth e) o b64Content ff
        _ -> do
            err "Wrong file type for database found remotely"
            exitFailure

-- | Download the remote databasefile if available
retrieveRemoteFile :: Environment -> IO (Either Error Content, Name Owner)
retrieveRemoteFile e = do
    r <- createRepoIfNeeded $ getAuth e
    let o = simpleOwnerLogin $ repoOwner r
    contents <- contentsFor o defaultRepoName db Nothing
    case contents of
        Left _ -> do
            notice "File does not exist remotely, skipping download"
            return (contents, o)
        Right (ContentFile f) -> do
            notice "File download successful"
            let content = B.decode . encodeUtf8 . T.filter (/= '\n') $
                    contentFileContent f
            case content of
                Left fd -> do
                    err $ printf "Unable to decode retrieved bytestring: %s" fd
                    exitFailure
                Right bs -> do
                    C.writeFile (T.unpack db) bs
                    notice "File successfully written to disk"
            return (contents, o)
        _ -> do
            err "Wrong file type for database found remotely"
            exitFailure

-- | The default repo name used for data upload
defaultRepoName :: Name Repo
defaultRepoName = N repo

-- | Create the default repository or simply retrieve it
createRepoIfNeeded :: Auth -> IO Repo
createRepoIfNeeded a = do
    notice $ printf "Searching for default storage repository: %s" repo
    repos <- currentUserRepos a RepoPublicityOwner
    case repos of
        Left f -> do
            err "Unable to retrieve repositories"
            debug $ show f
            exitFailure
        Right rs -> do
            debug "Found repositories"
            let repoExists = find (\x -> defaultRepoName == repoName x)
                                  (toList rs)
            case repoExists of
                Nothing -> do
                    notice "Default repository does not seem to exist, creating it"
                    created <- createRepo' a (newRepo defaultRepoName)
                    case created of
                        Left f -> do
                            err "Unable to create new repository"
                            debug $ show f
                            exitFailure
                        Right r -> do
                            notice "Default repository created"
                            return r
                Just r -> do
                    notice "Remote repo already exists"
                    return r

-- | Create the file withinthe default repository
createDatabaseFile :: Auth -> Name Owner -> Text -> IO ()
createDatabaseFile a o c = do
    -- Create the file
    notice "Unable to retrieve the database file, creating it"
    create <- createFile a
                         o
                         defaultRepoName
                         CreateFile { createFilePath      = db
                                    , createFileMessage   = "Create database"
                                    , createFileContent   = c
                                    , createFileBranch    = Nothing
                                    , createFileAuthor    = Nothing
                                    , createFileCommitter = Nothing
                                    }
    case create of
        Left f -> do
            err "Unable to create database file"
            debug $ show f
            exitFailure
        _ -> notice "File creation successful"

-- | Update the file withinthe default repository
updateDatabaseFile :: Auth -> Name Owner -> Text -> ContentFileData -> IO ()
updateDatabaseFile a o c f = do
    update <- updateFile a
                         o
                         defaultRepoName
                         UpdateFile { updateFilePath      = db
                                    , updateFileMessage   = "Update database"
                                    , updateFileContent   = c
                                    , updateFileSHA       =
                                          contentSha $ contentFileInfo f
                                    , updateFileBranch    = Nothing
                                    , updateFileAuthor    = Nothing
                                    , updateFileCommitter = Nothing
                                    }
    case update of
        Left uf -> do
            err "Unable to update database file"
            debug $ show uf
            exitFailure
        _ -> notice "File update successful"

