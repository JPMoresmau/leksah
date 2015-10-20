{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
--
-- Module      :  IDE.GhcMod
-- Copyright   :  2007-2014 Juergen Nicklisch-Franken, Hamish Mackenzie
-- License     :  GPL
--
-- Maintainer  :  maintainer@leksah.org
-- Stability   :  provisional
-- Portability :
--
-- | Integration with ghc-mod
--
-----------------------------------------------------------------------------

module IDE.GhcMod (
    ghcModType
) where

import IDE.Core.State
import IDE.Pane.SourceBuffer
       (selectedRange,selectedModuleName,inActiveBufContext,IDEBuffer(..))
import IDE.Pane.Log (getDefaultLogLaunch)
import IDE.Utils.ExternalTool (runExternalTool')
import IDE.Workspaces (packageTry)
import IDE.LogRef
import IDE.Utils.GUIUtils
import IDE.Utils.Tool (ToolOutput(..),toolline)
import IDE.TextEditor

import Control.Concurrent
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad (when, void)
import Control.Monad.Trans.Reader (ask)

import Data.Either
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.Read as T

import qualified Data.Conduit as C (Sink, ZipSink(..), getZipSink)
import qualified Data.Conduit.List as CL (foldM, fold, consume)
import Data.Conduit (($$))

import System.Exit (ExitCode(..))

import Graphics.UI.Gtk

-- Display ghc-mod type of the selected text
ghcModType :: FilePath -> IDEAction
ghcModType ghcModPath = do
    rg <- selectedRange
    maybeModuleName <- selectedModuleName
    case (rg,maybeModuleName) of
        (Just leksahSel,Just mn) -> do
            let ghcModSel@((sl,sc),_) = toGhcModSel leksahSel
            inActiveBufContext Nothing $ \_ edView eBuf ideBuf _ -> do
                let mfn = fileName ideBuf
                case mfn of
                  Just fn -> getToolOutput ghcModPath (["type",T.pack fn, T.pack $ show sl,T.pack $ show sc]) $ \types -> do
                    let mMatching= matchingType ghcModSel $ toTypeResults types
                    case mMatching of
                        Nothing -> return ()
                        Just matching -> showPopupText edView eBuf $ trText matching
                  Nothing -> return ()
                return $ Just ()
            return ()
        _ -> return ()

-- | Show the given text in popup over the editor selection
showPopupText :: (TextEditor editor) => (EditorView editor) -> (EditorBuffer editor) -> T.Text -> PackageAction
showPopupText edView eBuf txt = do
        window <- liftIDE getMainWindow
        mbDrawWindow <- liftIDE $ getWindow edView
        case mbDrawWindow of
            Nothing -> return ()
            Just drawWindow -> do
                (start, end) <- liftIDE $ getSelectionBounds eBuf
                Rectangle x y _ height <- liftIDE $ getIterLocation edView start
                (wx,wy)<-liftIDE $ bufferToWindowCoords edView (x,y+height)
                liftIO $ do
                    (mwx, mwy)  <- drawWindowGetOrigin drawWindow
                    popup <- windowNew -- to be able to get focus
                    set popup [
                         windowTypeHint        := WindowTypeHintUtility,
                         windowDecorated     := False,
                         windowResizable       := False,
                         windowTransientFor  := window]
                    lbl <- labelNew (Just txt)
                    set lbl [labelSelectable := True]
                    containerAdd popup lbl
                    popup `on` focusOutEvent $ do
                        liftIO $ widgetDestroy popup
                        return True
                    widgetShowAll popup
                    windowMove popup (wx+mwx) (wy+mwy)
                    widgetGrabFocus popup
        return ()

-- | Run the given executable with the given arguments in the current package folder
-- Gather all output lines and feed to given function
getToolOutput :: FilePath -> [T.Text]
                        -> ([T.Text] -> PackageAction) -> IDEAction
getToolOutput ghcModPath args f= packageTry $ do
    package <- ask
    logLaunch <- getDefaultLogLaunch
    mvar <- liftIO newEmptyMVar
    runExternalTool' (__ "ghc-mod type")
            ghcModPath args
            (ipdPackageDir package) $ do
                output <- CL.consume
                liftIO . putMVar mvar $ case take 1 $ reverse output of
                    [ToolExit ExitSuccess] ->
                        catMaybes $ map outputOnly output
                    _ -> []
    out <- liftIO $ takeMVar mvar
    f out

-- | Transform the text viewer selection (0 based), into a ghc-mod selection (1-base)
toGhcModSel :: ((Int,Int),(Int,Int)) -> ((Int,Int),(Int,Int))
toGhcModSel ((a,b),(c,d))=((a+1,b+1),(c+1,d+1))

-- | Get only the normal output from a tool
outputOnly :: ToolOutput -> Maybe T.Text
outputOnly (ToolOutput l)=Just l
outputOnly _ = Nothing

-- | One type result from ghc-mod
data TypeResult = TypeResult
    { trStart :: (Int,Int)
    ,  trEnd :: (Int,Int)
    ,  trText :: T.Text
    } deriving (Show,Read,Eq,Ord)

-- | Transform output from ghc-mod type into TypeResults
toTypeResults :: [T.Text] -> [TypeResult]
toTypeResults = rights . map readTypeResult

-- | Parse one type result
readTypeResult :: T.Text -> Either String TypeResult
readTypeResult t = do
    (sl,r0) <- T.decimal t
    (sc,r1) <- T.decimal $ T.stripStart r0
    (el,r2) <- T.decimal $ T.stripStart r1
    (ec,r3) <- T.decimal $ T.stripStart r2
    let typ = T.dropEnd 1 $ T.drop 1 $ T.stripStart r3
    return $ TypeResult (sl,sc) (el,ec) typ

-- | Find the best matching type from the selection
matchingType :: ((Int,Int),(Int,Int)) -> [TypeResult] -> Maybe TypeResult
matchingType _ [] = Nothing
matchingType (a,b) (x:_) | a==b = Just x -- single point: take the smallest type
matchingType (a,b) xs = listToMaybe $ filter (matchResult a b) xs -- take the smallest type
                                                                                                        -- encompassing the full selection range
    where
        matchResult s1 e1 tr = matchPos s1 e1 (trStart tr) (trEnd tr)
        matchPos  s1 e1 s2 e2 = s1 `after` s2 && e1 `before` e2
        before (l1,c1) (l2,c2) = l1 < l2 || (l1==l2 && c1<=c2)
        after (l1,c1) (l2,c2) = l1 > l2 || (l1==l2 && c1>=c2)
