{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RankNTypes #-}

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.DocTest.Helpers where

import Debug.Trace
import Data.List
import GHC.Stack (HasCallStack)

import System.Directory
  ( canonicalizePath, doesFileExist )
import System.FilePath ((</>), isDrive, takeDirectory)
import System.FilePath.Glob (glob)

#if __GLASGOW_HASKELL__ < 804
import Data.Monoid ((<>))
#endif

import qualified Data.Set as Set

-- Cabal
import Distribution.ModuleName (ModuleName)
import Distribution.Simple
  ( Extension (DisableExtension, EnableExtension, UnknownExtension) )
import Distribution.Types.UnqualComponentName ( unUnqualComponentName )
import Distribution.PackageDescription hiding (Library)
  -- ( CondTree(..), GenericPackageDescription (..)
  -- , exposedModules, libBuildInfo, hsSourceDirs, defaultExtensions, package
  -- , packageDescription, condSubLibraries, includeDirs, autogenModules )

import Distribution.Fields.Pretty
import Distribution.Pretty (prettyShow)
import Distribution.PackageDescription.PrettyPrint
import Distribution.CabalSpecVersion (cabalSpecLatest)
import Distribution.Verbosity (silent)

#if MIN_VERSION_Cabal(3,6,0)
import Distribution.Utils.Path (SourceDir, PackageDir, SymbolicPath)
#endif

-- cabal-install-parsers
import Distribution.PackageDescription.Parsec (readGenericPackageDescription)

import qualified Data.Map as Map
import Distribution.PackageDescription.Check (PackageCheck(..))
import Distribution.Types.CondTree

-- | Efficient implementation of set like deletion on lists
--
-- >>> "abcd" `rmList` "ad"
-- "bc"
-- >>> "aaabcccd" `rmList` "ad"
-- "bccc"
rmList :: Ord a => [a] -> [a] -> [a]
rmList xs (Set.fromList -> ys) = filter (not . (`Set.member` ys)) xs

data Library = Library
  { libSourceDirectories :: [FilePath]
    -- ^ Haskell source directories
  , libCSourceDirectories :: [FilePath]
    -- ^ C source directories
  , libModules :: [ModuleName]
    -- ^ Exposed modules
  , libDefaultExtensions :: [Extension]
    -- ^ Extensions enabled by default
  }
  deriving (Show)

-- | Convert a "Library" to arguments suitable to be passed to GHCi.
libraryToGhciArgs :: Library -> ([String], [String], [String])
libraryToGhciArgs Library{..} = (hsSrcArgs <> cSrcArgs, modArgs, extArgs)
 where
  hsSrcArgs = map ("-i" <>) libSourceDirectories
  cSrcArgs = map ("-I" <>) libCSourceDirectories
  modArgs = map prettyShow libModules
  extArgs = map showExt libDefaultExtensions

  showExt = \case
    EnableExtension ext -> "-X" <> show ext
    DisableExtension ext -> "-XNo" <> show ext
    UnknownExtension ext -> "-X" <> ext

-- | Drop a number of elements from the end of the list.
--
-- > dropEnd 3 "hello"  == "he"
-- > dropEnd 5 "bye"    == ""
-- > dropEnd (-1) "bye" == "bye"
-- > \i xs -> dropEnd i xs `isPrefixOf` xs
-- > \i xs -> length (dropEnd i xs) == max 0 (length xs - max 0 i)
-- > \i -> take 3 (dropEnd 5 [i..]) == take 3 [i..]
dropEnd :: Int -> [a] -> [a]
dropEnd i xs
  | i <= 0 = xs
  | otherwise = f xs (drop i xs)
 where
   f (a:as) (_:bs) = a : f as bs
   f _ _ = []

-- Searches for a file called @package.cabal@, where @package@ is given as an
-- argument. It will look for it in the current directory. If it can't find it
-- there, it will traverse up until it finds the file or a file called
-- @cabal.project@. In case of the latter, it will traverse down recursively
-- until it encounters a @package.cabal@.
--
-- The returned path points to the @package.cabal@. Errors if it could not
-- find @package.cabal@ anywhere, or when it found multiple.
--
findCabalPackage :: HasCallStack => String -> IO FilePath
findCabalPackage packageName = goUp =<< canonicalizePath packageName
 where
  goUp :: FilePath -> IO FilePath
  goUp path
    | isDrive path = error ("Could not find '" <> packageFilename <> "'")
    | otherwise = do
      packageExists <- doesFileExist (path </> packageFilename)
      projectExists <- doesFileExist (path </> projectFilename)

      if | packageExists -> pure (path </> packageFilename)
         | projectExists -> goDown path
         | otherwise -> goUp (takeDirectory path)

  goDown :: FilePath -> IO FilePath
  goDown path = do
    candidates <- glob (path </> "**" </> packageFilename)
    case candidates of
      [] -> error ("Could not find " <> packageFilename <> " in project " <> path)
      (_:_:_) -> error ("Ambiguous packages in project " <> path <> ": " <> show candidates)
      [c] -> pure c

  packageFilename = packageName <> ".cabal"
  projectFilename = "cabal.project"

#if MIN_VERSION_Cabal(3,6,0)
compatPrettyShow :: SymbolicPath PackageDir SourceDir -> FilePath
compatPrettyShow = prettyShow
#else
compatPrettyShow :: FilePath -> FilePath
compatPrettyShow = id
#endif

-- Given a filepath to a @package.cabal@, parse it, and yield a "Library". Yields
-- the default Library if first argument is Nothing, otherwise it will look for
-- a specific sublibrary.
extractSpecificCabalLibrary :: Maybe String -> FilePath -> IO Library
extractSpecificCabalLibrary maybeLibName pkgPath = do
  pkg <- readGenericPackageDescription silent pkgPath
  -- _ <- print $ showFields (const []) $ ppGenericPackageDescription cabalSpecLatest pkg
  case maybeLibName of
    Nothing ->
      case condLibrary pkg of
        Nothing ->
          let pkgDescription = package (packageDescription pkg) in
          error ("Could not find main library in: " <> show pkgDescription)
        Just lib ->
          go lib

    Just libName ->
      go (findSubLib pkg libName (condSubLibraries pkg))

 where
  findSubLib pkg targetLibName [] =
    let pkgDescription = package (packageDescription pkg) in
    error ("Could not find library " <> targetLibName <> " in " <> show pkgDescription)
  findSubLib pkg targetLibName ((libName, lib):libs)
    | unUnqualComponentName libName == targetLibName = lib
    | otherwise = findSubLib pkg targetLibName libs

  go xx@CondNode{condTreeData=lib} =
    let
      -- buildInfo = libBuildInfo lib
      buildInfo = libBuildInfo (fst $ ignoreConditions xx)
      sourceDirs = hsSourceDirs buildInfo
      cSourceDirs = includeDirs buildInfo
      root = takeDirectory pkgPath
    in
      traceShow xx $
      pure Library
        { libSourceDirectories = map ((root </>) . compatPrettyShow) sourceDirs
        , libCSourceDirectories = map (root </>) cSourceDirs
        , libModules = exposedModules lib `rmList` autogenModules buildInfo
        -- , libDefaultExtensions = defaultExtensions $ trace ("BUILD-INFO: " ++ show buildInfo) buildInfo
        , libDefaultExtensions = defaultExtensions buildInfo
        }


-- Given a filepath to a @package.cabal@, parse it, and yield a "Library". Returns
-- and error if no library was specified in the cabal package file.
extractCabalLibrary :: FilePath -> IO Library
extractCabalLibrary = extractSpecificCabalLibrary Nothing

checkDuplicateModules :: GenericPackageDescription -> [PackageCheck]
checkDuplicateModules pkg =
       concatMap checkLib   (maybe id (:) (condLibrary pkg) . map snd $ condSubLibraries pkg)
  where
    -- the duplicate modules check is has not been thoroughly vetted for backpack
    checkLib   = checkDups "library" (\l -> explicitLibModules l ++ map moduleReexportName (reexportedModules l))
    checkDups s getModules t =
               let sumPair (x,x') (y,y') = (x + x' :: Int, y + y' :: Int)
                   mergePair (x, x') (y, y') = (x + x', max y y')
                   maxPair (x, x') (y, y') = (max x x', max y y')
                   libMap = foldCondTree Map.empty
                                         (\(_,v) -> Map.fromListWith sumPair . map (\x -> (x,(1, 1))) $ getModules v )
                                         (Map.unionWith mergePair) -- if a module may occur in nonexclusive branches count it twice strictly and once loosely.
                                         (Map.unionWith maxPair) -- a module occurs the max of times it might appear in exclusive branches
                                         t
                   dupLibsStrict = Map.keys $ Map.filter ((>1) . fst) libMap
                   dupLibsLax = Map.keys $ Map.filter ((>1) . snd) libMap
               in if not (null dupLibsLax)
                      then [PackageBuildImpossible $ "Duplicate modules in " ++ s ++ ": " ++ commaSep (map prettyShow dupLibsLax)]
                      else if not (null dupLibsStrict)
                           then [PackageDistSuspicious $ "Potential duplicate modules (subject to conditionals) in " ++ s ++ ": " ++ commaSep (map prettyShow dupLibsStrict)]
                           else []

                           -- | Flatten a CondTree. This will traverse the CondTree by taking all
--  possible paths into account, but merging inclusive when two paths
--  may co-exist, and exclusively when the paths are an if/else
foldCondTree :: forall b c a v. b -> ((c, a) -> b) -> (b -> b -> b) -> (b -> b -> b) -> CondTree v c a -> b
foldCondTree e u mergeInclusive mergeExclusive = goTree
  where
    goTree :: CondTree v c a -> b
    goTree (CondNode a c ifs) = u (c, a) `mergeInclusive` foldl goBranch e ifs
    goBranch :: b -> CondBranch v c a -> b
    goBranch acc (CondBranch _ t mt) = mergeInclusive acc (maybe (goTree t) (mergeExclusive (goTree t) . goTree) mt)

commaSep :: [String] -> String
commaSep = intercalate ", "