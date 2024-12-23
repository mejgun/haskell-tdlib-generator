module Save (genData, genFuncs, getClasses) where

import Data.List (find, groupBy, nub, sort)
import Data.Text qualified as T
import Haskell.Data (generateBoot, generateData, generateGeneralResult)
import Haskell.Func (generateFunc)
import Haskell.Internal (classToDataClass, methodToFunc, upFst)
import Parser
  ( Arg (..),
    ArgVal (..),
    Class (..),
    ClassName (ClassName),
    Method (..),
    result,
  )

dataDir :: String
dataDir = "/TD/Data/"

grDir :: String
grDir = "/TD/"

funcDir :: String
funcDir = "/TD/Query/"

genData :: FilePath -> [Class] -> [Method] -> [ClassName] -> [(FilePath, T.Text)]
genData path classes methods requestClasses = do
  map (setFileName ".hs" . f (map (map fst) boots)) listwClass
    <> map (setFileName ".hs-boot") (concat boots)
  where
    boots :: [[(ClassName, T.Text)]]
    boots = generateBoot importsList requests

    requests =
      let f1 c = filter (\(k, _) -> k == c) importsList
          f2 = concatMap (\(k, v) -> k : v)
          f3 = nub . concatMap (f2 . f1)
          go old new = if old == new then old else new ++ go new (f3 new)
       in go [] requestClasses

    list :: [[Method]]
    list = groupBy (\a b -> a.result == b.result) methods

    listwClass :: [(ClassName, Maybe Class, [Method])]
    listwClass =
      map
        ( \ms ->
            let m = head ms
             in ( m.result,
                  find (\c -> c.name == m.result) classes,
                  ms
                )
        )
        list

    importsList :: [(ClassName, [ClassName])]
    importsList =
      map
        (\(n, _, ms) -> (n, concatMap getModules ms))
        listwClass
      where
        go (TModule mn) = [ClassName (upFst mn)]
        go (TVector v) = go v
        go _ = []

        getModules :: Method -> [ClassName]
        getModules m =
          concatMap (\a -> go a.value) m.args

    f :: [[ClassName]] -> (ClassName, Maybe Class, [Method]) -> (ClassName, T.Text)
    f bts (name, mbc, ms) =
      (name, generateData bts requests (classToDataClass mbc ms))

    setFileName :: String -> (ClassName, T.Text) -> (FilePath, T.Text)
    setFileName suffix (c, text) = (fileName c suffix, text)

    fileName :: ClassName -> String -> FilePath
    fileName (ClassName name) suffix =
      path <> dataDir <> T.unpack name <> suffix

getClasses :: [Method] -> [ClassName]
getClasses = concatMap (\m -> concatMap (go . (.value)) m.args)
  where
    go (TModule t) = [ClassName (upFst t)]
    go (TVector v) = go v
    go _ = []

genFuncs :: FilePath -> [Method] -> [(FilePath, T.Text)]
genFuncs path xs =
  ( path <> grDir <> "GeneralResult.hs",
    generateGeneralResult $
      sort $
        nub $
          map (\m -> m.result) xs
  )
    : map (setFileName . f) xs
  where
    f :: Method -> (T.Text, T.Text)
    f m@Method {name = name} = (name, generateFunc (methodToFunc m))

    setFileName :: (T.Text, T.Text) -> (FilePath, T.Text)
    setFileName (name, text) = (fileName name, text)

    fileName :: T.Text -> FilePath
    fileName n = path <> funcDir <> T.unpack (upFst n) <> ".hs"
