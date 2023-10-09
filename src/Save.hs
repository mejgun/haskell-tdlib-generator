module Save (writeData, writeFuncs) where

import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Haskell.Data (generateData)
import Haskell.Func (generateFunc)
import Haskell.Internal (classToDataClass, methodToFunc, upFst)
import Parser (Class (Class), ClassName (ClassName), Method (Method, name, result))

dataDir :: String
dataDir = "/TD/Data/"

funcDir :: String
funcDir = "/TD/Query/"

writeData :: FilePath -> [Class] -> [Method] -> IO ()
writeData path classes methods = mapM_ (save . f) classes
  where
    f :: Class -> (ClassName, T.Text)
    f c@(Class name1 _) =
      ( name1,
        generateData $
          classToDataClass c $
            filter (\(Method {result = name2}) -> name1 == name2) methods
      )

    save :: (ClassName, T.Text) -> IO ()
    save (c, text) = TIO.writeFile (fileName c) text

    fileName :: ClassName -> FilePath
    fileName (ClassName name) = path <> dataDir <> T.unpack name <> ".hs"

writeFuncs :: FilePath -> [Method] -> IO ()
writeFuncs path = mapM_ (save . f)
  where
    f :: Method -> (T.Text, T.Text)
    f m@Method {name = name} = (name, generateFunc (methodToFunc m))

    save :: (T.Text, T.Text) -> IO ()
    save (name, text) = TIO.writeFile (fileName name) text

    fileName :: T.Text -> FilePath
    fileName n = path <> funcDir <> T.unpack (upFst n) <> ".hs"
