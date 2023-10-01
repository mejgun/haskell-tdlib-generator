module Post () where

-- import Data.HashMap.Strict qualified as HM
-- import Parser (Arg (..), ArgVal (..), Entry (..))

-- check :: [Entry] -> IO ()
-- check xs = do
--   let m = foldr func HM.empty xs
--       ms = HM.toList m
--       res = map (\(_, es) -> foldr argCheck HM.empty es) ms
--   mapM_ print res
--   where
--     --   print m

--     func m@Method {result = r} ms = HM.insertWith (<>) r [m] ms
--     func _ b = b

-- argCheck :: Entry -> HM.HashMap String ArgVal -> HM.HashMap String ArgVal
-- argCheck e m =
--   foldMap
--     ( \a ->
--         HM.insertWith
--           ( \x y ->
--               if x /= y
--                 then error $ show a <> show x <> show y
--                 else x
--           )
--           a.name
--           a.value
--           m
--     )
--     e.args