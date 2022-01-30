module Lib where
import           Data.ByteString.Char8          ( unpack )
import           Data.ByteString.Lazy           ( ByteString
                                                , fromStrict
                                                )
import           Data.Text                      ( Text )
import           Network.HTTP.Types             ( notFound404
                                                , ok200
                                                )
import           Network.Wai                    ( Application
                                                , Middleware
                                                , Request(pathInfo, rawPathInfo)
                                                , ifRequest
                                                , responseFile
                                                , responseLBS
                                                )
import           Network.Wai.Handler.Warp       ( run )
import           Network.Wai.Middleware.Gzip    ( GzipFiles(GzipCompress)
                                                , GzipSettings(gzipFiles)
                                                , def
                                                , defaultCheckMime
                                                , gzip
                                                )
import           Network.Wai.Middleware.RequestLogger
                                                ( logStdout )

data Router = Route Application | Routes [([Text], Router)]

route :: Router -> Middleware
route router app req res = route' router app req res $ pathInfo req
 where
  route' (Route  r ) a req res p = r req res
  route' (Routes []) a req res p = a req res
  route' (Routes ((p', r) : xs)) a req res p
    | p' `isPrefixOf` p = route' r a req res $ drop (length p') p
    | otherwise         = route' (Routes xs) a req res p

someFunc :: IO ()
someFunc = putStrLn "someFunc"

notFound :: Application
notFound req res =
  res
    $         responseLBS notFound404 []
    $         "page "
    `mappend` fromStrict (rawPathInfo req)
    `mappend` " not found :("

handleText :: ByteString -> Application
handleText text req res = res $ responseLBS ok200 [] text


isPrefixOf :: Eq a => [a] -> [a] -> Bool
isPrefixOf [] _  = True
isPrefixOf _  [] = False
isPrefixOf (x : xs) (y : ys) | x == y    = xs `isPrefixOf` ys
                             | otherwise = False


app :: Middleware
app = route
  (Routes
    [ ( ["users"]
      , Routes
        [ (["get"] , Route $ handleText "get users")
        , (["post"], Route $ handleText "post user")
        ]
      )
    , ( ["groups"]
      , Routes
        [ (["get"] , Route $ handleText "get groups")
        , (["post"], Route $ handleText "post group")
        ]
      )
    ]
  )

serveStatic :: Middleware
serveStatic app req res = case pathInfo req of
  "static" : _ ->
    res $ responseFile ok200 [] (drop 1 . unpack $ rawPathInfo req) Nothing
  _ -> app req res

main' :: IO ()
main' = run
  4200
  (gzip (def { gzipFiles = GzipCompress }) $ serveStatic $ logStdout $ app
    notFound
  )
