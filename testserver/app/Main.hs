module Main where


{- main :: IO () -}
{- main = someFunc -}

import           Blaze.ByteString.Builder       ( copyByteString )
import qualified Data.ByteString.UTF8          as BU
import           Data.String                    ( IsString(fromString) )
import           Data.Text                      ( Text
                                                , pack
                                                )
import           Data.Text.Internal.Builder     ( toLazyText )
import           Layout                         ( layout
                                                , style
                                                , ul
                                                , vsplit
                                                )
import           Lib                            ( main' )
import           Network.HTTP.Types             ( notFound404
                                                , status200
                                                )
import           Network.Wai                    ( Request(pathInfo)
                                                , Response
                                                , responseBuilder
                                                )
import           Network.Wai.Handler.Warp       ( run )
import           Text.Blaze.Html                ( (!)
                                                , Html
                                                )
import           Text.Blaze.Html.Renderer.Utf8  ( renderHtmlBuilder )
import qualified Text.Blaze.Html5              as H
import qualified Text.Blaze.Html5.Attributes   as A

main :: IO ()
main = do
  let port = 8080
  putStrLn $ "Listening on port " ++ show port
  run port app

users :: [Text] -> Response
users ["get", name] = serveHtml $ H.text ("Get user " `mappend` name)
users ["post"]      = serveHtml $ H.text "Post user"
users _             = notFound

servePage :: Html -> Response
servePage html = serveHtml $ layout $ H.div html ! style ["margin: 1em"]

app :: Request -> (Response -> t) -> t
app req respond = respond $ case pathInfo req of
  "users" :     xs -> users xs
  [       "home"]  -> servePage $ H.h2 "Home"
  [       "work"]  -> servePage $ do
    H.h2 "Work"
    H.p "Here I will demonstrate IT stuff."
    H.figure $ do
      H.pre
        $ H.code
            "#include <stdio.h>\n\nint main() {\n  printf(\"Hello World!\\n\");\n}"
      H.figcaption "Hello World in \"C\""
    H.p "Some text in between the two code sections."
    H.figure $ do
      H.pre $ H.code
        "module Main where\n\nmain :: IO ()\nmain = putStrLn \"Hello World!\""
      H.figcaption "Hello World in \"Haskell\""
  x -> index x

serveHtml :: Html -> Response
serveHtml =
  responseBuilder status200 [("Content-Type", "text/html")] . renderHtmlBuilder

notFound :: Response
notFound = responseBuilder notFound404 [] ""

toPath :: [Text] -> Text
toPath = foldl (\str s -> mconcat [str, "/", s]) ""

fibs :: [Integer]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

table :: [[Html]] -> Html
table xs = (H.table . H.tbody . mconcat $ map (H.tr . mconcat . map H.td) xs)
  ! A.class_ "table"

tree :: (Eq a, Num a) => a -> Html
tree 0 = H.div "x" ! A.style "text-align: center;"
tree l = do
  H.div "x" ! A.style "text-align: center;"
  vsplit "1fr 1fr" (tree (l - 1)) (tree (l - 1))


fibP :: Html
fibP = H.p (fromString $ concatMap ((' ' :) . show) $ take 150 fibs)

index :: [Text] -> Response
index x =
  serveHtml
    $ layout
        ( do
          H.div
            (do
              H.p
                "This is just a bunch of normal Text. This is just a bunch of normal Text. This is just a bunch of normal Text. This is just a bunch of normal Text. This is just a bunch of normal Text. This is just a bunch of normal Text. This is just a bunch of normal Text. This is just a bunch of normal Text. This is just a bunch of normal Text. This is just a bunch of normal Text. This is just a bunch of normal Text. This is just a bunch of normal Text. "
              H.pre
                $ H.code
                    "#include <stdio.h>\n\nint main() {\n  printf(\"Hello World!\");\n}"
              fibP
            )
          vsplit "1fr 1fr"
                 (fibP ! style ["margin: 1em"])
                 (fibP ! style ["margin: 1em"])
        ! style ["margin: 1em"]
        )
    ! style ["height: 100%", "width: 100%"]
