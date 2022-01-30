module Layout where
import qualified Data.ByteString.UTF8          as BU
import           Data.String                    ( IsString )
import           Data.Text                      ( Text )
import           Text.Blaze.Html                ( (!)
                                                , Html
                                                )
import qualified Text.Blaze.Html5              as H
import qualified Text.Blaze.Html5.Attributes   as A

style :: [H.AttributeValue] -> H.Attribute
style = A.style . mconcat . map (`mappend` ";")

class' :: [H.AttributeValue] -> H.Attribute
class' = A.class_ . mconcat . map (`mappend` " ")

stylesheet :: H.AttributeValue -> Html
stylesheet str = H.link ! A.rel "stylesheet" ! A.href str


link :: Html -> H.AttributeValue -> Html
link c r = H.a c ! A.href r

lines' :: [Html] -> Html
lines' = mconcat . concatMap (: [H.br])

layout :: Html -> Html
layout html = H.html
  (do
    H.head
      (do
        H.title "Haskell Test"
        H.style
          "a {color: white;}\nh3 {color: lightgray;}\npre {background-color: #333; padding: 0.5em}\nfigure {margin: 1em}"
      )
    H.body
        (vsplit
          "max-content auto"
          ( H.div
              (lines'
                [ link "home"    "/home"
                , link "work"    "/work"
                , link "games"   "/games"
                , link "music"   "/music"
                , link "gallery" "/gallery"
                , link "about"   "/about"
                , ""
                , link "login"    "/login"
                , link "register" "/register"
                ]
              )
          ! style ["margin: 1em"]
          )
          html
        )
      ! style
          [ "overflow: auto"
          , "margin: 0"
          , "background-color: #1a1a1a"
          , "color: white"
          ]
  )

ul :: [Html] -> Html
ul xs = H.ul $ mconcat $ map H.li xs

ol :: [Html] -> Html
ol xs = H.ul $ mconcat $ map H.li xs

container :: Html -> Html
container = H.div ! style ["height: 100%"]

vsplit :: H.AttributeValue -> Html -> Html -> Html
vsplit str l r =
  H.div
      (do
        H.div l ! style
          [ "grid-column: 1 / 2"
          , "border-right: solid"
          , "border-color: white"
          , "border-width: 1px"
          ]
        H.div r ! style ["grid-column: 2 / 3"]
      )
    ! style ["display: grid", "grid-template-columns: " `mappend` str]
