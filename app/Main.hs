import           Control.Monad
import           System.Environment

import qualified Data.Text.Encoding            as Text
import qualified Network.HTTP.Types            as Http
import qualified Network.Wai                   as Web
import qualified Network.Wai.Handler.Warp      as Web
import qualified Network.Wai.Middleware.Static as Web
import qualified Network.Wai.Parse             as Web
import qualified Database.PostgreSQL.Simple    as DB
import qualified Text.Blaze.Html.Renderer.Utf8 as Html
import           Text.Blaze.Html5              ((!))
import qualified Text.Blaze.Html5              as Html
import qualified Text.Blaze.Html5.Attributes   as Html hiding (title, form)

import           DB
import           Types

main :: IO ()
main = do
    port <- read <$> getEnv "PORT"
    conn <- connectToDB
    setupDB conn
    Web.run port $ Web.static $ app conn

app :: DB.Connection -> Web.Application
app conn request respond = do
    when (Web.requestMethod request == "POST") do
        param <- lookup "text" . fst <$> Web.parseRequestBody (\_ _ _ -> pure ()) request
        case param of
            Just param -> do
                let todo = Todo $ Text.decodeUtf8 param
                addTodo todo conn
            Nothing ->
                pure ()

    todos <- getAllTodos conn
    respond $ Web.responseBuilder
        Http.status200
        [("Content-Type", "text/html")] 
        (Html.renderHtmlBuilder $ page todos)


page :: [Todo] -> Html.Markup
page todos = do
    Html.docType
    Html.html ! Html.lang "en" $ do
        Html.head do
            Html.title "To-do list"
            Html.meta ! Html.charset "UTF-8"
            Html.link ! Html.rel "preconnect" ! Html.href "https://fonts.gstatic.com"
            Html.link ! Html.rel "stylesheet" ! Html.href "https://fonts.googleapis.com/css2?family=Montserrat&family=Roboto+Slab:wght@400&display=swap"
            Html.link ! Html.rel "stylesheet" ! Html.href "style.css"
        Html.body do
            Html.h1 ! Html.class_ "todo__header" $
                "To-do list"
            Html.form ! Html.class_ "todo__form" ! Html.method "POST" $ do
                Html.textarea ! Html.placeholder "What to do next?" ! Html.name "text" $
                    pure ()
                Html.button ! Html.type_ "submit" $
                    Html.text "Add"
            Html.ul ! Html.class_ "todo__list" $
                forM_ todos \(Todo todo) ->
                    Html.li ! Html.class_ "todo__item" $
                        Html.div ! Html.class_ "add" $
                            Html.text todo
