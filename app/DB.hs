module DB
  where

import           Data.Functor
import           Data.String
import           System.Environment
import           Text.Printf

import           Data.ByteString            (ByteString)
import qualified Database.PostgreSQL.Simple as Sql
import           Text.Parsec                (parse)
import qualified Text.Parsec                as Parse
import           Text.Parsec.String         (Parser)

import           Types

connectToDB :: IO Sql.Connection
connectToDB = Sql.connectPostgreSQL =<< mkConnectionString

setupDB :: Sql.Connection -> IO ()
setupDB conn = do
    query <- fromString <$> readFile "setup.sql"
    void $ Sql.execute_ conn query

-- Acquiring database connection string from Heroku.

data PostgresUrl = PostgresUrl
    { postgresHost     :: String
    , postgresPort     :: String -- I don't care what port actually is, so I even wont parse it.
    , postgresUser     :: String
    , postgresPassword :: String
    , postgresPath     :: String
    }

postgresUrl :: Parser PostgresUrl
postgresUrl = do
    _                <- until ':' <* Parse.string "//"
    postgresUser     <- until ':'
    postgresPassword <- until '@'
    postgresHost     <- until ':'
    postgresPort     <- until '/'
    postgresPath     <- Parse.many Parse.anyChar
    pure PostgresUrl { .. }
  where
    until = Parse.manyTill Parse.anyChar . Parse.try . Parse.char

mkConnectionString :: IO ByteString
mkConnectionString = do
    Right PostgresUrl { .. } <- parse postgresUrl "$DATABASE_URL" <$> getEnv "DATABASE_URL"
    pure $ fromString $
        printf "host='%s' port=%s dbname='%s' user='%s' password='%s'"
            postgresHost
            postgresPort
            postgresPath
            postgresUser
            postgresPassword

-- Various CRUD operations.

addTodo :: Todo -> Sql.Connection -> IO ()
addTodo (Todo todo) conn = do
    let query  = "INSERT INTO public.todos (todo) VALUES (?);"
        params = Sql.Only todo
    void $ Sql.execute conn query params

getAllTodos :: Sql.Connection -> IO [Todo]
getAllTodos conn = do
    let query = "SELECT todo FROM public.todos;"
    fmap (Todo . Sql.fromOnly) <$> Sql.query_ conn query
