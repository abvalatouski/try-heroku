module Types
  where

import           Data.Text (Text)
import qualified Data.Text as Text

newtype AppState = AppState
    { appTodos :: [Todo]
    }
  deriving stock Show

newtype Todo = Todo
    { getTodo :: Text
    }

instance Show Todo where
    show = Text.unpack . getTodo
