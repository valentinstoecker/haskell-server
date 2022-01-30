module App where

import           Control.Monad.Trans.Reader     ( ReaderT(ReaderT) )
import           Database.Persist               ( )
import           Database.Persist.MySQL         ( )


type App a = ReaderT Int IO a
