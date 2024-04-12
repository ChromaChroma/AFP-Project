module Database.Config where 

import Database.PostgreSQL.Simple

-- | Hardcoded database connection for development (Docker) database
getConnection :: IO Connection
getConnection = connect $ defaultConnectInfo 
  { connectHost     = "localhost"
  , connectPort     = 15432
  , connectDatabase = "CodeCommit"
  , connectUser     = "code-commit-user"
  , connectPassword = "code-commit-user-password"
  }
