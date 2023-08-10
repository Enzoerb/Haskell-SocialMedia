{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Schema where

import Data.UUID (UUID)
import Data.Time.Clock (UTCTime)
import Data.Time (UTCTime, getCurrentTime, formatTime)
import Data.Aeson (ToJSON, FromJSON, toJSON, object, (.=))
import qualified Database.PostgreSQL.Simple.ToRow as PGToRow
import qualified Database.PostgreSQL.Simple.ToField as PGToField
import qualified Data.Time.Format as TimeFormat
import GHC.Generics (Generic)

-- Utils

formatUTCTime :: UTCTime -> String
formatUTCTime = formatTime TimeFormat.defaultTimeLocale "%Y-%m-%d %H:%M:%S%Q"

-- User

data User = User
  { userUserId      :: UUID
  , username        :: String
  , firstName       :: String
  , lastName        :: String
  , email           :: String
  , password        :: String
  , userCreatedAt   :: UTCTime
  , userUpdatedAt   :: UTCTime
  }
  deriving (Show, Generic)

instance ToJSON User
instance FromJSON User

instance PGToRow.ToRow User where
  toRow user =
    [ PGToField.toField (userUserId user)
    , PGToField.toField (username user)
    , PGToField.toField (firstName user)
    , PGToField.toField (lastName user)
    , PGToField.toField (email user)
    , PGToField.toField (password user)
    , PGToField.toField (formatUTCTime $ userCreatedAt user)
    , PGToField.toField (formatUTCTime $ userUpdatedAt user)
    ]


data UserUpdate = UserUpdate
  { oldUserId        :: UUID
  , newUsername   :: String
  , newFirstName  :: String
  , newLastName   :: String
  , newEmail      :: String
  , newPassword   :: String
  }
  deriving (Show, Generic)

instance ToJSON UserUpdate
instance FromJSON UserUpdate


data UserInsert = UserInsert
  { insertUsername   :: String
  , insertFirstName  :: String
  , insertLastName   :: String
  , insertEmail      :: String
  , insertPassword   :: String
  }
  deriving (Show, Generic)

instance ToJSON UserInsert
instance FromJSON UserInsert


-- Follow (does not have update)

data Follow = Follow
  { followId        :: UUID
  , userFollowedId  :: UUID
  , userFollowerId  :: UUID
  , followCreatedAt :: UTCTime
  , followUpdatedAt :: UTCTime
  }

instance ToJSON Follow where
  toJSON follow = object
    [ "followId"        .= followId follow
    , "userFollowedId"  .= userFollowedId follow
    , "userFollowerId"  .= userFollowerId follow
    , "userCreatedAt"   .= followCreatedAt follow
    , "userUpdatedAt"   .= followUpdatedAt follow
    ]

instance PGToRow.ToRow Follow where
  toRow follow =
    [ PGToField.toField (followId follow)
    , PGToField.toField (userFollowedId follow)
    , PGToField.toField (userFollowerId follow)
    , PGToField.toField (formatUTCTime $ followCreatedAt follow)
    , PGToField.toField (formatUTCTime $ followUpdatedAt follow)
    ]

-- Post

data Post = Post
  { postId        :: UUID
  , postUserId    :: UUID
  , content       :: String
  , postType      :: String
  , postCreatedAt :: UTCTime
  , postUpdatedAt :: UTCTime
  }

instance ToJSON Post where
  toJSON post = object
    [ "postId"          .= postId post
    , "postUserId"      .= postUserId post
    , "content"         .= content post
    , "postType"        .= postType post
    , "postCreatedAt"   .= postCreatedAt post
    , "postUpdatedAt"   .= postUpdatedAt post
    ]

instance PGToRow.ToRow Post where
  toRow post =
    [ PGToField.toField (postId post)
    , PGToField.toField (postUserId post)
    , PGToField.toField (content post)
    , PGToField.toField (postType post)
    , PGToField.toField (formatUTCTime $ postCreatedAt post)
    , PGToField.toField (formatUTCTime $ postUpdatedAt post)
    ]

data PostUpdate = PostUpdate
  { oldPostId      :: UUID
  , newContent     :: String
  , newPostType    :: String
  }
