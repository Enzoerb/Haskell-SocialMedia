module Schema where

import Data.UUID (UUID)
import Data.Time.Clock (UTCTime)

data User = User
  { userUserId      :: UUID
  , username    :: String
  , firstName   :: String
  , lastName    :: String
  , email       :: String
  , password    :: String
  , userCreatedAt   :: UTCTime
  , userUpdatedAt   :: UTCTime
  }

data Follow = Follow
  { followId        :: UUID
  , userFollowedId  :: UUID
  , userFollowerId  :: UUID
  , followCreatedAt :: UTCTime
  , followUpdatedAt :: UTCTime
  }

data Post = Post
  { postId      :: UUID
  , postUserId      :: UUID
  , content     :: String
  , postType    :: String
  , postCreatedAt :: UTCTime
  , postUpdatedAt :: UTCTime
  }

