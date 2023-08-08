{-# LANGUAGE OverloadedStrings #-}
module Schema where

import Data.UUID (UUID)
import Data.Time.Clock (UTCTime)
import Data.Aeson (ToJSON, toJSON, object, (.=))

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

data UserUpdate = UserUpdate
  { userId :: UUID
  , newUsername :: String
  , newFirstName :: String
  , newLastName :: String
  , newEmail :: String
  , newPassword :: String
  }

instance ToJSON User where
  toJSON user = object
    [ "userUserId" .= userUserId user
    , "username" .= username user
    , "firstName" .= firstName user
    , "lastName" .= lastName user
    , "email" .= email user
    , "password" .= password user
    , "userCreatedAt" .= userCreatedAt user
    , "userUpdatedAt" .= userUpdatedAt user
    ]

instance Show User where
  show user = "User { userUserId = " ++ show (Schema.userUserId user)
              ++ ", username = " ++ show (Schema.username user)
              ++ ", firstName = " ++ show (Schema.firstName user)
              ++ ", lastName = " ++ show (Schema.lastName user)
              ++ ", email = " ++ show (Schema.email user)
              ++ ", password = " ++ show (Schema.password user)
              ++ ", userCreatedAt = " ++ show (Schema.userCreatedAt user)
              ++ ", userUpdatedAt = " ++ show (Schema.userUpdatedAt user)
              ++ " }"

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

