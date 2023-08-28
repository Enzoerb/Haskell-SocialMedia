module Utils.PasswordEncryption (encryptPassword) where

import Crypto.Hash (hash, Digest, SHA256)
import Data.ByteArray (convert)
import Data.ByteString.UTF8 (fromString)

-- Encrypt the password using SHA-256 hash function
encryptPassword :: String -> String
encryptPassword password =
    show (hash (fromString password) :: Digest SHA256)

-- Check if the provided password matches the encrypted version
checkPassword :: String -> String -> Bool
checkPassword actualPassword encryptedPassword =
    let encryptedActualPassword = encryptPassword actualPassword
    in encryptedActualPassword == encryptedPassword

main :: IO ()
main = do
    let actualPassword = "mysecretpassword"
        encryptedPassword = encryptPassword actualPassword
    putStrLn $ "Encrypted password: " ++ encryptedPassword

    -- Example usage of the checkPassword function
    let userInput = "mysecretpassword"
    putStrLn $ "Password matches: " ++ show (checkPassword userInput encryptedPassword)
