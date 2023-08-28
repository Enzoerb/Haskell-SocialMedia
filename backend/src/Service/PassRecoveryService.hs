{-# LANGUAGE OverloadedStrings #-}

module Service.PassRecoveryService (requestPassRecovery) where

import Data.UUID (UUID, toString)
import Database.PostgreSQL.Simple (Connection)
import qualified Repository.UserRepository as UserRepo
import Schema
import Data.List.NonEmpty (fromList)
import Network.SendGridV3.Api
import Control.Lens ((^.))
import Network.Wreq (responseStatus, statusCode)
import qualified Data.Text as T
import Utils.PasswordEncryption (encryptPassword)

type Email = String
type URL = String

urlBuilder :: Email ->  UUID -> URL
urlBuilder email userId = "http://localhost:3000/recovery/" <> (encryptPassword $ email <> toString userId)

sendGridApiKey :: ApiKey
sendGridApiKey = ApiKey "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"

emailTemplate :: Email -> URL -> Mail () ()
emailTemplate email url =
  let to = personalization $ fromList [MailAddress (T.pack email) ""]
      from = MailAddress "m.sena@aluno.ufabc.edu.br" "Matheus from λ-Social"
      subject = "Recuperação de Senha"
      content = fromList [mailContentText $ T.pack ("Para recuperar sua senha, clique no link abaixo:\n\n" <> url <> "")]
  in mail [to] from subject (Just content)

requestPassRecovery :: Email -> UUID -> IO (String)
requestPassRecovery email userId  = do
  let urlRecovery = urlBuilder email userId
  eResponse <- sendMail sendGridApiKey ((emailTemplate email urlRecovery) { _mailSendAt = Just 1516468000 })
  case eResponse of
    Left httpException -> error $ show httpException
    Right response -> return "Sucesso ao enviar o email de recuperação de senha!"