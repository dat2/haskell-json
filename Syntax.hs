module Syntax where

import qualified Data.Map as M

data JsonValue =
    JsonObject (M.Map String JsonValue)
  | JsonArray [JsonValue]
  | JsonNull
  | JsonBool Bool
  | JsonString String
  | JsonNumber Double
  deriving (Eq, Show)
