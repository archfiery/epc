module Data.URI where

type URIPrefix = String

type URIQuantifier = String

type URIPayload = String

class URI a where
  uri        :: a -> String
  prefix     :: a -> URIPrefix
  quantifier :: a -> URIQuantifier
  payload    :: a -> URIPayload
