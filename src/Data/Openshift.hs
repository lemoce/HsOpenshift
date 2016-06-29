{-# LANGUAGE OverloadedStrings #-}

module Data.Openshift ( UnversionedStatus(..)
                      ) where

import           Control.Monad
import           Data.Aeson
import           Data.Kubernets
import           Data.Openshift.Class as OC





class From a where
    from :: a -> ObjectReference


data NamedTagReference = NamedTagReference String [Any] ObjectReference
                              deriving (Show)

instance OC.Name NamedTagReference where
    name (NamedTagReference n _ _) = n

instance Annotations NamedTagReference where
    annotations (NamedTagReference _ a _) = a

instance From NamedTagReference where
    from (NamedTagReference _ _ f) = f

instance FromJSON NamedTagReference where
    parseJSON (Object a) = NamedTagReference <$> a .: "name"
                                             <*> a .: "annotations"
                                             <*> a .: "from"
    parseJSON _ = mzero

instance ToJSON NamedTagReference where
    toJSON (NamedTagReference n a f) = object ["name" .= n
                                              , "annotations" .= a
                                              , "from" .= f]



class Tags a where
    tags :: a -> [NamedTagReference]



data ImageStreamSpec = ImageStreamSpec String [NamedTagReference] deriving (Show)

instance OC.DockerImageRepository ImageStreamSpec where
    dockerImageRepository (ImageStreamSpec d t) = d

instance Tags ImageStreamSpec where
    tags (ImageStreamSpec d t) = t

instance FromJSON ImageStreamSpec where
    parseJSON (Object a) = ImageStreamSpec <$> a .: "dockerImageRepository"
                                           <*> a .: "tags"
    parseJSON _ = mzero

instance ToJSON ImageStreamSpec where
    toJSON (ImageStreamSpec d t) = object [ "dockerImageRepository" .= d
                                          , "tags" .= t
                                          ]


data TagEvent = TagEvent String String String deriving (Show)

instance OC.Created TagEvent where
    created (TagEvent c d i) = c

instance OC.DockerImageReference TagEvent where
    dockerImageReference (TagEvent c d i) = d

instance OC.Image TagEvent where
    image (TagEvent c d i) = i

instance FromJSON TagEvent where
    parseJSON (Object a) = TagEvent <$> a .: "created"
                                    <*> a .: "dockerImageRepository"
                                    <*> a .: "image"
    parseJSON _ = mzero

instance ToJSON TagEvent where
    toJSON (TagEvent c d i) = object [ "created" .= c
                                     , "dockerImageRepository" .= d
                                     , "image" .= i
                                     ]


class Items a where
    items :: a -> [TagEvent]


data NamedTagEventList = NamedTagEventList String [TagEvent] deriving (Show)

instance OC.Tag NamedTagEventList where
    tag (NamedTagEventList t i) = t

instance Items NamedTagEventList where
    items (NamedTagEventList t i) = i

instance FromJSON NamedTagEventList where
    parseJSON (Object a) = NamedTagEventList <$> a .: "tag"
                                             <*> a .: "items"
    parseJSON _ = mzero

instance ToJSON NamedTagEventList where
    toJSON (NamedTagEventList t i) = object [ "tag" .= t
                                            , "items" .= i
                                            ]

data ImageStreamStatus = ImageStreamStatus String [NamedTagReference] deriving (Show)
-- { dockerImageRepository :: String
-- , tags                  :: [NamedTagReference]}





data ImageStream = ImageStream { kind       :: String
                               , apiVersion :: String
--                               , metadata   :: ObjectMeta
                               , spec       :: ImageStreamSpec
                               , status     :: ImageStreamStatus
                               } deriving (Show)




data UnversionedStatusCause = UnversionedStatusCause String String String
                            | NullStatusCause
        deriving (Show)

instance OC.Reason UnversionedStatusCause where
    reason (UnversionedStatusCause r m f) = r

instance OC.Message UnversionedStatusCause where
    message (UnversionedStatusCause r m f) = m

instance OC.Field UnversionedStatusCause where
    field (UnversionedStatusCause r m f) = f

instance FromJSON UnversionedStatusCause where
    parseJSON (Object a) = UnversionedStatusCause <$> a .: "reason"
                                                  <*> a .: "message"
                                                  <*> a .: "field"
    parseJSON _ = mzero

instance ToJSON UnversionedStatusCause where
    toJSON (UnversionedStatusCause r m f) = object [ "reason" .= r
                                                   , "message" .= m
                                                   , "field" .= f
                                                   ]

class Causes a where
    causes :: a -> [UnversionedStatusCause]


data UnversionedStatusDetails =
      UnversionedStatusDetails String String [UnversionedStatusCause] Int
    | NullStatusDetails
         deriving (Show)

instance OC.Name UnversionedStatusDetails where
    name (UnversionedStatusDetails n k c r) = n

instance OC.Kind UnversionedStatusDetails where
    kind (UnversionedStatusDetails n k c r) = k

instance Causes UnversionedStatusDetails where
    causes (UnversionedStatusDetails n k c r) = c

instance OC.RetryAfterSeconds UnversionedStatusDetails where
    retryAfterSeconds (UnversionedStatusDetails n k c r) = r


instance FromJSON UnversionedStatusDetails where
    parseJSON (Object a) = UnversionedStatusDetails <$> a .: "name"
                                                    <*> a .: "kind"
                                                    <*> a .: "causes"
                                                    <*> a .: "retryAfterSeconds"
    parseJSON _ = mzero

instance ToJSON UnversionedStatusDetails where
    toJSON (UnversionedStatusDetails n k c r) = object [ "name" .= n
                                                       , "kind" .= k
                                                       , "causes" .= c
                                                       , "retryAfterSeconds" .= r
                                                       ]


class Details a where
    details :: a -> UnversionedStatusDetails


data UnversionedStatus =
    UnversionedStatus String String UnversionedListMeta String String String
        UnversionedStatusDetails Int
        deriving (Show)

instance OC.Kind UnversionedStatus where
    kind (UnversionedStatus k a m s msg r d c) = k

instance OC.ApiVersion UnversionedStatus where
    apiVersion (UnversionedStatus k a m s msg r d c) = a

instance Metadata UnversionedStatus where
    metadata (UnversionedStatus k a m s msg r d c) = m

instance OC.Status UnversionedStatus where
    status (UnversionedStatus k a m s msg r d c) = s

instance OC.Message UnversionedStatus where
    message (UnversionedStatus k a m s msg r d c) = msg

instance OC.Reason UnversionedStatus where
    reason (UnversionedStatus k a m s msg r d c) = r

instance Details UnversionedStatus where
    details (UnversionedStatus k a m s msg r d c) = d

instance OC.Code UnversionedStatus where
    code (UnversionedStatus k a m s msg r d c) = c


instance FromJSON UnversionedStatus where
    parseJSON (Object a) = UnversionedStatus <$> a .:? "kind" .!= ""
                                             <*> a .:? "apiVersion" .!= ""
                                             <*> a .:? "metadata" .!= NullListMeta
                                             <*> a .:? "status" .!= ""
                                             <*> a .:? "message" .!= ""
                                             <*> a .:? "reason" .!= ""
                                             <*> a .:? "details" .!= NullStatusDetails
                                             <*> a .:? "code" .!= (-256)
    parseJSON _ = mzero

instance ToJSON UnversionedStatus where
    toJSON (UnversionedStatus k a m s msg r d c) = object [ "kind" .= k
                                                          , "apiVersion" .= a
                                                          , "metadata" .= m
                                                          , "status" .= s
                                                          , "message" .= msg
                                                          , "reason" .= r
                                                          , "details" .= d
                                                          , "code" .= c
                                                          ]






