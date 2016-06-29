{-# LANGUAGE OverloadedStrings #-}

module Data.Kubernets ( Any(..)
                      , Annotations(..)
                      , Labels(..)
                      , Metadata(..)
                      , UnversionedListMeta(..)
                      , ObjectMeta(..)
                      , ObjectReference(..)
                      , EndpointAddress(..)
                      , EndpointPort(..)
                      , Endpoints(..)
                      , EndpointSubset(..)
                      , EndpointsList(..)) where

import           Control.Monad
import           Data.Aeson
import           Data.Openshift.Class as OC

data Any = Any String String deriving (Show)

instance OC.Key Any where
    key (Any k _) = k

instance OC.Value Any where
    value (Any _ v) = v

instance FromJSON Any where
    parseJSON (Object a) = Any <$> a .: "key"
                               <*> a .: "value"
    parseJSON _ = mzero

instance ToJSON Any where
    toJSON (Any k v) = object [ "key" .= k
                              , "value" .= v ]


class Annotations a where
    annotations :: a -> [Any]


class Labels a where
    labels :: a -> [Any]



class Metadata a where
    metadata :: a -> UnversionedListMeta

data UnversionedListMeta = UnversionedListMeta String String
                         | NullListMeta deriving (Show)

instance OC.SelfLink UnversionedListMeta where
    selfLink (UnversionedListMeta s r) = s

instance OC.ResourceVersion UnversionedListMeta where
    resourceVersion (UnversionedListMeta s r) = r

instance FromJSON UnversionedListMeta where
    parseJSON (Object a) = UnversionedListMeta <$> a .:? "selfLink" .!= ""
                                               <*> a .:? "resourceVersion" .!= ""
    parseJSON _ = mzero

instance ToJSON UnversionedListMeta where
    toJSON (UnversionedListMeta s r) = object [ "selfLink" .= s
                                              , "resourceVersion" .= r
                                              ]



data ObjectMeta =
  ObjectMeta String String String String String String Int String String [Any] [Any]
       deriving (Show)


instance OC.Name ObjectMeta where
    name (ObjectMeta n gn ns s u r g c d l a) = n

instance OC.GenerateName ObjectMeta where
    generateName (ObjectMeta n gn ns s u r g c d l a) = gn

instance OC.Namespace ObjectMeta where
    namespace (ObjectMeta n gn ns s u r g c d l a) = ns

instance OC.SelfLink ObjectMeta where
    selfLink (ObjectMeta n gn ns s u r g c d l a) = s

instance OC.UID ObjectMeta where
    uid (ObjectMeta n gn ns s u r g c d l a) = u

instance OC.ResourceVersion ObjectMeta where
    resourceVersion (ObjectMeta n gn ns s u r g c d l a) = r

instance OC.Generation ObjectMeta where
    generation (ObjectMeta n gn ns s u r g c d l a) = g

instance OC.CreationTimestamp ObjectMeta where
    creationTimestamp (ObjectMeta n gn ns s u r g c d l a) = c

instance OC.DeletionTimestamp ObjectMeta where
    deletionTimestamp (ObjectMeta n gn ns s u r g c d l a) = d

instance Labels ObjectMeta where
    labels (ObjectMeta n gn ns s u r g c d l a) = l

instance Annotations ObjectMeta where
    annotations (ObjectMeta n gn ns s u r g c d l a) = a

instance FromJSON ObjectMeta where
    parseJSON (Object a) = ObjectMeta <$> a .: "name"
                                      <*> a .: "generateName"
                                      <*> a .: "namespace"
                                      <*> a .: "selfLink"
                                      <*> a .: "uid"
                                      <*> a .: "resourceVersion"
                                      <*> a .: "generation"
                                      <*> a .: "creationTimestamp"
                                      <*> a .: "deletionTimestamp"
                                      <*> a .: "labels"
                                      <*> a .: "annotations"
    parseJSON _ = mzero

instance ToJSON ObjectMeta where
    toJSON (ObjectMeta n gn ns s u r g c d l a) =
      object [ "name" .= n
             , "generateName" .= gn
             , "namespace" .= ns
             , "selfLink" .= s
             , "uid" .= u
             , "resourceVersion" .= r
             , "generation" .= g
             , "creationTimestamp" .= c
             , "deletionTimestamp" .= d
             , "labels" .= l
             , "annotations" .= a
             ]


data ObjectReference =
  ObjectReference String String String String String String String deriving (Show)

instance Kind ObjectReference where
    kind (ObjectReference k _ _ _ _ _ _) = k

instance Namespace ObjectReference where
    namespace (ObjectReference _ n _ _ _ _ _) = n

instance Name ObjectReference where
    name (ObjectReference _ _ n _ _ _ _) = n

instance UID ObjectReference where
    uid (ObjectReference _ _ _ u _ _ _) = u

instance ApiVersion ObjectReference where
    apiVersion (ObjectReference _ _ _ _ a _ _) = a

instance ResourceVersion ObjectReference where
    resourceVersion (ObjectReference _ _ _ _ _ r _) = r

instance FieldPath ObjectReference where
    fieldPath (ObjectReference _ _ _ _ _ _ f) = f

instance FromJSON ObjectReference where
    parseJSON (Object a) = ObjectReference <$> a .: "kind"
                                           <*> a .: "namespace"
                                           <*> a .: "name"
                                           <*> a .: "uid"
                                           <*> a .: "apiVersion"
                                           <*> a .: "resourceVersion"
                                           <*> a .: "fieldPath"
    parseJSON _ = mzero

instance ToJSON ObjectReference where
    toJSON (ObjectReference k ns n u a r f) = object [ "kind" .= k
                                                     , "namespace" .= ns
                                                     , "name" .= n
                                                     , "uid" .= u
                                                     , "apiVersion" .= a
                                                     , "resourceVersion" .= r
                                                     , "fieldPath" .= f ]


class TargetRef a where
    targetRef :: a -> ObjectReference


data EndpointAddress = EndpointAddress String ObjectReference deriving (Show)

instance OC.IP EndpointAddress where
    ip (EndpointAddress i t) = i

instance TargetRef EndpointAddress where
    targetRef (EndpointAddress i t) = t

instance FromJSON EndpointAddress where
    parseJSON (Object a) = EndpointAddress <$> a .: "ip"
                                           <*> a .: "targetRef"
    parseJSON _ = mzero

instance ToJSON EndpointAddress where
    toJSON (EndpointAddress i t) = object [ "ip" .= i
                                          , "targetRef" .= t ]


data EndpointPort = EndpointPort String Int String deriving (Show)

instance OC.Name EndpointPort where
    name (EndpointPort n port prot) = n

instance OC.Port EndpointPort where
    port (EndpointPort n port prot) = port

instance OC.Protocol EndpointPort where
    protocol (EndpointPort n port prot) = prot

instance FromJSON EndpointPort where
    parseJSON (Object a) = EndpointPort <$> a .: "name"
                                        <*> a .: "port"
                                        <*> a .: "protocol"
    parseJSON _ = mzero

instance ToJSON EndpointPort where
    toJSON (EndpointPort n port prot) =
      object [ "name" .= n
             , "port" .= port
             , "protocol" .= prot
             ]


data EndpointSubset = EndpointSubset [EndpointAddress] [EndpointAddress] [EndpointPort]
                          deriving (Show)

data Endpoints = Endpoints String String ObjectMeta [EndpointSubset]
                     deriving (Show)

data EndpointsList = EndpointsList String String UnversionedListMeta [Endpoints]
                         deriving (Show)


