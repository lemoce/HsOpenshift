module Data.Openshift.Class ( Key (..)
                            , Value (..)
                            , Name (..)
                            , Kind (..)
                            , Namespace (..)
                            , UID (..)
                            , ApiVersion (..)
                            , ResourceVersion (..)
                            , FieldPath (..)
                            , GenerateName (..)
                            , SelfLink (..)
                            , Generation (..)
                            , CreationTimestamp (..)
                            , DeletionTimestamp (..)
                            , DockerImageRepository (..)
                            , Created (..)
                            , DockerImageReference (..)
                            , Image (..)
                            , Tag (..)
                            , Reason (..)
                            , Message (..)
                            , Field (..)
                            , RetryAfterSeconds (..)
                            , Code (..)
                            , Status (..)
                            , IP (..)
                            , Port (..)
                            , Protocol (..)) where

class Key a where
    key :: a -> String

class Value a where
    value :: a -> String

class Name a where
    name :: a -> String

class Kind a where
    kind :: a -> String

class Namespace a where
    namespace :: a -> String

class UID a where
    uid :: a -> String

class ApiVersion a where
    apiVersion :: a -> String

class ResourceVersion a where
    resourceVersion :: a -> String

class FieldPath a where
    fieldPath :: a -> String

class GenerateName a where
    generateName :: a -> String

class SelfLink a where
    selfLink :: a -> String

class Generation a where
    generation :: a -> Int

class CreationTimestamp a where
    creationTimestamp :: a -> String

class DeletionTimestamp a where
    deletionTimestamp :: a -> String

class DockerImageRepository a where
    dockerImageRepository :: a -> String

class Created a where
    created :: a -> String

class DockerImageReference a where
    dockerImageReference :: a -> String

class Image a where
    image :: a -> String

class Tag a where
    tag :: a -> String

class Reason a where
    reason :: a -> String

class Message a where
    message :: a -> String

class Field a where
    field :: a -> String

class RetryAfterSeconds a where
    retryAfterSeconds :: a -> Int

class Code a where
    code :: a -> Int

class Status a where
    status :: a -> String

class IP a where
    ip :: a -> String

class Port a where
    port :: a -> Int

class Protocol a where
    protocol :: a -> String
