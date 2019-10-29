{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}
module Schemas.SOP
  ( gSchema
  , HasGenericSchema
  , gRecordFields
  , Options(..)
  , defOptions
  , FieldEncode
  )
where

import           Control.Lens (prism')
import qualified Data.List.NonEmpty       as NE
import           Data.Profunctor
import           Data.Text                (Text, pack)
import           Generics.SOP             as SOP
import           Schemas.Class
import           Schemas.Internal

type HasGenericSchema a = (HasDatatypeInfo a, All2 FieldEncode (Code a))

data Options = Options
  { fieldLabelModifier     :: String -> String
  , constructorTagModifier :: String -> String
  }

defOptions :: Options
defOptions = Options id id

fieldSchemaC :: Proxy FieldEncode
fieldSchemaC = Proxy

gSchema :: forall a. HasGenericSchema a => Options -> TypedSchema a
gSchema opts = case datatypeInfo (Proxy @a) of
    (Newtype _ _ ci       ) -> dimap (unZ . unSOP . from) (to . SOP . Z) $ gSchemaNP opts ci
    (ADT _ _ (ci :* Nil) _) -> dimap (unZ . unSOP . from) (to . SOP . Z) $ gSchemaNP opts ci
    (ADT _ _ cis         _) -> dimap (unSOP . from) (to . SOP) $ gSchemaNS opts cis

gRecordFields :: forall a xs. (HasDatatypeInfo a, All FieldEncode xs, Code a ~ '[xs]) => Options -> RecordFields a a
gRecordFields opts = case datatypeInfo (Proxy @a) of
    (Newtype _ _ ci       ) -> dimap (unZ . unSOP . from) (to . SOP . Z) $ gRecordFields' opts ci
    (ADT _ _ (ci :* Nil) _) -> dimap (unZ . unSOP . from) (to . SOP . Z) $ gRecordFields' opts ci


gSchemaNS :: forall xss . All2 FieldEncode xss => Options -> NP ConstructorInfo xss -> TypedSchema (NS (NP I) xss)
gSchemaNS opts ci =
    case mkAlts ci of
      [] -> mempty
      other -> union $ NE.fromList other
    where
        mkAlts = hcollapse . hczipWith3 (Proxy :: Proxy (All FieldEncode)) mk (injections @_ @(NP I)) (ejections  @_ @(NP I))
        mk
            :: forall (xs :: [*])
             . All FieldEncode xs
            => Injection (NP I) xss xs
            -> Ejection (NP I) xss xs
            -> ConstructorInfo xs
            -> K (Text, UnionAlt (NS (NP I) xss)) xs
        mk (Fn inject) (Fn eject) ci = K (cons, altWith sc (prism' (unK . inject) (unComp . eject . K))) where
            -- sc = dimap (unComp . eject . K) (unK . inject . fromJust) gSchemaNP opts ci)
            sc = gSchemaNP opts ci
            cons = pack (constructorTagModifier opts (constructorName ci))

gSchemaNP
    :: forall (xs :: [*])
     . (All FieldEncode xs)
    => Options
    -> ConstructorInfo xs
    -> TypedSchema (NP I xs)
gSchemaNP opts = record . gRecordFields' opts

gRecordFields'
    :: forall (xs :: [*])
     . (All FieldEncode xs)
    => Options
    -> ConstructorInfo xs
    -> RecordFields (NP I xs) (NP I xs)
gRecordFields' opts ci =
  hsequence $
  hczipWith fieldSchemaC mk fieldNames projections
  where
    mk :: (FieldEncode x) => K String x -> Projection I xs x -> RecordFields (NP I xs) x
    mk (K theFieldName) (Fn proj) =
      fieldEncoder (pack $ fieldLabelModifier opts theFieldName) (dimap K unI proj)

    fieldNames :: NP (K String) xs
    fieldNames = case ci of
      SOP.Record _ theFieldNames -> hmap (K . SOP.fieldName) theFieldNames
      SOP.Infix{}                -> hmap (K . ("$" ++) . show . unK) (numbers 1)
      SOP.Constructor{}          -> hmap (K . ("$" ++) . show . unK) (numbers 1)

    numbers :: forall k (fields :: [k]) . SListI fields => Int -> NP (K Int) fields
    numbers no = case sList :: SList fields of
      SNil  -> Nil
      SCons -> K no :* numbers (no + 1)

class FieldEncode a where fieldEncoder :: Text -> (from -> a) -> RecordFields from a

instance {-# OVERLAPPABLE #-} HasSchema a => FieldEncode a where fieldEncoder = field
instance HasSchema a => FieldEncode (Maybe a) where fieldEncoder = optField
