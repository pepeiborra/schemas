{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}
module Schemas.SOP where

import           Control.Alternative.Free
import qualified Data.List.NonEmpty       as NE
import           Data.Maybe
import           Data.Profunctor
import           Data.Text                (Text, pack)
import           Generics.SOP             as SOP
import           Schemas.Internal

data Options = Options
  { fieldLabelModifier     :: String -> String
  , constructorTagModifier :: String -> String
  }

defOptions :: Options
defOptions = Options id id

type FieldSchema = FieldEncode `And` HasSchema

fieldSchemaC :: Proxy FieldSchema
fieldSchemaC = Proxy

gSchema :: forall a. (HasDatatypeInfo a, All2 FieldSchema (Code a)) => Options -> TypedSchema a
gSchema opts = case datatypeInfo (Proxy @a) of
    (Newtype _ _ ci       ) -> dimap (unZ . unSOP . from) (to . SOP . Z) $ gSchemaNP opts ci
    (ADT _ _ (ci :* Nil) _) -> dimap (unZ . unSOP . from) (to . SOP . Z) $ gSchemaNP opts ci
    (ADT _ _ cis         _) -> dimap (unSOP . from) (to . SOP) $ gSchemaNS opts cis

gSchemaNS :: forall xss . All2 FieldSchema xss => Options -> NP ConstructorInfo xss -> TypedSchema (NS (NP I) xss)
gSchemaNS opts =
    union
        . NE.fromList
        . hcollapse
        . hczipWith3 (Proxy :: Proxy (All FieldSchema)) mk (injections @_ @(NP I)) (ejections  @_ @(NP I))
    where
        mk
            :: forall (xs :: [*])
             . All FieldSchema xs
            => Injection (NP I) xss xs
            -> Ejection (NP I) xss xs
            -> ConstructorInfo xs
            -> K (Text, TypedSchema (NS (NP I) xss), NS (NP I) xss -> Bool) xs
        mk (Fn inject) (Fn eject) ci = K
            ( cons
            , dimap (fromJust . unComp . eject . K) (unK . inject) (gSchemaNP opts ci)
            , isJust . unComp . eject . K
            )
            where cons = pack (constructorTagModifier opts (constructorName ci))

gSchemaNP
    :: forall (xs :: [*])
     . (All FieldSchema xs)
    => Options
    -> ConstructorInfo xs
    -> TypedSchema (NP I xs)
gSchemaNP opts ci =
  record $
  hsequence $
  hczipWith fieldSchemaC mk fieldNames projections
  where
    mk :: (HasSchema x, FieldEncode x) => K String x -> Projection I xs x -> Alt (RecordField (NP I xs)) x
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

class FieldEncode a where fieldEncoder :: Text -> (from -> a) -> Alt(RecordField from) a

instance {-# OVERLAPPABLE #-} HasSchema a => FieldEncode a where fieldEncoder = field
instance HasSchema a => FieldEncode (Maybe a) where fieldEncoder = optField
