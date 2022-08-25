{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}

module Database.Persist.Monad.Internal.PersistentShim (
  SafeToInsert,
) where

#if MIN_VERSION_persistent(2,14,1)
import Database.Persist.Class (SafeToInsert)
#elif MIN_VERSION_persistent(2,14,0)
import Database.Persist.Class.PersistEntity (SafeToInsert)
#endif

#if !MIN_VERSION_persistent(2,14,0)
class SafeToInsert record
instance SafeToInsert record
#endif
