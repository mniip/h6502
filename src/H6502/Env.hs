module H6502.Env
    (
        H6502Environment(..)
    )
    where

import Data.Word

data H6502Environment m = H6502Environment {
        memoryRead :: Word16 -> m Word8,
        memoryWrite :: Word16 -> Word8 -> m ()
    }

