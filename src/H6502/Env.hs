module H6502.Env
    (
        H6502Environment(..)
    )
    where

import Data.Word

-- | Structure representing the environment of a 6502 CPU: specifically the memory bus. These two functions are the ones that are called when a memory cell is read or written. The environment is parametrized by the monad that these functions operate under.
data H6502Environment m = H6502Environment {
        memoryRead :: Word16 -> m Word8, -- ^ A function that is called when a memory cell is read, should take a 16-bit address and return a 8-bit value.
        memoryWrite :: Word16 -> Word8 -> m () -- ^ A function that is called when a memory cell is read, should take a 16-bit address and a 8-bit value.
    }

