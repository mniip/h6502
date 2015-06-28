module H6502.State
    (
        H6502State(..),
        flagCarry,
        flagZero,
        flagInterrupt,
        flagDecimal,
        flagBrk,
        flagOverflow,
        flagNegative,
        reservedFlags,
        defaultState
    )
    where

import Data.Bits
import Data.Word

-- | A structure representing the state of a 6502 processor
-- Includes PC, S, A, X, and Y registers, and a flag register, with reserved and B bits always set.
data H6502State = H6502State {
        reg_pc :: Word16, -- ^ Program counter
        reg_s :: Word8, -- ^ Stack pointer
        reg_a :: Word8, -- ^ Accumulator
        reg_x :: Word8, -- ^ X index
        reg_y :: Word8, -- ^ Y index
        reg_flags :: Word8 -- ^ Flags
    } deriving (Eq, Show, Read)

-- | C flag (0th bit)
flagCarry :: Word8
flagCarry = 0x01

-- | Z flag (1st bit)
flagZero :: Word8
flagZero = 0x02

-- | I flag (2nd bit)
flagInterrupt :: Word8
flagInterrupt = 0x04

-- | D flag (3rd bit)
flagDecimal :: Word8
flagDecimal = 0x08

-- | B flag (4th bit)
flagBrk :: Word8
flagBrk = 0x10

-- | V flag (6th bit)
flagOverflow :: Word8
flagOverflow = 0x40

-- | N flag (7th bit)
flagNegative :: Word8
flagNegative = 0x80

-- | Flags that are always set: reserved (5th bit) and B (4th bit)
reservedFlags :: Word8
reservedFlags = 0x30

-- | The default post-on state of a 6502 CPU: all registers set to 0, stack pointer to 0xFD, and only the I flag is set
defaultState :: H6502State
defaultState = H6502State {
        reg_pc = 0,
        reg_s = 0xFD,
        reg_a = 0,
        reg_x = 0,
        reg_y = 0,
        reg_flags = reservedFlags .|. flagInterrupt
    }
