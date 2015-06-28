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

data H6502State = H6502State {
        reg_pc :: Word16,
        reg_s :: Word8,
        reg_a :: Word8,
        reg_x :: Word8,
        reg_y :: Word8,
        reg_flags :: Word8
    } deriving (Eq, Show, Read)

flagCarry :: Word8
flagCarry = 0x01

flagZero :: Word8
flagZero = 0x02

flagInterrupt :: Word8
flagInterrupt = 0x04

flagDecimal :: Word8
flagDecimal = 0x08

flagBrk :: Word8
flagBrk = 0x10

flagOverflow :: Word8
flagOverflow = 0x40

flagNegative :: Word8
flagNegative = 0x80

reservedFlags :: Word8
reservedFlags = 0x30

defaultState :: H6502State
defaultState = H6502State {
		reg_pc = 0,
		reg_s = 0xFD,
		reg_a = 0,
		reg_x = 0,
		reg_y = 0,
		reg_flags = reservedFlags .|. flagInterrupt
	}
