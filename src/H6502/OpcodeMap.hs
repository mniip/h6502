module H6502.OpcodeMap
    (
        opcodeMap
    )
    where

import Control.Monad
import Data.Array
import Data.Bits
import Data.Word

import H6502.State
import H6502.Trans
import H6502.Util

-- | Map of all opcode behaviors. All 'Nothing's correspond to unspecified opcodes.
opcodeMap :: Monad m => Array Word8 (Maybe (H6502T m ()))
opcodeMap = array (0x00, 0xFF) [
        (0x00, Just $ do
            pc <- getPC
            flags <- getFlags
            push16 (pc + 1)
            push flags
            irq <- peek16 0xFFFE
            setPC irq
            setFlag flagInterrupt
        ),
        (0x01, Just $ iORA (addrIxIndirect >>= peek)),
        (0x02, Nothing),
        (0x03, Nothing),
        (0x04, Nothing),
        (0x05, Just $ iORA (addrZeroPage >>= peek)),
        (0x06, Just $ iASL addrZeroPage),
        (0x07, Nothing),
        (0x08, Just $ getFlags >>= push),
        (0x09, Just $ iORA peekPC),
        (0x0A, Just $ do
            a <- getA
            let newa = shiftL a 1
            setA newa
            putFlag flagCarry (testBit a 7)
            putFlag flagNegative (testBit newa 7)
            putFlag flagZero (newa == 0)
        ),
        (0x0B, Nothing),
        (0x0C, Nothing),
        (0x0D, Just $ iORA (addrAbsolute >>= peek)),
        (0x0E, Just $ iASL addrAbsolute),
        (0x0F, Nothing),
        (0x10, Just $ branchIfNot (getFlag flagNegative)),
        (0x11, Just $ iORA (addrIndirectIx >>= peek)),
        (0x12, Nothing),
        (0x13, Nothing),
        (0x14, Nothing),
        (0x15, Just $ iORA (addrZeroPageX >>= peek)),
        (0x16, Just $ iASL addrZeroPageX),
        (0x17, Nothing),
        (0x18, Just $ unsetFlag flagCarry),
        (0x19, Just $ iORA (addrAbsoluteY >>= peek)),
        (0x1A, Nothing),
        (0x1B, Nothing),
        (0x1C, Nothing),
        (0x1D, Just $ iORA (addrAbsoluteX >>= peek)),
        (0x1E, Just $ iASL addrAbsoluteX),
        (0x1F, Nothing),
        (0x20, Just $ do
            imm <- peekPC16
            pc <- getPC
            push16 (pc - 1)
            setPC imm
        ),
        (0x21, Just $ iAND (addrIxIndirect >>= peek)),
        (0x22, Nothing),
        (0x23, Nothing),
        (0x24, Just $ iBIT (addrZeroPage >>= peek)),
        (0x25, Just $ iAND (addrZeroPage >>= peek)),
        (0x26, Just $ iROL addrZeroPage),
        (0x27, Nothing),
        (0x28, Just $ pop >>= setFlags),
        (0x29, Just $ iAND peekPC),
        (0x2A, Just $ do
            a <- getA
            cf <- getFlag flagCarry
            let newa = shiftL a 1 .|. if cf then 1 else 0
            setA newa
            putFlag flagCarry (testBit a 7)
            putFlag flagNegative (testBit newa 7)
            putFlag flagZero (newa == 0)
        ),
        (0x2B, Nothing),
        (0x2C, Just $ iBIT (addrAbsolute >>= peek)),
        (0x2D, Just $ iAND (addrAbsolute >>= peek)),
        (0x2E, Just $ iROL addrAbsolute),
        (0x2F, Nothing),
        (0x30, Just $ branchIf (getFlag flagNegative)),
        (0x31, Just $ iAND (addrIndirectIx >>= peek)),
        (0x32, Nothing),
        (0x33, Nothing),
        (0x34, Nothing),
        (0x35, Just $ iAND (addrZeroPageX >>= peek)),
        (0x36, Just $ iROL addrZeroPageX),
        (0x37, Nothing),
        (0x38, Just $ setFlag flagCarry),
        (0x39, Just $ iAND (addrAbsoluteY >>= peek)),
        (0x3A, Nothing),
        (0x3B, Nothing),
        (0x3C, Nothing),
        (0x3D, Just $ iAND (addrAbsoluteX >>= peek)),
        (0x3E, Just $ iROL addrAbsoluteX),
        (0x3F, Nothing),
        (0x40, Just $ do
            flags <- pop
            pc <- pop16
            setFlags flags
            setPC pc
        ),
        (0x41, Just $ iEOR (addrIxIndirect >>= peek)),
        (0x42, Nothing),
        (0x43, Nothing),
        (0x44, Nothing),
        (0x45, Just $ iEOR (addrZeroPage >>= peek)),
        (0x46, Just $ iLSR addrZeroPage),
        (0x47, Nothing),
        (0x48, Just $ getA >>= push),
        (0x49, Just $ iEOR peekPC),
        (0x4A, Just $ do
            a <- getA
            let newa = shiftR a 1
            setA newa
            putFlag flagCarry (testBit a 0)
            putFlag flagNegative (testBit newa 7)
            putFlag flagZero (newa == 0)
        ),
        (0x4B, Nothing),
        (0x4C, Just $ do
            imm <- peekPC16
            setPC imm
        ),
        (0x4D, Just $ iEOR (addrAbsolute >>= peek)),
        (0x4E, Just $ iLSR addrAbsolute),
        (0x4F, Nothing),
        (0x50, Just $ branchIfNot (getFlag flagOverflow)),
        (0x51, Just $ iEOR (addrIndirectIx >>= peek)),
        (0x52, Nothing),
        (0x53, Nothing),
        (0x54, Nothing),
        (0x55, Just $ iEOR (addrZeroPageX >>= peek)),
        (0x56, Just $ iLSR addrZeroPageX),
        (0x57, Nothing),
        (0x58, Just $ unsetFlag flagInterrupt),
        (0x59, Just $ iEOR (addrAbsoluteY >>= peek)),
        (0x5A, Nothing),
        (0x5B, Nothing),
        (0x5C, Nothing),
        (0x5D, Just $ iEOR (addrAbsoluteX >>= peek)),
        (0x5E, Just $ iLSR addrAbsoluteX),
        (0x5F, Nothing),
        (0x60, Just $ do
            pc <- pop16
            setPC (pc + 1)
        ),
        (0x61, Just $ iADC (addrIxIndirect >>= peek)),
        (0x62, Nothing),
        (0x63, Nothing),
        (0x64, Nothing),
        (0x65, Just $ iADC (addrZeroPage >>= peek)),
        (0x66, Just $ iROR addrZeroPage),
        (0x67, Nothing),
        (0x68, Just $ do
            a <- pop
            setA a
            putFlag flagNegative (testBit a 7)
            putFlag flagZero (a == 0)
        ),
        (0x69, Just $ iADC peekPC),
        (0x6A, Just $ do
            a <- getA
            cf <- getFlag flagCarry
            let newa = shiftR a 1 .|. if cf then 0x80 else 0
            setA newa
            putFlag flagCarry (testBit a 0)
            putFlag flagNegative (testBit newa 7)
            putFlag flagZero (newa == 0)
        ),
        (0x6B, Nothing),
        (0x6C, Just $ do
            addr <- peekPC16
            pc <- peek16 addr
            setPC pc
        ),
        (0x6D, Just $ iADC (addrAbsolute >>= peek)),
        (0x6E, Just $ iROR addrAbsolute),
        (0x6F, Nothing),
        (0x70, Just $ branchIf (getFlag flagOverflow)),
        (0x71, Just $ iADC (addrIndirectIx >>= peek)),
        (0x72, Nothing),
        (0x73, Nothing),
        (0x74, Nothing),
        (0x75, Just $ iADC (addrZeroPageX >>= peek)),
        (0x76, Just $ iROR addrZeroPageX),
        (0x77, Nothing),
        (0x78, Just $ setFlag flagInterrupt),
        (0x79, Just $ iADC (addrAbsoluteY >>= peek)),
        (0x7A, Nothing),
        (0x7B, Nothing),
        (0x7C, Nothing),
        (0x7D, Just $ iADC (addrAbsoluteX >>= peek)),
        (0x7E, Just $ iROR addrAbsoluteX),
        (0x7F, Nothing),
        (0x80, Nothing),
        (0x81, Just $ iST getA addrIxIndirect),
        (0x82, Nothing),
        (0x83, Nothing),
        (0x84, Just $ iST getY addrZeroPage),
        (0x85, Just $ iST getA addrZeroPage),
        (0x86, Just $ iST getX addrZeroPage),
        (0x87, Nothing),
        (0x88, Just $ do
            y <- getY
            let newy = y - 1
            setY newy
            putFlag flagZero (newy == 0)
            putFlag flagNegative (testBit newy 7)
        ),
        (0x89, Nothing),
        (0x8A, Just $ do
            x <- getX
            setA x
            putFlag flagZero (x == 0)
            putFlag flagNegative (testBit x 7)
        ),
        (0x8B, Nothing),
        (0x8C, Just $ iST getY addrAbsolute),
        (0x8D, Just $ iST getA addrAbsolute),
        (0x8E, Just $ iST getX addrAbsolute),
        (0x8F, Nothing),
        (0x90, Just $ branchIfNot (getFlag flagCarry)),
        (0x91, Just $ iST getA addrIndirectIx),
        (0x92, Nothing),
        (0x93, Nothing),
        (0x94, Just $ iST getY addrZeroPageX),
        (0x95, Just $ iST getA addrZeroPageX),
        (0x96, Just $ iST getX addrZeroPageY),
        (0x97, Nothing),
        (0x98, Just $ do
            y <- getY
            setA y
            putFlag flagZero (y == 0)
            putFlag flagNegative (testBit y 7)
        ),
        (0x99, Just $ iST getA addrAbsoluteY),
        (0x9A, Just $ getX >>= setS),
        (0x9B, Nothing),
        (0x9C, Nothing),
        (0x9D, Just $ iST getA addrAbsoluteX),
        (0x9E, Nothing),
        (0x9F, Nothing),
        (0xA0, Just $ iLD setY peekPC),
        (0xA1, Just $ iLD setA (addrIxIndirect >>= peek)),
        (0xA2, Just $ iLD setX peekPC),
        (0xA3, Nothing),
        (0xA4, Just $ iLD setY (addrZeroPage >>= peek)),
        (0xA5, Just $ iLD setA (addrZeroPage >>= peek)),
        (0xA6, Just $ iLD setX (addrZeroPage >>= peek)),
        (0xA7, Nothing),
        (0xA8, Just $ do
            a <- getA
            setY a
            putFlag flagZero (a == 0)
            putFlag flagNegative (testBit a 7)
        ),
        (0xA9, Just $ iLD setA peekPC),
        (0xAA, Just $ do
            a <- getA
            setX a
            putFlag flagZero (a == 0)
            putFlag flagNegative (testBit a 7)
        ),
        (0xAB, Nothing),
        (0xAC, Just $ iLD setY (addrAbsolute >>= peek)),
        (0xAD, Just $ iLD setA (addrAbsolute >>= peek)),
        (0xAE, Just $ iLD setX (addrAbsolute >>= peek)),
        (0xAF, Nothing),
        (0xB0, Just $ branchIf (getFlag flagCarry)),
        (0xB1, Just $ iLD setA (addrIndirectIx >>= peek)),
        (0xB2, Nothing),
        (0xB3, Nothing),
        (0xB4, Just $ iLD setY (addrZeroPageX >>= peek)),
        (0xB5, Just $ iLD setA (addrZeroPageX >>= peek)),
        (0xB6, Just $ iLD setX (addrZeroPageY >>= peek)),
        (0xB7, Nothing),
        (0xB8, Just $ unsetFlag flagOverflow),
        (0xB9, Just $ iLD setA (addrAbsoluteY >>= peek)),
        (0xBA, Just $ do
            sp <- getS
            setX sp
            putFlag flagZero (sp == 0)
            putFlag flagNegative (testBit sp 7)
        ),
        (0xBB, Nothing),
        (0xBC, Just $ iLD setY (addrAbsoluteX >>= peek)),
        (0xBD, Just $ iLD setA (addrAbsoluteX >>= peek)),
        (0xBE, Just $ iLD setX (addrAbsoluteY >>= peek)),
        (0xBF, Nothing),
        (0xC0, Just $ iCP getY peekPC),
        (0xC1, Just $ iCP getA (addrIxIndirect >>= peek)),
        (0xC2, Nothing),
        (0xC3, Nothing),
        (0xC4, Just $ iCP getY (addrZeroPage >>= peek)),
        (0xC5, Just $ iCP getA (addrZeroPage >>= peek)),
        (0xC6, Just $ iDEC addrZeroPage),
        (0xC7, Nothing),
        (0xC8, Just $ do
            y <- getY
            let newy = y + 1
            setY newy
            putFlag flagZero (newy == 0)
            putFlag flagNegative (testBit newy 7)
        ),
        (0xC9, Just $ iCP getA peekPC),
        (0xCA, Just $ do
            x <- getX
            let newx = x - 1
            setX newx
            putFlag flagZero (newx == 0)
            putFlag flagNegative (testBit newx 7)
        ),
        (0xCB, Nothing),
        (0xCC, Just $ iCP getY (addrAbsolute >>= peek)),
        (0xCD, Just $ iCP getA (addrAbsolute >>= peek)),
        (0xCE, Just $ iDEC addrAbsolute),
        (0xCF, Nothing),
        (0xD0, Just $ branchIfNot (getFlag flagZero)),
        (0xD1, Just $ iCP getA (addrIndirectIx >>= peek)),
        (0xD2, Nothing),
        (0xD3, Nothing),
        (0xD4, Nothing),
        (0xD5, Just $ iCP getA (addrZeroPageX >>= peek)),
        (0xD6, Just $ iDEC addrZeroPageX),
        (0xD7, Nothing),
        (0xD8, Just $ unsetFlag flagDecimal),
        (0xD9, Just $ iCP getA (addrAbsoluteY >>= peek)),
        (0xDA, Nothing),
        (0xDB, Nothing),
        (0xDC, Nothing),
        (0xDD, Just $ iCP getA (addrAbsoluteX >>= peek)),
        (0xDE, Just $ iDEC addrAbsoluteX),
        (0xDF, Nothing),
        (0xE0, Just $ iCP getX peekPC),
        (0xE1, Just $ iSBC (addrIxIndirect >>= peek)),
        (0xE2, Nothing),
        (0xE3, Nothing),
        (0xE4, Just $ iCP getX (addrZeroPage >>= peek)),
        (0xE5, Just $ iSBC (addrZeroPage >>= peek)),
        (0xE6, Just $ iINC addrZeroPage),
        (0xE7, Nothing),
        (0xE8, Just $ do
            x <- getX
            let newx = x + 1
            setX newx
            putFlag flagZero (newx == 0)
            putFlag flagNegative (testBit newx 7)
        ),
        (0xE9, Just $ iSBC peekPC),
        (0xEA, Just $ return ()),
        (0xEB, Nothing),
        (0xEC, Just $ iCP getX (addrAbsolute >>= peek)),
        (0xED, Just $ iSBC (addrAbsolute >>= peek)),
        (0xEE, Just $ iINC addrAbsolute),
        (0xEF, Nothing),
        (0xF0, Just $ branchIf (getFlag flagZero)),
        (0xF1, Just $ iSBC (addrIndirectIx >>= peek)),
        (0xF2, Nothing),
        (0xF3, Nothing),
        (0xF4, Nothing),
        (0xF5, Just $ iSBC (addrZeroPageX >>= peek)),
        (0xF6, Just $ iINC addrZeroPageX),
        (0xF7, Nothing),
        (0xF8, Just $ setFlag flagDecimal),
        (0xF9, Just $ iSBC (addrAbsoluteY >>= peek)),
        (0xFA, Nothing),
        (0xFB, Nothing),
        (0xFC, Nothing),
        (0xFD, Just $ iSBC (addrAbsoluteX >>= peek)),
        (0xFE, Just $ iINC addrAbsoluteX),
        (0xFF, Nothing)
    ]
    where
        iORA :: Monad m => H6502T m Word8 -> H6502T m ()
        iORA mm = do
            m <- mm
            a <- getA
            let newa = a .|. m
            setA newa
            putFlag flagZero (newa == 0)
            putFlag flagNegative (testBit newa 7)

        iASL :: Monad m => H6502T m Word16 -> H6502T m ()
        iASL maddr = do
            addr <- maddr
            m <- peek addr
            let newm = shiftL m 1
            poke addr newm
            putFlag flagCarry (testBit m 7)
            putFlag flagNegative (testBit newm 7)
            putFlag flagZero (newm == 0)

        branchIf :: Monad m => H6502T m Bool -> H6502T m ()
        branchIf mb = do
            rel <- peekPC
            b <- mb
            when b $ do
                pc <- getPC
                setPC (pc + fromIntegral rel - if testBit rel 7 then 0x100 else 0)

        branchIfNot :: Monad m => H6502T m Bool -> H6502T m ()
        branchIfNot mb = do
            rel <- peekPC
            b <- mb
            when (not b) $ do
                pc <- getPC
                setPC (pc + fromIntegral rel - if testBit rel 7 then 0x100 else 0)

        iAND :: Monad m => H6502T m Word8 -> H6502T m ()
        iAND mm = do
            m <- mm
            a <- getA
            let newa = a .&. m
            setA newa
            putFlag flagZero (newa == 0)
            putFlag flagNegative (testBit newa 7)

        iBIT :: Monad m => H6502T m Word8 -> H6502T m ()
        iBIT mm = do
            m <- mm
            a <- getA
            let r = a .&. m
            putFlag flagZero (r == 0)
            putFlag flagOverflow (testBit m 6)
            putFlag flagNegative (testBit m 7)

        iROL :: Monad m => H6502T m Word16 -> H6502T m ()
        iROL maddr = do
            addr <- maddr
            m <- peek addr
            cf <- getFlag flagCarry
            let newm = shiftL m 1 .|. if cf then 1 else 0
            poke addr newm
            putFlag flagCarry (testBit m 7)
            putFlag flagNegative (testBit newm 7)
            putFlag flagZero (newm == 0)

        iEOR :: Monad m => H6502T m Word8 -> H6502T m ()
        iEOR mm = do
            m <- mm
            a <- getA
            let newa = xor a m
            setA newa
            putFlag flagZero (newa == 0)
            putFlag flagNegative (testBit newa 7)

        iLSR :: Monad m => H6502T m Word16 -> H6502T m ()
        iLSR maddr = do
            addr <- maddr
            m <- peek addr
            let newm = shiftR m 1
            poke addr newm
            putFlag flagCarry (testBit m 0)
            putFlag flagNegative (testBit newm 7)
            putFlag flagZero (newm == 0)

        iROR :: Monad m => H6502T m Word16 -> H6502T m ()
        iROR maddr = do
            addr <- maddr
            m <- peek addr
            cf <- getFlag flagCarry
            let newm = shiftR m 1 .|. if cf then 0x80 else 0
            poke addr newm
            putFlag flagCarry (testBit m 0)
            putFlag flagNegative (testBit newm 7)
            putFlag flagZero (newm == 0)

        iADC :: Monad m => H6502T m Word8 -> H6502T m ()
        iADC mm = do
            m <- mm
            a <- getA
            cf <- getFlag flagCarry
            d <- getFlag flagDecimal
            if d
            then do
                let ah = shiftR a 4; al = a .&. 0xF
                let mh = shiftR m 4; ml = m .&. 0xF
                let sl = al + ml + if cf then 1 else 0
                let asl = sl + if sl >= 10 then 6 else 0
                let sh = ah + mh + if sl >= 10 then 1 else 0
                let ash = sh + if sh >= 10 then 6 else 0
                let newa = shiftL ash 4 .|. (asl .&. 0xF)
                setA newa
                putFlag flagZero (newa == 0)
                putFlag flagCarry (sh >= 10)
                putFlag flagOverflow (testBit a 7 == testBit m 7 && testBit a 7 /= testBit newa 7)
                putFlag flagNegative (testBit newa 7)
            else do
                let newa = a + m + if cf then 1 else 0
                setA newa
                if cf
                    then putFlag flagCarry (newa <= a)
                    else putFlag flagCarry (newa < a)
                putFlag flagZero (newa == 0)
                putFlag flagOverflow (testBit a 7 == testBit m 7 && testBit a 7 /= testBit newa 7)
                putFlag flagNegative (testBit newa 7)

        iST :: Monad m => H6502T m Word8 -> H6502T m Word16 -> H6502T m ()
        iST mr maddr = do
            addr <- maddr
            r <- mr
            poke addr r

        iLD :: Monad m => (Word8 -> H6502T m ()) -> H6502T m Word8 -> H6502T m ()
        iLD f mm = do
            m <- mm
            f m
            putFlag flagZero (m == 0)
            putFlag flagNegative (testBit m 7)

        iCP :: Monad m => H6502T m Word8 -> H6502T m Word8 -> H6502T m ()
        iCP mr mm = do
            m <- mm
            r <- mr
            putFlag flagCarry (r >= m)
            putFlag flagZero (r == m)
            putFlag flagNegative (testBit (r - m) 7)

        iDEC :: Monad m => H6502T m Word16 -> H6502T m ()
        iDEC maddr = do
            addr <- maddr
            m <- peek addr
            let newm = m - 1
            poke addr newm
            putFlag flagZero (newm == 0)
            putFlag flagNegative (testBit newm 7)

        iSBC :: Monad m => H6502T m Word8 -> H6502T m ()
        iSBC mm = do
            m <- mm
            a <- getA
            cf <- getFlag flagCarry
            d <- getFlag flagDecimal
            if d
            then do
                let ah = shiftR a 4; al = a .&. 0xF
                let mh = shiftR m 4; ml = m .&. 0xF
                let sl = al - ml - if cf then 0 else 1
                let asl = sl - if sl >= 16 then 6 else 0
                let sh = ah - mh - if sl >= 16 then 1 else 0
                let ash = sh - if sh >= 16 then 6 else 0
                let newa = shiftL ash 4 .|. (asl .&. 0xF)
                setA newa
                putFlag flagZero (newa == 0)
                putFlag flagCarry (sh < 16)
                putFlag flagOverflow (testBit a 7 /= testBit m 7 && testBit a 7 /= testBit newa 7)
                putFlag flagNegative (testBit newa 7)
            else do
                let newa = a - m - if cf then 0 else 1
                setA newa
                if cf
                    then putFlag flagCarry (a >= m)
                    else putFlag flagCarry (a /= 0 && a > m)
                putFlag flagZero (newa == 0)
                putFlag flagOverflow (testBit a 7 /= testBit m 7 && testBit a 7 /= testBit newa 7)
                putFlag flagNegative (testBit newa 7)

        iINC :: Monad m => H6502T m Word16 -> H6502T m ()
        iINC maddr = do
            addr <- maddr
            m <- peek addr
            let newm = m + 1
            poke addr newm
            putFlag flagZero (newm == 0)
            putFlag flagNegative (testBit newm 7)
