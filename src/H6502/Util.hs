module H6502.Util
    (
        setPC, setS, setA, setX, setY,
        setFlag, unsetFlag, setFlags, putFlag,
        onState, getPC, getS, getA, getX, getY,
        getFlag, getFlags,
        wordJoin, wordSplit,
        peek, poke, peek16, poke16, peekPC, peekPC16,
        pop, push, pop16, push16,
        addrZeroPage, addrZeroPageX, addrZeroPageY, addrAbsolute, addrAbsoluteX, addrAbsoluteY, addrIxIndirect, addrIndirectIx 
    )
    where

import Data.Bits
import Data.Word

import H6502.Env
import H6502.State
import H6502.Trans

modifyState :: Monad m => (H6502State -> H6502State) -> H6502T m ()
modifyState f = H6502T $ \e s -> return ((), f s)

setPC :: Monad m => Word16 -> H6502T m ()
setPC pc = modifyState $ \s -> s{reg_pc = pc}

setS :: Monad m => Word8 -> H6502T m ()
setS sp = modifyState $ \s -> s{reg_s = sp}

setA :: Monad m => Word8 -> H6502T m ()
setA a = modifyState $ \s -> s{reg_a = a}

setX :: Monad m => Word8 -> H6502T m ()
setX x = modifyState $ \s -> s{reg_x = x}

setY :: Monad m => Word8 -> H6502T m ()
setY y = modifyState $ \s -> s{reg_y = y}

setFlag :: Monad m => Word8 -> H6502T m ()
setFlag x = modifyState $ \s -> s{reg_flags = reg_flags s .|. x}

unsetFlag :: Monad m => Word8 -> H6502T m ()
unsetFlag x = modifyState $ \s -> s{reg_flags = reg_flags s .&. complement x}

putFlag :: Monad m => Word8 -> Bool -> H6502T m ()
putFlag x True = setFlag x
putFlag x False = unsetFlag x

setFlags :: Monad m => Word8 -> H6502T m ()
setFlags x = H6502T $ \e s -> return ((), s{reg_flags = x .|. reservedFlags})

onState :: Monad m => (H6502State -> a) -> H6502T m a
onState f = H6502T $ \e s -> return (f s, s)

getPC :: Monad m => H6502T m Word16
getPC = onState reg_pc

getS :: Monad m => H6502T m Word8
getS = onState reg_s

getA :: Monad m => H6502T m Word8
getA = onState reg_a

getX :: Monad m => H6502T m Word8
getX = onState reg_x

getY :: Monad m => H6502T m Word8
getY = onState reg_y

getFlag :: Monad m => Word8 -> H6502T m Bool
getFlag x = H6502T $ \e s -> return (reg_flags s .&. x /= 0, s)

getFlags :: Monad m => H6502T m Word8
getFlags = H6502T $ \e s -> return (reg_flags s, s)

wordJoin :: Word8 -> Word8 -> Word16
wordJoin hi lo = shiftL (fromIntegral hi) 8 .|. fromIntegral lo

wordSplit :: Word16 -> (Word8, Word8)
wordSplit w = (fromIntegral (shiftR w 8), fromIntegral w)

peek :: Monad m => Word16 -> H6502T m Word8
peek a = H6502T $ \e s -> do
    r <- memoryRead e a
    return (r, s)

poke :: Monad m => Word16 -> Word8 -> H6502T m ()
poke a x = H6502T $ \e s -> do
    memoryWrite e a x
    return ((), s)

peek16 :: Monad m => Word16 -> H6502T m Word16
peek16 a = H6502T $ \e s -> do
    lo <- memoryRead e a
    hi <- memoryRead e (a + 1)
    return (wordJoin hi lo, s)

poke16 :: Monad m => Word16 -> Word16 -> H6502T m ()
poke16 a x = H6502T $ \e s -> do
    let ~(hi, lo) = wordSplit x
    memoryWrite e a lo
    memoryWrite e (a + 1) hi
    return ((), s)

onStack :: Word8 -> Word16
onStack x = fromIntegral x .|. 0x0100

onZeroPage :: Word8 -> Word16
onZeroPage x = fromIntegral x

pop :: Monad m => H6502T m Word8
pop = H6502T $ \e s -> do
    let sp = reg_s s
    r <- memoryRead e (onStack $ sp + 1)
    return (r, s{reg_s = sp + 1})

push :: Monad m => Word8 -> H6502T m ()
push x = H6502T $ \e s -> do
    let sp = reg_s s
    memoryWrite e (onStack sp) x
    return ((), s{reg_s = sp - 1})

pop16 :: Monad m => H6502T m Word16
pop16 = H6502T $ \e s -> do
    let sp = reg_s s
    lo <- memoryRead e (onStack $ sp + 1)
    hi <- memoryRead e (onStack $ sp + 2)
    return (wordJoin hi lo, s{reg_s = sp + 2})

push16 :: Monad m => Word16 -> H6502T m ()
push16 x = H6502T $ \e s -> do
    let ~(hi, lo) = wordSplit x
    let sp = reg_s s
    memoryWrite e (onStack $ sp - 1) lo
    memoryWrite e (onStack sp) hi
    return ((), s{reg_s = sp - 2})

peekPC :: Monad m => H6502T m Word8
peekPC = H6502T $ \e s -> do
    let pc = reg_pc s
    r <- memoryRead e pc
    return (r, s{reg_pc = pc + 1})

peekPC16 :: Monad m => H6502T m Word16
peekPC16 = H6502T $ \e s -> do
    let pc = reg_pc s
    lo <- memoryRead e pc
    hi <- memoryRead e (pc + 1)
    return (wordJoin hi lo, s{reg_pc = pc + 2})

addrZeroPage :: Monad m => H6502T m Word16
addrZeroPage = do
    imm <- peekPC
    return (onZeroPage imm)

addrZeroPageX :: Monad m => H6502T m Word16
addrZeroPageX = do
    imm <- peekPC
    x <- getX
    return (onZeroPage $ imm + x)

addrZeroPageY :: Monad m => H6502T m Word16
addrZeroPageY = do
    imm <- peekPC
    y <- getY
    return (onZeroPage $ imm + y)

addrAbsolute :: Monad m => H6502T m Word16
addrAbsolute = peekPC16

addrAbsoluteX :: Monad m => H6502T m Word16
addrAbsoluteX = do
    imm <- peekPC16
    x <- getX
    return (imm + fromIntegral x)

addrAbsoluteY :: Monad m => H6502T m Word16
addrAbsoluteY = do
    imm <- peekPC16
    y <- getY
    return (imm + fromIntegral y)

addrIxIndirect :: Monad m => H6502T m Word16
addrIxIndirect = do
    imm <- peekPC
    x <- getX
    peek16 (onZeroPage $ imm + x)

addrIndirectIx :: Monad m => H6502T m Word16
addrIndirectIx = do
    imm <- peekPC
    y <- getY
    addr <- peek16 (onZeroPage imm)
    return (addr + fromIntegral y)
