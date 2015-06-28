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

-- | Set the program counter.
setPC :: Monad m => Word16 -> H6502T m ()
setPC pc = modifyState $ \s -> s{reg_pc = pc}

-- | Set the stack pointer.
setS :: Monad m => Word8 -> H6502T m ()
setS sp = modifyState $ \s -> s{reg_s = sp}

-- | Set the accumulator.
setA :: Monad m => Word8 -> H6502T m ()
setA a = modifyState $ \s -> s{reg_a = a}

-- | Set the X index.
setX :: Monad m => Word8 -> H6502T m ()
setX x = modifyState $ \s -> s{reg_x = x}

-- | Set the Y index.
setY :: Monad m => Word8 -> H6502T m ()
setY y = modifyState $ \s -> s{reg_y = y}

-- | Turn the given flag on, if not already.
setFlag :: Monad m => Word8 -> H6502T m ()
setFlag x = modifyState $ \s -> s{reg_flags = reg_flags s .|. x}

-- | Turn the given flag off, if not already.
unsetFlag :: Monad m => Word8 -> H6502T m ()
unsetFlag x = modifyState $ \s -> s{reg_flags = reg_flags s .&. complement x}

-- | Set the state of then given flag to the given boolean expression.
putFlag :: Monad m => Word8 -> Bool -> H6502T m ()
putFlag x True = setFlag x
putFlag x False = unsetFlag x

-- | Set the flags register.
setFlags :: Monad m => Word8 -> H6502T m ()
setFlags x = H6502T $ \e s -> return ((), s{reg_flags = x .|. reservedFlags})

onState :: Monad m => (H6502State -> a) -> H6502T m a
onState f = H6502T $ \e s -> return (f s, s)

-- | Get the program counter.
getPC :: Monad m => H6502T m Word16
getPC = onState reg_pc

-- | Get the stack pointer.
getS :: Monad m => H6502T m Word8
getS = onState reg_s

-- | Get the accumulator.
getA :: Monad m => H6502T m Word8
getA = onState reg_a

-- | Get the X index.
getX :: Monad m => H6502T m Word8
getX = onState reg_x

-- | Get the Y index.
getY :: Monad m => H6502T m Word8
getY = onState reg_y

-- | Get the state of the given flag.
getFlag :: Monad m => Word8 -> H6502T m Bool
getFlag x = H6502T $ \e s -> return (reg_flags s .&. x /= 0, s)

-- | Get the flags register.
getFlags :: Monad m => H6502T m Word8
getFlags = H6502T $ \e s -> return (reg_flags s, s)

-- | Join two 8-bit bytes into a 16-bit word.
wordJoin :: Word8 -> Word8 -> Word16
wordJoin hi lo = shiftL (fromIntegral hi) 8 .|. fromIntegral lo

-- | Split a 16-bit word into two 8-bit bytes.
wordSplit :: Word16 -> (Word8, Word8)
wordSplit w = (fromIntegral (shiftR w 8), fromIntegral w)

-- | Read a byte from the given adddress.
peek :: Monad m => Word16 -> H6502T m Word8
peek a = H6502T $ \e s -> do
    r <- memoryRead e a
    return (r, s)

-- | Write a byte to the given address.
poke :: Monad m => Word16 -> Word8 -> H6502T m ()
poke a x = H6502T $ \e s -> do
    memoryWrite e a x
    return ((), s)

-- | Read a little-endian word from a given address.
peek16 :: Monad m => Word16 -> H6502T m Word16
peek16 a = H6502T $ \e s -> do
    lo <- memoryRead e a
    hi <- memoryRead e (a + 1)
    return (wordJoin hi lo, s)

-- | Write a little-endian word to a given address.
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

-- | Pop a byte from the stack.
pop :: Monad m => H6502T m Word8
pop = H6502T $ \e s -> do
    let sp = reg_s s
    r <- memoryRead e (onStack $ sp + 1)
    return (r, s{reg_s = sp + 1})

-- | Push a byte onto the stack.
push :: Monad m => Word8 -> H6502T m ()
push x = H6502T $ \e s -> do
    let sp = reg_s s
    memoryWrite e (onStack sp) x
    return ((), s{reg_s = sp - 1})

-- | Pop a little-endian word from the stack.
pop16 :: Monad m => H6502T m Word16
pop16 = H6502T $ \e s -> do
    let sp = reg_s s
    lo <- memoryRead e (onStack $ sp + 1)
    hi <- memoryRead e (onStack $ sp + 2)
    return (wordJoin hi lo, s{reg_s = sp + 2})

-- | Push a little-endian word from the stack.
push16 :: Monad m => Word16 -> H6502T m ()
push16 x = H6502T $ \e s -> do
    let ~(hi, lo) = wordSplit x
    let sp = reg_s s
    memoryWrite e (onStack $ sp - 1) lo
    memoryWrite e (onStack sp) hi
    return ((), s{reg_s = sp - 2})

-- | Read a byte at the location of the program counter and then increment it.
peekPC :: Monad m => H6502T m Word8
peekPC = H6502T $ \e s -> do
    let pc = reg_pc s
    r <- memoryRead e pc
    return (r, s{reg_pc = pc + 1})

-- | Read a little-endian word at the location of the program counter and then increment it by two.
peekPC16 :: Monad m => H6502T m Word16
peekPC16 = H6502T $ \e s -> do
    let pc = reg_pc s
    lo <- memoryRead e pc
    hi <- memoryRead e (pc + 1)
    return (wordJoin hi lo, s{reg_pc = pc + 2})

-- | ZeroPage addressing mode: read a byte from the instruction, and use it as an offset into the zero page.
addrZeroPage :: Monad m => H6502T m Word16
addrZeroPage = do
    imm <- peekPC
    return (onZeroPage imm)

-- | ZeroPage,X addressing mode: read a byte from the instruction, add it to X (with 8-bit addition), and use it as an offset inot the zero page.
addrZeroPageX :: Monad m => H6502T m Word16
addrZeroPageX = do
    imm <- peekPC
    x <- getX
    return (onZeroPage $ imm + x)

-- | ZeroPage,Y addressing mode: read a byte from the instruction, add it to Y (with 8-bit addition), and use it as an offset into the zero page.
addrZeroPageY :: Monad m => H6502T m Word16
addrZeroPageY = do
    imm <- peekPC
    y <- getY
    return (onZeroPage $ imm + y)

-- | Absolute addressing mode: read a word from the instruction, and use it as an address
addrAbsolute :: Monad m => H6502T m Word16
addrAbsolute = peekPC16

-- | Absolute,X addressing mode: read a word from the instruction, add it to X, and use it as an address
addrAbsoluteX :: Monad m => H6502T m Word16
addrAbsoluteX = do
    imm <- peekPC16
    x <- getX
    return (imm + fromIntegral x)

-- | Absolute,Y addressing mode: read a word from the instruction, add it to Y, and use it as an address
addrAbsoluteY :: Monad m => H6502T m Word16
addrAbsoluteY = do
    imm <- peekPC16
    y <- getY
    return (imm + fromIntegral y)

-- | (Indirect,X) addressing mode: read a byte from the instruction, add it to X, use the result as an offset into the zero page, read a word at that offset, and use that as an address
addrIxIndirect :: Monad m => H6502T m Word16
addrIxIndirect = do
    imm <- peekPC
    x <- getX
    peek16 (onZeroPage $ imm + x)

-- | (Indirect),Y addressing mode: read a byte from the instruction, use it as an offset into the zero page, read a word at that offset, add Y, and use that as an address
addrIndirectIx :: Monad m => H6502T m Word16
addrIndirectIx = do
    imm <- peekPC
    y <- getY
    addr <- peek16 (onZeroPage imm)
    return (addr + fromIntegral y)
