module H6502
    (
        H6502T(..),
        executeInstruction,
        tryTriggerIRQ,
        triggerNMI,
        defaultState,
        withH6502,
        H6502Environment(..)
    )
    where

import Data.Array
import Data.Bits

import H6502.Env
import H6502.OpcodeMap
import H6502.State
import H6502.Trans
import H6502.Util

-- | Run a 6502 transformer action discarding the result.
withH6502 :: Monad m => H6502Environment m -> H6502State -> H6502T m a -> m H6502State
withH6502 e s m = do
    (_, s) <- runH6502T m e s
    return s

-- | Execute a single instruction.
executeInstruction :: Monad m => H6502T m ()
executeInstruction = do
    w <- peekPC
    case opcodeMap ! w of
        Just e -> e
        Nothing -> return ()

-- | Attempt triggering an IRQ. The function returns 'True' if the IRQ has been triggered, and 'False' if the interrupt disable flag has been set.
tryTriggerIRQ :: Monad m => H6502T m Bool
tryTriggerIRQ = do
    i <- getFlag flagInterrupt
    if i
        then return False
        else do
            pc <- getPC
            flags <- getFlags
            push16 (pc + 1)
            push (flags .&. complement flagBrk)
            irq <- peek16 0xFFFE
            setPC irq
            setFlag flagInterrupt
            return True

-- | Trigger a non-maskable interrupt.
triggerNMI :: Monad m => H6502T m ()
triggerNMI = do
    pc <- getPC
    flags <- getFlags
    push16 (pc + 1)
    push (flags .&. complement flagBrk)
    irq <- peek16 0xFFFA
    setPC irq
    setFlag flagInterrupt
