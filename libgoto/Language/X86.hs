{-|
Module      : Language.X86
Description : Abstract representation of x86 assembly
Copyright   : (c) Jacob Errington and Frederic Lafrance, 2016
License     : MIT
Maintainer  : goto@mail.jerrington.me
Stability   : experimental

Monad that represents an assembler with simple label support.
-}

{-# LANGUAGE DeriveFunctor #-}

module Language.X86
( -- * The @Assembler@ monad
  Assembler
, AssemblerF(..)
  -- ** Operations in 'Assembler'
  -- *** Label management
, newLabel
, setLabel
, here
, newLabelHere
  -- *** Emitting instructions
, ret
, mov
, add
, sub
, mul
, xor
, inc
, dec
, push
, pop
, nop
, syscall
, int
, cmp
, jump
  -- * x86 instructions
, Instruction(..)
, JumpVariant(..)
, JumpDistance(..)
, Signedness(..)
) where

import Control.Monad.Free

-- | The base functor for 'Assembler'.
--
-- The types for addresses, labels, and values are left abstract so that we can
-- reuse this base functor in different situations:
-- * Pretty-printing: @label@ should be some stringy type
-- * Generating machine code: @label@ should be a unique identifier
--
-- Normally, @label@, @addr@ and @val@ should be related somehow. For example,
-- if we want to allow jumping to labels, then @val@ should be some sum type in
-- which one summand contains @label@.
data AssemblerF addr label val next
    = Emit (Instruction val) next
    -- ^ Output an instruction.
    | NewLabel (label -> next)
    -- ^ Create a new uninitialized label.
    | SetLabel label addr next
    -- ^ Assign a value to a label.
    | Here (addr -> next)
    -- ^ Get the current code offset.
    deriving (Functor)

-- | The free monad on 'AssemblerF'.
type Assembler addr label val = Free (AssemblerF addr label val)

-- | An x86 instruction
data Instruction val
    = Ret
    -- ^ Return from a function; 'ret'.
    | Mov val val
    -- ^ Move data; 'mov'.
    | Add val val
    -- ^ Addition; 'add'.
    | Sub val val
    -- ^ Subtraction; 'sub'.
    | Mul Signedness val val (Maybe val)
    -- ^ Multiplication; 'mul'.
    | Xor val val
    -- ^ Bitwise exclusive OR; 'xor'.
    | Inc val
    -- ^ Increment; 'inc'.
    | Dec val
    -- ^ Decrement; 'dec'.
    | Push val
    -- ^ Push onto the stack; 'push'.
    | Pop val
    -- ^ Pop from the stack; 'pop'.
    | Nop
    -- ^ Do nothing; 'nop'.
    | Syscall
    -- ^ Perform a syscall; 'syscall'.
    | Int val
    -- ^ Send an interrupt; 'int'.
    | Cmp val val
    -- ^ Perform a comparison; 'cmp'.
    | Jump JumpVariant val
    -- ^ Perform a jump; 'jump'.

-- | Variants of jumps.
--
-- * @CF@ - /carry/ flag: set on high-order bit carry (or borrow in case of
--   subtractions).
-- * @PF@ - /parity/ flag: set on low-order eight bits having an even
--   population count.
-- * @ZF@ - /zero/ flag: set when result is equal to zero.
-- * @SF@ - /sign/ flag: set to high-order bit of result; zero is positive, and
--   one is negative.
-- * @OF@ - /overflow/ flag: set if result is too large a positive number or
--   too small a negative number (excluding the sign bit) to fit in the
--   destination operand.
data JumpVariant
    = JMP JumpDistance
    -- ^ Jump unconditionally.
    | JO JumpDistance
    -- ^ Jump on overflow, @OF = 1@.
    | JNO JumpDistance
    -- ^ Jump on no overflow, @OF = 0@.
    | JS JumpDistance
    -- ^ Jump on sign, @SF = 1@.
    | JNS JumpDistance
    -- ^ Jump on no sign, @SF = 0@.
    | JE JumpDistance
    -- ^ Jump on equal, @ZF = 1@.
    --
    -- Synonymous with @jz@, jump on zero.
    | JNE JumpDistance
    -- ^ Jump on not equal, @ZF = 0@.
    --
    -- Synonymous with @jnz@, jump on not zero.
    | JB Signedness JumpDistance
    -- ^ If unsigned, then jump if /below/, @CF = 1@.
    --
    -- Synonymous with @jnae@, jump if not /above/ or equal, and @jc@ jump on
    -- carry.
    --
    -- If signed, then jump if /less/, @SF /= OF@.
    --
    -- Synonymous with @jnge@, jump if not /greater than/ or equal.
    | JNB Signedness JumpDistance
    -- ^ If unsigned, then jump if not /below/, @CF = 0@.
    --
    -- Synonymous with @jae@, jump if above or equal, and @jnc@ jump on no
    -- carry.
    --
    -- If signed, then jump if /greater than/ or equal.
    --
    -- Synonymous with @jnl@, jump if not /less/.
    | JBE Signedness JumpDistance
    -- ^ If unsigned, then jump if /below/ or equal, @CF = 1@ or @ZF = 1@.
    --
    -- Synonymous with @jna@, jump if not /above/.
    --
    -- If signed, then jump if /less than/ or equal, @ZF = 1$ or @SF /= OF@.
    --
    -- Synonymous with @jng@, jump if not /greater/.
    | JA Signedness JumpDistance
    -- ^ If unsigned, then jump if /above/, @CF = 0@ and @ZF = 0@.
    --
    -- Synonymous with @jnbe@, jump if not /below/ or equal.
    --
    -- If signed, then jump if /greater than/, @ZF = 0@ and @SF = OF@.
    --
    -- Synonymous with @jnle@, jump if not /less than/ or equal.
    | JPE JumpDistance
    -- ^ Jump if parity even, @PF = 1@.
    | JPO JumpDistance
    -- ^ Jump if parity odd, @PF = 0@.
    | JCXZ
    -- ^ Jump on @CX@ (@ECX@) equal to zero.

-- | Indicates whether an instruction operates on signed or unsigned data.
data Signedness
    = Signed
    | Unsigned

-- | The distance of a jump.
data JumpDistance
    = JumpShort
    -- ^ Relative jump by a one-byte signed integer.
    | JumpNear
    -- ^ Relative jump by a two-byte signed integer.

-- | 'NewLabel'
newLabel :: Assembler addr label val label
newLabel = liftF . NewLabel $ id

-- | 'SetLabel'
setLabel :: label -> addr -> Assembler addr label val ()
setLabel l a = liftF . SetLabel l a $ ()

-- | 'Here'
here :: Assembler addr label val addr
here = liftF . Here $ id

-- | Defines a new label and immediately sets it to the value obtained from
-- 'here'.
newLabelHere :: Assembler addr label val label
newLabelHere = do
    l <- newLabel
    setLabel <$> pure l <*> here
    pure l

-- | 'Emit' 'Ret'
ret :: Assembler addr label val ()
ret = liftF . Emit Ret $ ()

-- | 'Emit' 'Mov'
mov :: val -> val -> Assembler addr label val ()
mov x y = liftF . Emit (Mov x y) $ ()

-- | 'Emit' 'Add'
add :: val -> val -> Assembler addr label val ()
add x y = liftF . Emit (Add x y) $ ()

-- | 'Emit' 'Sub'
sub :: val -> val -> Assembler addr label val ()
sub x y = liftF . Emit (Sub x y) $ ()

-- | 'Emit' 'Mul'
mul :: Signedness -> val -> val -> Maybe val -> Assembler addr label val ()
mul s x y z = liftF . Emit (Mul s x y z) $ ()

xor :: val -> val -> Assembler addr label val ()
xor x y = liftF . Emit (Xor x y) $ ()

inc :: val -> Assembler addr label val ()
inc v = liftF . Emit (Inc v) $ ()

dec :: val -> Assembler addr label val ()
dec v = liftF . Emit (Dec v) $ ()

push :: val -> Assembler addr label val ()
push v = liftF . Emit (Push v) $ ()

pop :: val -> Assembler addr label val ()
pop v = liftF . Emit (Pop v) $ ()

nop :: Assembler addr label val ()
nop = liftF . Emit Nop $ ()

syscall :: Assembler addr label val ()
syscall = liftF . Emit Syscall $ ()

int :: val -> Assembler addr label val ()
int v = liftF . Emit (Int v) $ ()

cmp :: val -> val -> Assembler addr label val ()
cmp x y = liftF . Emit (Cmp x y) $ ()

jump :: JumpVariant -> val -> Assembler addr label val ()
jump v x = liftF . Emit (Jump v x) $ ()
