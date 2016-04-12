{-|
Module      : Language.X86.Core
Description : Abstract representation of x86 assembly
Copyright   : (c) Jacob Errington and Frederic Lafrance, 2016
License     : MIT
Maintainer  : goto@mail.jerrington.me
Stability   : experimental

Monad that represents an abstract form of x86 with labels.
-}

{-# LANGUAGE DeriveFunctor #-}

module Language.X86.Core
( -- * The @Asm@ monad
  Asm
, AsmF(..)
  -- ** Operations in 'Asm'
  -- *** Label management
, here
, newLabel
, newLabelHere
, setLabel
, setLabelHere
  -- *** Emitting instructions
, ret
, call
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
, test
, sal
, sar
, cmp
, jump
  -- * x86 instructions
, Instruction(..)
  -- ** Jumps
, JumpVariant(..)
, JumpDistance(..)
  -- ** Operands
, Operand(..)
, Offset(..)
, Scale(..)
, Displacement
, Immediate
, FloatingRegister(..)
, IntegerRegister(..)
, RegisterAccessMode(..)
, HardwareRegister(..)
, RegisterSize(..)
, SizedRegister(..)
, registerIndex
, hwxmm
  -- * Misc
, Signedness(..)
) where

import Control.Monad.Free
import Data.Int
import Data.Word

-- | The base functor for 'Asm'.
--
-- The types for addresses, labels, and values are left abstract so that we can
-- reuse this base functor in different situations:
-- * Pretty-printing: @label@ should be some stringy type
-- * Generating machine code: @label@ should be a unique identifier
--
-- Normally, @label@, @addr@ and @val@ should be related somehow. For example,
-- if we want to allow jumping to labels, then @val@ should be some sum type in
-- which one summand contains @label@.
data AsmF addr label val next
    = Emit (Instruction val) next
    -- ^ Output an instruction.
    | NewLabel (label -> next)
    -- ^ Create a new uninitialized label.
    | SetLabel label addr next
    -- ^ Assign a value to a label.
    | Here (addr -> next)
    -- ^ Get the current code offset.
    deriving (Functor)

-- | The free monad on 'AsmF'.
type Asm reg addr label = Free (AsmF addr label (Operand reg addr label))

-- | An x86 instruction
data Instruction val
    = Ret
    -- ^ Return from a function; 'ret'.
    | Call val
    -- ^ Call a function; 'call'.
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
    | Test val val
    -- ^ Logical AND; 'test'.
    | Sal val val
    -- ^ Arithmetic left shift; 'sal'.
    | Sar val val
    -- ^ Arithmetic right shift; 'sar'.
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
    = JMP
    -- ^ Jump unconditionally.
    | JO
    -- ^ Jump on overflow, @OF = 1@.
    | JNO
    -- ^ Jump on no overflow, @OF = 0@.
    | JS
    -- ^ Jump on sign, @SF = 1@.
    | JNS
    -- ^ Jump on no sign, @SF = 0@.
    | JE
    -- ^ Jump on equal, @ZF = 1@.
    --
    -- Synonymous with @jz@, jump on zero.
    | JNE
    -- ^ Jump on not equal, @ZF = 0@.
    --
    -- Synonymous with @jnz@, jump on not zero.
    | JB Signedness
    -- ^ If unsigned, then jump if /below/, @CF = 1@.
    --
    -- Synonymous with @jnae@, jump if not /above/ or equal, and @jc@ jump on
    -- carry.
    --
    -- If signed, then jump if /less/, @SF /= OF@.
    --
    -- Synonymous with @jnge@, jump if not /greater than/ or equal.
    | JNB Signedness
    -- ^ If unsigned, then jump if not /below/, @CF = 0@.
    --
    -- Synonymous with @jae@, jump if above or equal, and @jnc@ jump on no
    -- carry.
    --
    -- If signed, then jump if /greater than/ or equal.
    --
    -- Synonymous with @jnl@, jump if not /less/.
    | JBE Signedness
    -- ^ If unsigned, then jump if /below/ or equal, @CF = 1@ or @ZF = 1@.
    --
    -- Synonymous with @jna@, jump if not /above/.
    --
    -- If signed, then jump if /less than/ or equal, @ZF = 1$ or @SF /= OF@.
    --
    -- Synonymous with @jng@, jump if not /greater/.
    | JA Signedness
    -- ^ If unsigned, then jump if /above/, @CF = 0@ and @ZF = 0@.
    --
    -- Synonymous with @jnbe@, jump if not /below/ or equal.
    --
    -- If signed, then jump if /greater than/, @ZF = 0@ and @SF = OF@.
    --
    -- Synonymous with @jnle@, jump if not /less than/ or equal.
    | JPE
    -- ^ Jump if parity even, @PF = 1@.
    | JPO
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

-- | A concrete integer register.
data IntegerRegister
    = Rax
    | Rbx
    | Rcx
    | Rdx
    | Rbp
    | Rsi
    | Rdi
    | Rsp
    | R8
    | R9
    | R10
    | R11
    | R12
    | R13
    | R14
    | R15
    deriving (Eq, Ord, Read, Show)

-- | A concrete floating point register
data FloatingRegister
    = FloatingRegister Int
    deriving (Eq, Ord, Read, Show)

-- | A concrete register.
data HardwareRegister
    = IntegerHwRegister IntegerRegister
    | FloatingHwRegister FloatingRegister
    deriving (Eq, Ord, Read, Show)

-- | Gets the index of a register.
registerIndex :: HardwareRegister -> Int
registerIndex r = case r of
    IntegerHwRegister reg -> case reg of
        Rax -> 0
        Rcx -> 1
        Rdx -> 2
        Rbx -> 3
        Rsp -> 4
        Rbp -> 5
        Rsi -> 6
        Rdi -> 7
        R8 -> 8
        R9 -> 9
        R10 -> 10
        R11 -> 11
        R12 -> 12
        R13 -> 13
        R14 -> 14
        R15 -> 15
    FloatingHwRegister (FloatingRegister n) -> n

-- | An SSE register.
hwxmm :: Int -> HardwareRegister
hwxmm = FloatingHwRegister . FloatingRegister

-- | A size modifier for a register.
data RegisterSize
    = Low8
    -- ^ The lowest eight bits of the register, e.g. @al@.
    | High8
    -- ^ The high eight bits of the lowest sixteen bits of the register, e.g.
    -- @ah@.
    | Extended16
    -- ^ The low sixteen bits of the register, e.g. @ax@.
    | Extended32
    -- ^ The low 32 bits of the register, e.g. @eax@.
    | Extended64
    -- ^ The full 64 bits of the register, e.g. @rax@.
    deriving (Eq, Ord, Read, Show)

data RegisterAccessMode
    = IntegerMode
    | FloatingMode
    | MemoryMode
    deriving (Eq, Ord, Read, Show)

-- | A register together with a size modifier.
data SizedRegister reg = SizedRegister RegisterSize reg
    deriving (Eq, Ord, Read, Show)

-- | An operand to an instruction.
data Operand reg addr label
    = Immediate Immediate
    -- ^ A constant value.
    | DirectRegister reg
    -- ^ The value contained in a register.
    | IndirectRegister (Offset reg)
    -- ^ The value contained in a memory address indicated by the value of a
    -- register together with a signed offset.
    | Address addr
    -- ^ An address
    | Label label
    -- ^ A internal label to a location within the current function.
    | Internal String
    -- ^ An internal reference, e.g. to another compiled Go function.
    | External String
    -- ^ An external reference, e.g. to a C function.

data Offset reg
    = Offset Displacement reg
    -- ^ A register indirection with a displacement.
    --
    -- More space-efficient code can be generated if the displacement is small.
    | ScaledIndexBase Scale Displacement reg reg
    -- ^ Scaled index base addressing can be used to concisely express array
    -- indexing.

-- | A multiplier on on an index.
data Scale
    = ScaleByte
    | ScaleShort
    | ScaleWord
    | ScaleDword

type Displacement = Int64
type Immediate = Word64

-- | Nullary instruction.
type Instr0 reg addr label a = Asm reg addr label a

-- | Unary instruction.
type Instr1 reg addr label a = Operand reg addr label -> Instr0 reg addr label a

-- | Binary instruction.
type Instr2 reg addr label a = Operand reg addr label -> Instr1 reg addr label a

-- | 'NewLabel'
newLabel :: Asm reg addr label label
newLabel = liftF . NewLabel $ id

-- | 'SetLabel'
setLabel :: label -> addr -> Asm reg addr label ()
setLabel l a = liftF . SetLabel l a $ ()

-- | 'Here'
here :: Asm reg addr label addr
here = liftF . Here $ id

-- | Defines a new label and immediately sets it to the value obtained from
-- 'here'.
newLabelHere :: Asm reg addr label label
newLabelHere = do
    l <- newLabel
    setLabel <$> pure l <*> here
    pure l

setLabelHere :: label -> Asm reg addr label ()
setLabelHere l = setLabel l =<< here

-- | 'Emit' 'Ret'
ret :: Instr0 reg addr label ()
ret = liftF . Emit Ret $ ()

call :: Instr1 reg addr label ()
call f = liftF . Emit (Call f) $ ()

-- | 'Emit' 'Mov'
mov :: Instr2 reg addr label ()
mov x y = liftF . Emit (Mov x y) $ ()

-- | 'Emit' 'Add'
add :: Instr2 reg addr label ()
add x y = liftF . Emit (Add x y) $ ()

-- | 'Emit' 'Sub'
sub :: Instr2 reg addr label ()
sub x y = liftF . Emit (Sub x y) $ ()

-- | 'Emit' 'Mul'
mul :: Signedness
    -> Operand reg addr label
    -> Operand reg addr label
    -> Maybe (Operand reg addr label)
    -> Asm reg addr label ()
mul s x y z = liftF . Emit (Mul s x y z) $ ()

xor :: Instr2 reg addr label ()
xor x y = liftF . Emit (Xor x y) $ ()

inc :: Instr1 reg addr label ()
inc v = liftF . Emit (Inc v) $ ()

dec :: Instr1 reg addr label ()
dec v = liftF . Emit (Dec v) $ ()

push :: Instr1 reg addr label ()
push v = liftF . Emit (Push v) $ ()

pop :: Instr1 reg addr label ()
pop v = liftF . Emit (Pop v) $ ()

nop :: Instr0 reg addr label ()
nop = liftF . Emit Nop $ ()

syscall :: Instr0 reg addr label ()
syscall = liftF . Emit Syscall $ ()

int :: Instr1 reg addr label ()
int v = liftF . Emit (Int v) $ ()

cmp :: Instr2 reg addr label ()
cmp x y = liftF . Emit (Cmp x y) $ ()

test :: Instr2 reg addr label ()
test x y = liftF . Emit (Test x y) $ ()

sal :: Instr2 reg addr label ()
sal x y = liftF . Emit (Sal x y) $ ()

sar :: Instr2 reg addr label ()
sar x y = liftF . Emit (Sar x y) $ ()

jump :: JumpVariant -> Instr1 reg addr label ()
jump v x = liftF . Emit (Jump v x) $ ()
