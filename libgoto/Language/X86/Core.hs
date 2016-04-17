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
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Language.X86.Core
( -- * The @Asm@ monad
  AsmT
, Asm
, AsmF(..)
  -- ** Operations in 'Asm'
  -- *** Label management
, newLabel
, newLabelHere
, setLabel
  -- *** Emitting instructions
, ret
, call
, mov
, movq
, add
, sub
, imul
, idiv
, addsse
, subsse
, mulsse
, divsse
, bwand
, bwor
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
, setc
, neg1
, neg2
, pxor
, cvt
, cqo
, cmpsse
  -- *** Miscellaneous operations
, scratch
, withScratch
, prologue
, withPrologue
  -- * x86 instructions
, Instruction(..)
  -- ** Jumps
, FlagCondition(..)
, JumpDistance(..)
, invertFlag
  -- ** Operands
, Operand(..)
, Directness(..)
, Offset(..)
, Scale(..)
, Displacement
, Immediate(..)
, FloatingRegister(..)
, IntegerRegister(..)
, RegisterAccessMode(..)
, HardwareRegister(..)
, RegisterSize(..)
, SizedRegister(..)
, SseType(..)
, SsePredicate(..)
, registerIndex
, hwxmm
  -- * Misc
, Signedness(..)
, ScratchFlow(..)
) where

import Language.Common.Pretty hiding ( int )
import qualified Language.Common.Pretty as P
import Language.Common.Misc
import Language.Common.Storage

import Control.Monad.Identity
import Control.Monad.State
import Control.Monad.Trans.Free
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
data AsmF label val next
    = Emit (Instruction val) next
    -- ^ Output an instruction.
    | NewLabel (label -> next)
    -- ^ Create a new uninitialized label.
    | SetLabel label next
    -- ^ Assign a value to a label.
    | Scratch ScratchFlow next
    -- ^ Save or load live scratch registers. Load instructions must always
    -- occur after a save instruction. Saves and loads may not be nested.
    | Prologue ScratchFlow next
    deriving (Functor)

-- | Whether to save or load the scratch registers.
data ScratchFlow
    = Save
    | Load

-- | The free monad transformer on 'AsmF'.
type AsmT reg label = FreeT (AsmF label (Operand reg label))

type Asm reg label = AsmT reg label Identity

-- | An x86 instruction
data Instruction val
    = Ret
    -- ^ Return from a function; 'ret'.
    | Call val
    -- ^ Call a function; 'call'.
    | Mov val val
    -- ^ Move data; 'mov'.
    | And val val
    -- ^ Bitwise and; 'and'.
    | Or val val
    -- ^ Bitwise or; 'or'.
    | Add val val
    -- ^ Addition; 'add'.
    | AddSse SseType val val
    -- ^ Floating point addition; 'sseadd'.
    | SubSse SseType val val
    -- ^ Floating point subtraction; 'ssesub'.
    | MulSse SseType val val
    -- ^ Floating point multiplication; 'ssemul'.
    | DivSse SseType val val
    -- ^ Floating point division; 'ssediv'.
    | Sub val val
    -- ^ Subtraction; 'sub'.
    | Mul Signedness val val
    -- ^ Multiplication; 'imul'.
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
    | Jump FlagCondition val
    -- ^ Perform a jump; 'jump'.
    | Setc FlagCondition val
    -- ^ Perform a conditional byte set; 'setc'.
    | Neg1 val
    -- ^ Perform a one's complement (bitwise) negation; 'neg1'.
    | Neg2 val
    -- ^ Perform a two's complement (arithmetic) negation; 'neg2'.
    | Cvt SseType SseType val val
    -- ^ Convert integers into floats and vice versa
    | Div Signedness val val val
    -- ^ Signed or unsigned division; 'idiv'.
    | Cqo val val
    -- ^ Extend rax into rdx. Any other combination of operands is invalid.
    | Pxor val val
    -- ^ Packed xor
    | Movq val val
    -- ^ Move quadwords; 'movq'.
    | CmpSse SsePredicate SseType val val

-- | An interpretation of a 128-bit value's contents.
data SseType
    = PackedSingle
    -- ^ Four packed single-precision floats.
    | PackedDouble
    -- ^ Two packed double-precision floats.
    | ScalarSingle
    -- ^ One scalar single-precision float.
    | ScalarDouble
    -- ^ One scalar double-precision float.
    | SingleInteger
    -- ^ One scalar 32-bit integer.
    | PackedInteger
    -- ^ Four packed single-precision integers.

-- | A comparison predicate for SSE data.
data SsePredicate
    = SseEqual
    | SseLessThan
    | SseLessThanOrEqual
    | SseUnordered
    | SseNotEqual
    | SseNotLessThan
    | SseNotLessThanOrEqual
    | SseOrdered

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
data FlagCondition
    = Unconditionally
    -- ^ Jump unconditionally.
    | OnOverflow
    -- ^ Jump on overflow, @OF = 1@.
    | OnNoOverflow
    -- ^ Jump on no overflow, @OF = 0@.
    | OnSign
    -- ^ Jump on sign, @SF = 1@.
    | OnNoSign
    -- ^ Jump on no sign, @SF = 0@.
    | OnEqual
    -- ^ Jump on equal, @ZF = 1@.
    --
    -- Synonymous with @jz@, jump on zero.
    | OnNotEqual
    -- ^ Jump on not equal, @ZF = 0@.
    --
    -- Synonymous with @jnz@, jump on not zero.
    | OnBelow Signedness
    -- ^ If unsigned, then jump if /below/, @CF = 1@.
    --
    -- Synonymous with @jnae@, jump if not /above/ or equal, and @jc@ jump on
    -- carry.
    --
    -- If signed, then jump if /less/, @SF /= OF@.
    --
    -- Synonymous with @jnge@, jump if not /greater than/ or equal.
    | OnNotBelow Signedness
    -- ^ If unsigned, then jump if not /below/, @CF = 0@.
    --
    -- Synonymous with @jae@, jump if above or equal, and @jnc@ jump on no
    -- carry.
    --
    -- If signed, then jump if /greater than/ or equal.
    --
    -- Synonymous with @jnl@, jump if not /less/.
    | OnBelowOrEqual Signedness
    -- ^ If unsigned, then jump if /below/ or equal, @CF = 1@ or @ZF = 1@.
    --
    -- Synonymous with @jna@, jump if not /above/.
    --
    -- If signed, then jump if /less than/ or equal, @ZF = 1$ or @SF /= OF@.
    --
    -- Synonymous with @jng@, jump if not /greater/.
    | OnAbove Signedness
    -- ^ If unsigned, then jump if /above/, @CF = 0@ and @ZF = 0@.
    --
    -- Synonymous with @jnbe@, jump if not /below/ or equal.
    --
    -- If signed, then jump if /greater than/, @ZF = 0@ and @SF = OF@.
    --
    -- Synonymous with @jnle@, jump if not /less than/ or equal.
    | OnParityEven
    -- ^ Jump if parity even, @PF = 1@.
    | OnParityOdd
    -- ^ Jump if parity odd, @PF = 0@.
    | OnCounterZero
    -- ^ Jump on @CX@ (@ECX@) equal to zero.

-- | Inverses a flag condition.
--
-- Flag conditions with no intuitive inverse (e.g. 'OnCounterZero') are their
-- own inverse.
invertFlag :: FlagCondition -> FlagCondition
invertFlag c = case c of
    Unconditionally -> Unconditionally
    OnOverflow -> OnNoOverflow
    OnNoOverflow -> OnOverflow
    OnSign -> OnNoSign
    OnNoSign -> OnSign
    OnEqual -> OnNotEqual
    OnNotEqual -> OnEqual
    OnBelow sign -> OnNotBelow sign
    OnNotBelow sign -> OnBelow sign
    OnBelowOrEqual sign -> OnAbove sign
    OnAbove sign -> OnBelowOrEqual sign
    OnParityEven -> OnParityOdd
    OnParityOdd -> OnParityEven
    OnCounterZero -> OnCounterZero

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

instance Pretty IntegerRegister where
    pretty reg = text $ case reg of
        Rax -> "rax"
        Rbx -> "rbx"
        Rcx -> "rcx"
        Rdx -> "rdx"
        Rbp -> "rbp"
        Rsi -> "rsi"
        Rdi -> "rdi"
        Rsp -> "rsp"
        R8 -> "r8"
        R9 -> "r9"
        R10 -> "r10"
        R11 -> "r11"
        R12 -> "r12"
        R13 -> "r13"
        R14 -> "r14"
        R15 -> "r15"

-- | A concrete floating point register
data FloatingRegister
    = FloatingRegister Int
    deriving (Eq, Ord, Read, Show)

instance Pretty FloatingRegister where
    pretty f = case f of
        FloatingRegister n -> text "xmm" <> P.int n

-- | A concrete register.
data HardwareRegister
    = IntegerHwRegister IntegerRegister
    | FloatingHwRegister FloatingRegister
    deriving (Eq, Ord, Read, Show)

instance Pretty HardwareRegister where
    pretty hw = case hw of
        IntegerHwRegister ir -> pretty ir
        FloatingHwRegister fr -> pretty fr

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

instance StorageSize RegisterSize where
    storageSize p = case p of
        Low8 -> 1
        High8 -> 1
        Extended16 -> 2
        Extended32 -> 4
        Extended64 -> 8

data RegisterAccessMode
    = IntegerMode
    | FloatingMode
    deriving (Eq, Ord, Read, Show)

-- | A register together with a size modifier.
data SizedRegister reg = SizedRegister RegisterSize reg
    deriving (Eq, Ord, Read, Show)

-- | An operand to an instruction.
data Operand reg label
    = Immediate Immediate
    -- ^ A constant value.
    | Register (Directness reg)
    | Label label
    -- ^ A internal label to a location within the current function.
    | Internal (Directness String)
    -- ^ An internal reference, e.g. to another compiled Go function.
    | External (Directness String)
    -- ^ An external reference, e.g. to a C function.
    deriving (Eq, Ord, Read, Show)

-- | Whether access to a memory location is direct or indirect.
data Directness a
    = Direct a
    -- ^ Direct access to data.
    | Indirect (Offset a)
    -- ^ Indirect access to data, with an offset.
    deriving (Eq, Functor, Ord, Read, Show)

data Offset reg
    = Offset Displacement reg
    -- ^ A register indirection with a displacement.
    --
    -- More space-efficient code can be generated if the displacement is small.
    deriving (Eq, Functor, Ord, Read, Show)

-- | A multiplier on on an index.
data Scale
    = ScaleByte
    | ScaleShort
    | ScaleWord
    | ScaleDword
    deriving (Eq, Ord, Read, Show)

type Displacement = Int64

data Immediate
    = ImmF Double
    | ImmI Word64
    deriving (Eq, Ord, Read, Show)

-- | Nullary instruction.
type Instr0 reg label m a = AsmT reg label m a

-- | Unary instruction.
type Instr1 reg label m a = Operand reg label -> Instr0 reg label m a

-- | Binary instruction.
type Instr2 reg label m a = Operand reg label -> Instr1 reg label m a

-- | Ternary instruction.
type Instr3 reg label m a = Operand reg label -> Instr2 reg label m a

prologue :: Monad m => ScratchFlow -> AsmT reg label m ()
prologue flow = liftF . Prologue flow $ ()

withPrologue :: Monad m => AsmT reg label m a -> AsmT reg label m a
withPrologue m = do
    prologue Save
    x <- m
    prologue Load
    pure x

scratch :: Monad m => ScratchFlow -> AsmT reg label m ()
scratch flow = liftF . Scratch flow $ ()

withScratch :: Monad m => AsmT reg label m a -> AsmT reg label m a
withScratch m = do
    scratch Save
    x <- m
    scratch Load
    pure x

-- | 'NewLabel'
newLabel :: Monad m => AsmT reg label m label
newLabel = liftF . NewLabel $ id

-- | 'SetLabel'
setLabel :: Monad m => label -> AsmT reg label m ()
setLabel l = liftF . SetLabel l $ ()

-- | Defines a new label and immediately sets it to the value obtained from
-- 'here'.
newLabelHere :: Monad m => AsmT reg label m label
newLabelHere = do
    l <- newLabel
    setLabel l
    pure l

-- | 'Emit' 'Ret'
ret :: Monad m => Instr0 reg label m ()
ret = liftF . Emit Ret $ ()

call :: Monad m => Instr1 reg label m ()
call f = liftF . Emit (Call f) $ ()

-- | 'Emit' 'Mov'
mov :: Monad m => Instr2 reg label m ()
mov x y = liftF . Emit (Mov x y) $ ()

-- | 'Emit' 'Add'
add :: Monad m => Instr2 reg label m ()
add x y = liftF . Emit (Add x y) $ ()

-- | 'Emit' 'Sub'
sub :: Monad m => Instr2 reg label m ()
sub x y = liftF . Emit (Sub x y) $ ()

-- | 'Emit' 'Mul'
imul :: Monad m =>
       Operand reg label
    -> Operand reg label
    -> AsmT reg label m ()
imul x y = liftF . Emit (Mul Signed x y) $ ()

-- | 'Emit' 'Xor'
xor :: Monad m => Instr2 reg label m ()
xor x y = liftF . Emit (Xor x y) $ ()

-- | 'Emit' 'Inc'
inc :: Monad m => Instr1 reg label m ()
inc v = liftF . Emit (Inc v) $ ()

-- | 'Emit' 'Dec'
dec :: Monad m => Instr1 reg label m ()
dec v = liftF . Emit (Dec v) $ ()

-- | 'Emit' 'Push'
push :: Monad m => Instr1 reg label m ()
push v = liftF . Emit (Push v) $ ()

-- | 'Emit' 'Pop'
pop :: Monad m => Instr1 reg label m ()
pop v = liftF . Emit (Pop v) $ ()

-- | 'Emit' 'Nop'
nop :: Monad m => Instr0 reg label m ()
nop = liftF . Emit Nop $ ()

-- | 'Emit' 'Syscall'
syscall :: Monad m => Instr0 reg label m ()
syscall = liftF . Emit Syscall $ ()

-- | 'Emit' 'Int'
int :: Monad m => Instr1 reg label m ()
int v = liftF . Emit (Int v) $ ()

-- | 'Emit' 'Cmp'
cmp :: Monad m => Instr2 reg label m ()
cmp x y = liftF . Emit (Cmp x y) $ ()

-- | 'Emit' 'Test'
test :: Monad m => Instr2 reg label m ()
test x y = liftF . Emit (Test x y) $ ()

-- | 'Emit' 'Sal'
sal :: Monad m => Instr2 reg label m ()
sal x y = liftF . Emit (Sal x y) $ ()

-- | 'Emit' 'Sar'
sar :: Monad m => Instr2 reg label m ()
sar x y = liftF . Emit (Sar x y) $ ()

-- | 'Emit 'Jump'
jump :: Monad m => FlagCondition -> Instr1 reg label m ()
jump v x = liftF . Emit (Jump v x) $ ()

-- | 'Emit' 'Setc'
setc :: Monad m => FlagCondition -> Instr1 reg label m ()
setc v x = liftF . Emit (Setc v x) $ ()

-- | 'Emit' 'Neg1'
neg1 :: Monad m => Instr1 reg label m ()
neg1 v = liftF . Emit (Neg1 v) $ ()

-- | 'Emit' 'Neg2'
neg2 :: Monad m => Instr1 reg label m ()
neg2 v = liftF . Emit (Neg2 v) $ ()

-- | 'Emit' 'Cvt'
cvt :: Monad m => SseType -> SseType -> Instr2 reg label m ()
cvt s1 s2 x y = liftF . Emit (Cvt s1 s2 x y) $ ()

-- | 'Emit' 'Div'
idiv :: Monad m => Instr3 reg label m ()
idiv x y z = liftF . Emit (Div Signed x y z) $ ()

-- | 'Emit' 'Cqo'
cqo :: Monad m => Instr2 reg label m ()
cqo x y = liftF . Emit (Cqo x y) $ ()

-- | 'Emit' 'And'
bwand :: Monad m => Instr2 reg label m ()
bwand x y = liftF . Emit (And x y) $ ()

-- | 'Emit' 'Or'
bwor :: Monad m => Instr2 reg label m ()
bwor x y = liftF . Emit (Or x y) $ ()

-- | 'Emit' 'AddSse'
addsse :: Monad m => SseType -> Instr2 reg label m ()
addsse s x y = liftF . Emit (AddSse s x y) $ ()

-- | 'Emit' 'SubSse'
subsse :: Monad m => SseType -> Instr2 reg label m ()
subsse s x y = liftF . Emit (SubSse s x y) $ ()

-- | 'Emit' 'MulSse'
mulsse :: Monad m => SseType -> Instr2 reg label m ()
mulsse s x y = liftF . Emit (MulSse s x y) $ ()

-- | 'Emit' 'DivSse'
divsse :: Monad m => SseType -> Instr2 reg label m ()
divsse s x y = liftF . Emit (DivSse s x y) $ ()

-- | 'Emit' 'Pxor'
pxor :: Monad m => Instr2 reg label m ()
pxor x y = liftF . Emit (Pxor x y) $ ()

-- | 'Emit' 'Movq'
movq :: Monad m => Instr2 reg label m ()
movq x y = liftF . Emit (Movq x y) $ ()

cmpsse :: Monad m => SsePredicate -> SseType -> Instr2 reg label m ()
cmpsse s t x y = liftF . Emit (CmpSse s t x y) $ ()

-- Pretty printers --

instance Pretty reg => Pretty (AsmT reg Int Identity ()) where
    pretty = flip evalState initial . iter f . ($> pure empty) where
        initial :: Int
        initial = 0

        nextNum :: State Int Int
        nextNum = do
            n <- get
            put $ n + 1
            pure n

        f :: AsmF Int (Operand reg Int) (State Int Doc) -> (State Int Doc)
        f a = case a of
            Emit instr next -> ($+$) <$> pure (prettyInstr instr) <*> next
            NewLabel lf -> lf =<< nextNum
            SetLabel i m -> ($+$) <$> pure (text "l" <> P.int i <> text ":") <*> m
            Scratch _ m -> m
            Prologue _ m -> m

        prettySseCmp :: SsePredicate -> SseType -> Doc
        prettySseCmp p t = text "cmp" <> pretty p <> pretty t

        opretty :: Pretty reg => Doc -> Operand reg Int -> Doc
        opretty size op = case op of
            Immediate imm -> case imm of
                ImmI i -> P.int (fromIntegral i)
                ImmF i -> P.double i
            Register d -> case pretty <$> d of
                Direct reg -> pretty reg
                Indirect off -> size <+> prettyBrackets True (offsetp off)
            Label i -> text "l" <> P.int i
            Internal d -> case text <$> d of
                Direct s -> size <+> s
                Indirect off -> size <+> prettyBrackets True (offsetp off)
            External d -> case text <$> d of
                Direct s -> s
                Indirect off -> size <+> prettyBrackets True (offsetp off)

        offsetp off = case off of
            Offset d r -> r <+> if d >= 0
                then text "+" <+> P.int (fromIntegral d)
                else text "-" <+> P.int (negate $ fromIntegral d)

        opretty2 s1 o1 s2 o2 = opretty s1 o1 <> text "," <+> opretty s2 o2

        mnep :: Pretty reg => Doc -> Doc -> Operand reg Int -> Doc
        mnep t s o = t <+> opretty s o

        mnep2
            :: Pretty reg
            => Doc
            -> Doc -> Operand reg Int
            -> Doc -> Operand reg Int
            -> Doc
        mnep2 t s1 o1 s2 o2 = t <+> opretty2 s1 o1 s2 o2

        prettyCond :: FlagCondition -> Doc
        prettyCond cond = text $ case cond of
            Unconditionally -> error "prettyCond: unconditionally unsupported"
            OnOverflow -> "o"
            OnNoOverflow -> "no"
            OnSign -> "s"
            OnNoSign -> "ns"
            OnEqual -> "e"
            OnNotEqual -> "ne"
            OnBelow sign -> case sign of
                Signed -> "l"
                Unsigned -> "b"
            OnNotBelow sign -> case sign of
                Signed -> "nl"
                Unsigned -> "nb"
            OnBelowOrEqual sign -> case sign of
                Signed -> "le"
                Unsigned -> "be"
            OnAbove sign -> case sign of
                Signed -> "g"
                Unsigned -> "a"
            OnParityEven -> "pe"
            OnParityOdd -> "po"
            OnCounterZero -> "cxz"

        qword = text "QWORD"

        prettyInstr :: Instruction (Operand reg Int) -> Doc
        prettyInstr instr = case instr of
            Ret -> text "ret"
            Call o -> mnep (text "call") empty o
            And o1 o2 -> mnep2 (text "and") qword o1 qword o2
            Or o1 o2 -> mnep2 (text "or") qword o1 qword o2
            Mov o1 o2 -> mnep2 (text "mov") qword o1 qword o2
            Movq o1 o2 -> mnep2 (text "movq") qword o1 qword o2
            Add o1 o2 -> mnep2 (text "add") qword o1 qword o2
            AddSse st o1 o2 -> mnep2 (text "add" <> pretty st) qword o1 qword o2
            Sub o1 o2 -> mnep2 (text "sub") qword o1 qword o2
            SubSse st o1 o2 -> mnep2 (text "sub" <> pretty st) qword o1 qword o2
            Mul sign o1 o2 ->
                let mnemonic = case sign of Signed -> "imul" ; Unsigned -> "mul" in
                mnep2 (text mnemonic) qword o1 qword o2
            MulSse st o1 o2 -> mnep2 (text "mul" <> pretty st) qword o1 qword o2
            Div sign _ _ o -> (<+> opretty qword o) . text $ case sign of
                Signed -> "idiv"
                Unsigned -> "div"
            DivSse st o1 o2 -> mnep2 (text "div" <> pretty st) qword o1 qword o2
            Xor o1 o2 -> mnep2 (text "xor") qword o1 qword o2
            Inc o -> mnep (text "inc") qword o
            Dec o -> mnep (text "dec") qword o
            Push o -> mnep (text "push") qword o
            Pop o -> mnep (text "pop") qword o
            Nop -> text "nop"
            Syscall -> text "syscall"
            Int o -> mnep (text "int") qword o
            Cmp o1 o2 -> mnep2 (text "cmp") qword o1 qword o2
            CmpSse p s o1 o2 -> mnep2 (prettySseCmp p s) qword o1 qword o2
            Test o1 o2 -> mnep2 (text "test") qword o1 qword o2
            Sal o1 o2 -> mnep2 (text "sal") qword o1 qword o2
            Sar o1 o2 -> mnep2 (text "sar") qword o1 qword o2
            Jump cond o -> (<+> opretty empty o) $ case cond of
                Unconditionally -> text "jmp"
                _ -> text "j" <> prettyCond cond
            Setc cond o -> (<+> opretty empty o) $ case cond of
                Unconditionally -> error "can't set byte unconditionally"
                _ -> text "set" <> prettyCond cond
            Neg1 o -> mnep (text "not") qword o
            Neg2 o -> mnep (text "neg") qword o
            Cvt t1 t2 o1 o2 ->
                text "cvt" <> pretty t1 <> pretty t2 <+> opretty2 qword o1 qword o2
            Cqo _ _ -> text "cqo"
            Pxor o1 o2 -> mnep2 (text "pxor") qword o1 qword o2

instance Pretty SseType where
    pretty t = text $ case t of
        PackedSingle -> "ps"
        PackedDouble -> "pd"
        ScalarSingle -> "ss"
        ScalarDouble -> "sd"
        SingleInteger -> "si"
        PackedInteger -> "pi"

instance Pretty SsePredicate where
    pretty p = text $ case p of
        SseEqual -> "eq"
        SseLessThan -> "lt"
        SseLessThanOrEqual -> "le"
        SseUnordered -> "unord"
        SseNotEqual -> "neq"
        SseNotLessThan -> "nlt"
        SseNotLessThanOrEqual -> "nle"
        SseOrdered -> "ord"

instance Pretty (SizedRegister HardwareRegister) where
    pretty (SizedRegister size reg) = text $ case size of
        Low8 -> case reg of
            IntegerHwRegister r -> case r of
                Rax -> "al"
                Rbx -> "bl"
                Rcx -> "cl"
                Rdx -> "dl"
                Rbp -> error "rbp has no 8-bit form"
                Rsi -> error "rsi has no 8-bit form"
                Rdi -> error "Rdi has no 8-bit form"
                Rsp -> error "Rsp has no 8-bit form"
                R8 -> error "R8 has no 8-bit form"
                R9 -> error "R9 has no 8-bit form"
                R10 -> error "R10 has no 8-bit form"
                R11 -> error "R11 has no 8-bit form"
                R12 -> error "R12 has no 8-bit form"
                R13 -> error "R13 has no 8-bit form"
                R14 -> error "R14 has no 8-bit form"
                R15 -> error "R15 has no 8-bit form"
            FloatingHwRegister r -> error (show r ++ " has no 8-bit form")
        High8 -> case reg of
            IntegerHwRegister r -> case r of
                Rax -> "ah"
                Rbx -> "bh"
                Rcx -> "ch"
                Rdx -> "dh"
                Rbp -> error "rbp has no 8-bit form"
                Rsi -> error "rsi has no 8-bit form"
                Rdi -> error "Rdi has no 8-bit form"
                Rsp -> error "Rsp has no 8-bit form"
                R8 -> error "R8 has no 8-bit form"
                R9 -> error "R9 has no 8-bit form"
                R10 -> error "R10 has no 8-bit form"
                R11 -> error "R11 has no 8-bit form"
                R12 -> error "R12 has no 8-bit form"
                R13 -> error "R13 has no 8-bit form"
                R14 -> error "R14 has no 8-bit form"
                R15 -> error "R15 has no 8-bit form"
            FloatingHwRegister r -> error (show r ++ " has no 8-bit form")
        Extended16 -> case reg of
            IntegerHwRegister r -> case r of
                Rax -> "ax"
                Rbx -> "bx"
                Rcx -> "cx"
                Rdx -> "dx"
                Rbp -> "bp"
                Rsi -> "si"
                Rdi -> "di"
                Rsp -> "sp"
                R8 -> error "R8 has no 16-bit form"
                R9 -> error "R9 has no 16-bit form"
                R10 -> error "R10 has no 16-bit form"
                R11 -> error "R11 has no 16-bit form"
                R12 -> error "R12 has no 16-bit form"
                R13 -> error "R13 has no 16-bit form"
                R14 -> error "R14 has no 16-bit form"
                R15 -> error "R15 has no 16-bit form"
            FloatingHwRegister r -> error (show r ++ " has no 16-bit form")
        Extended32 -> case reg of
            IntegerHwRegister r -> case r of
                Rax -> "eax"
                Rbx -> "ebx"
                Rcx -> "ecx"
                Rdx -> "edx"
                Rbp -> "ebp"
                Rsi -> "esi"
                Rdi -> "edi"
                Rsp -> "esp"
                R8 -> error "R8 has no 32-bit form"
                R9 -> error "R9 has no 32-bit form"
                R10 -> error "R10 has no 32-bit form"
                R11 -> error "R11 has no 32-bit form"
                R12 -> error "R12 has no 32-bit form"
                R13 -> error "R13 has no 32-bit form"
                R14 -> error "R14 has no 32-bit form"
                R15 -> error "R15 has no 32-bit form"
            FloatingHwRegister (FloatingRegister n) -> "xmm" ++ show n
        Extended64 -> case reg of
            IntegerHwRegister r -> case r of
                Rax -> "rax"
                Rbx -> "rbx"
                Rcx -> "rcx"
                Rdx -> "rdx"
                Rbp -> "rbp"
                Rsi -> "rsi"
                Rdi -> "rdi"
                Rsp -> "rsp"
                R8 -> "r8"
                R9 -> "r9"
                R10 -> "r10"
                R11 -> "r11"
                R12 -> "r12"
                R13 -> "r13"
                R14 -> "r14"
                R15 -> "r15"
            FloatingHwRegister (FloatingRegister n) -> "xmm" ++ show n
