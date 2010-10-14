module Stump where

import Data.Word
import Data.Int
import Data.Bits hiding (shift)
import qualified Data.Array as A
import qualified Data.IntMap as I


-- The full processor state.
data State = State { regs  :: A.Array Word8 Word16
                   , mem   :: I.IntMap Word16
                   , flags :: Flags
                   } deriving (Read, Show)

empty_state = State (A.listArray (0, 7) (repeat 0)) I.empty $ Flags False False False False


-- The condition codes/flags.
data Flags = Flags { f_n :: Bool
                   , f_z :: Bool
                   , f_v :: Bool
                   , f_c :: Bool
                   } deriving (Read, Show, Eq)

no_flags = Flags False False False False


-- An instruction.
                   -- [op] [result] [reg_a] [reg_b] [shift]
data Instruction = Std { instr_type :: InstrType
                       , ld_cc      :: Bool
                       , dst_reg    :: Word8
                       , src_a_reg  :: Word8
                       , src_b_reg  :: Word8
                       , shift      :: Word8
                       }
                   -- [op] [result] [reg_a] [immediate]
                 | Imm { instr_type :: InstrType
                       , ld_cc      :: Bool
                       , dst_reg    :: Word8
                       , src_a_reg  :: Word8
                       , immediate  :: Int8
                       }
                   -- [condition] [label]
                 | Branch { condition :: Word8
                          , offset    :: Int8
                          -- KLUDGE: This is always false.
                          , ld_cc     :: Bool
                          }
                 deriving (Read, Show)

-- Types of instructions.
data InstrType = Add | Adc | Sub | Sbc | And | Or | LdSt | Bcc deriving (Read, Show)



----------------------
-- Instruction Parsing
----------------------

parse_instr :: Word16 -> Instruction
parse_instr instr = case (type_bit, i_type) of
		(False, _) -> Std { instr_type = i_type
		                  , ld_cc      = testBit instr 11
		                  , dst_reg    = fromIntegral $ get_bits 8 11 instr
		                  , src_a_reg  = fromIntegral $ get_bits 5 8 instr
		                  , src_b_reg  = fromIntegral $ get_bits 2 5 instr
		                  , shift      = fromIntegral $ get_bits 0 2 instr
		                  }
		(True, Bcc) -> Branch { condition = fromIntegral $ get_bits 8 12 instr
		                      , offset    = fromIntegral $ get_bits 0 8 instr
		                      , ld_cc     = False
		                      }
		(True, _) -> Imm { instr_type = i_type
		                 , ld_cc      = testBit instr 11
		                 , dst_reg    = fromIntegral $ get_bits 8 11 instr
		                 , src_a_reg  = fromIntegral $ get_bits 5 8 instr
		                 , immediate  = sign_extend $ get_bits 0 5 instr
		                 }
	where type_bit = testBit instr 12
	      i_type   = parse_instr_type $ get_bits 13 16 instr

parse_instr_type x = case x of 
	0 -> Add
	1 -> Adc
	2 -> Sub
	3 -> Sbc
	4 -> And
	5 -> Or
	6 -> LdSt
	7 -> Bcc



------------------------
-- Instruction Execution
------------------------

-- Fetch the instruction, increment the program counter, decode and execute.
step :: State -> State
step state = exec state_inc_pc instr
	where pc = get_reg state 7
	      instr = parse_instr $ fromIntegral $ get_mem state $ fromIntegral pc 
	      state_inc_pc = set_reg state 7 $ pc + 1


exec :: State -> Instruction -> State
exec state instr = 
	case instr of
		-- Standard instructions.
		Std {} -> writeback state instr (value, _flags)
			where 
				-- Execute the operation.
				(value, _flags) = exec_oper state shift_flags instr
				                            shift_value
				                            (get_reg state (src_b_reg instr))
				-- Shift src_a.
				(shift_value, shift_flags) = alu_shift (flags state) 
				                                       (get_reg state (src_a_reg instr)) 
				                                       (shift instr)
		
		-- Immediate instructions.
		Imm {} -> writeback state instr (value, _flags)
			where (value, _flags) = exec_oper state no_flags
				instr (get_reg state (src_a_reg instr)) 
				(fromIntegral $ immediate instr)
		
		-- Branch instructions. Only write back if should_branch says so.
		Branch { condition = c } -> if should_branch (flags state) c
			then writeback state instr ( get_reg state 7 + (fromIntegral $ offset instr)
			                           , no_flags)
			else state


writeback :: State -> Instruction -> (Word16, Flags) -> State
writeback state instr (value, _flags) = 
	case instr of 
		-- Standard store.
		Std { instr_type = LdSt, ld_cc = True, src_a_reg = a, src_b_reg = b } ->
			_mem (fromIntegral $ (get_reg state a) + (get_reg state b))
		-- Other standard instructions.
		Std { dst_reg = d } ->
			_reg d
		-- Immediate store.
		Imm { instr_type = LdSt, ld_cc = True, src_a_reg = a, immediate = i } ->
			_mem (fromIntegral (fromIntegral (get_reg state a) + i))
		-- Other immediate instructions.
		Imm { dst_reg = d } ->
			_reg d
		-- Branch instructions store to the pc.
		Branch {} ->
			_reg 7
	where
		-- Write to a memory location.
		_mem dest = set_mem state dest value
		-- Write to a register.
		_reg dest = (set_reg state dest value) { flags = if ld_cc instr 
		                                                 then _flags 
		                                                 else flags state }


exec_oper :: State -> Flags -> Instruction -> Word16 -> Word16 -> (Word16, Flags)
exec_oper s@(State {flags=f}) shift_flags i a b = 
	case instr_type i of
		Add -> exec_add_oper a b False False 
		Adc -> exec_add_oper a b (f_c f) False
		Sub -> exec_add_oper a (complement b) True True
		Sbc -> exec_add_oper a (complement b) (not $ f_c f) True
		And -> norm_flags (a .&. b) False (f_c shift_flags)
		Or  -> norm_flags (a .|. b) False (f_c shift_flags)
		LdSt -> case ld_cc i of
			-- store
			True  -> (get_reg s $ dst_reg i, no_flags)
			-- load
			False -> (get_mem s $ fromIntegral $ a + b, no_flags)
	where 
		-- The normal zero and negative flags for a result.
		norm_flags result v c = (result, Flags (testBit result 15) (result == 0) v c)


alu_shift :: Flags -> Word16 -> Word8 -> (Word16, Flags)
alu_shift flags value bits = 
	case bits of
		-- No shift.
		0 -> (value, no_flags)
		-- ASR
		1 -> ( (shiftR value 1) .|. (value .&. bit 15)
		     , no_flags { f_c = testBit value 0 })
		-- ROR
		2 -> ( rotateR value 1
		     , no_flags { f_c = testBit value 0 })
		-- RRC - Rotate through carry.
		3 -> ( ((if f_c flags then setBit else clearBit) rotated 15)
		     , no_flags { f_c = testBit value 0 } )
			where rotated = rotateR value 1


should_branch :: Flags -> Word8 -> Bool
should_branch flags@(Flags{ f_n = n, f_z = z, f_v = v, f_c = c }) cond =
	case cond of
		-- BAL, BNV
		1  -> False
		-- BHI, BLS
		3  -> c || z
		-- BCC, BCS
		5  -> c
		-- BNE, BEQ
		7  -> z
		-- BVC, BVS
		9  -> v
		-- BPL, BMI
		11 -> n
		-- BGE, BLT
		13 -> ((not n) && v) || (n && (not v))
		-- BGT, BLE
		15 -> (((not n) && v) || (n && (not v))) || z
		-- Flags are alternately complemented.
		_  -> not $ should_branch flags $ setBit cond 0


exec_add_oper :: Word16 -> Word16 -> Bool -> Bool -> (Word16, Flags)
exec_add_oper _a _b carry_in invert_carry = 
	(fromIntegral $ (result .&. 0xffff), Flags (testBit result 15) (result == 0)
	                (((a<0) && (b<0) && (result>=0)) || ((a>=0) && (b>=0) && (result<0)))
	                c)
	where 
		a = sign_extend32 $ fromIntegral _a :: Int32
		b = sign_extend32 $ fromIntegral _b :: Int32
		int_carry = (if carry_in then 1 else 0) :: Int32
		result = sign_extend32 $ fromIntegral $ 
		         (a + b + (fromIntegral int_carry)) .&. 0xffff :: Int32
		c = ((if invert_carry then not else id)((result .&. 0xFFFF)<(a .&. 0xFFFF)))


-- Utilities gor getting/setting memory and state.
get_mem state loc = I.findWithDefault 0 loc $ mem state
set_mem state loc value = state{ mem = I.adjust (\x -> value) loc $ mem state }

-- r0 is always 0 - make doubly sure!
get_reg state num = if num == 0 then 0 else (regs state) A.! num
set_reg state num value = if num == 0 
                          then state 
                          else state{ regs = regs state A.// [(num, value)] }



------------------------------------------
-- Some utilities for working with binary.
------------------------------------------
bin_to_dec :: String -> Word16
bin_to_dec = (sum . zipWith (\y x -> if x == '0' then 0 else (2 ^ y)) [0..] . reverse)


get_bits :: Int -> Int -> Word16 -> Word16
get_bits first last x = fromIntegral $ shiftR x first .&. (2 ^ (last - first) - 1) 


-- Sign extend from 4 to 8 bits.
-- This is usually used with get_bits, so the type makes sense.
sign_extend :: Word16 -> Int8
sign_extend x = if testBit x 4 
                then (fromIntegral 224) .|. (fromIntegral x) 
                else fromIntegral x


-- Sign extend from 16 to 32 bits.
sign_extend32 :: Word16 -> Int32
sign_extend32 x = if testBit x 15
                  then (fromIntegral 0xffff0000) .|. (fromIntegral x) 
                  else fromIntegral x          -- ^ Penis operator.


-- Read some memory from a file.
read_mem f_name = (readFile f_name)
                  >>= (return . map (fromIntegral . bin_to_dec) . lines)


-- Create a new state from a bunch of memory.
state_from_mem _mem = empty_state { mem = I.fromList $ zip [0..] _mem }

