module Day23 (main) where

import Data.Function ((&))
import Data.List (intercalate)
import Data.Map (Map)
import qualified Data.Map as Map
import Helpers (debug)

data Register
  = A
  | B
  deriving (Show)

data Instruction
  = HLF Register
  | TPL Register
  | INC Register
  | JMP Int
  | JIE Register Int
  | JIO Register Int
  deriving (Show)

data CPU = CPU
  { regA :: Int,
    regB :: Int,
    pc :: Int,
    memory :: Map Int Instruction
  }

instance Show CPU where
  show cpu@CPU {..} =
    [ "A: " ++ show regA,
      "B: " ++ show regB,
      "PC: " ++ show pc
    ]
      & intercalate ", "

step :: CPU -> Maybe CPU
step cpu@CPU {..} = do
  instruction <- Map.lookup pc memory
  -- let !_ = debug "instruction" instruction
  let result =
        case instruction of
          HLF reg -> hlf reg cpu
          TPL reg -> tpl reg cpu
          INC reg -> inc reg cpu
          JMP offset -> jmp offset cpu
          JIE reg offset -> jie reg offset cpu
          JIO reg offset -> jio reg offset cpu
  return $ {-debug "result"-} result

onReg :: Register -> (Int -> Int) -> CPU -> CPU
onReg reg fn cpu@CPU {..} =
  case reg of
    A -> cpu {regA = fn regA}
    B -> cpu {regB = fn regB}

ifReg :: Register -> (Int -> Bool) -> (CPU -> CPU) -> (CPU -> CPU) -> CPU -> CPU
ifReg reg pred fnTrue fnFalse cpu@CPU {..} =
  case reg of
    A -> if pred regA then fnTrue cpu else fnFalse cpu
    B -> if pred regB then fnTrue cpu else fnFalse cpu

hlf reg cpu = cpu & onReg reg (`div` 2) & jmp 1
tpl reg cpu = cpu & onReg reg (* 3) & jmp 1
inc reg cpu = cpu & onReg reg (+ 1) & jmp 1
jmp offset cpu@CPU {..} = cpu {pc = pc + offset}
jie reg offset = ifReg reg even (jmp offset) (jmp 1)
jio reg offset = ifReg reg (== 1) (jmp offset) (jmp 1)

run :: CPU -> CPU
run = iterate' step

iterate' :: (a -> Maybe a) -> a -> a
iterate' fn x =
  case fn x of
    Nothing -> x
    Just x' -> iterate' fn x'

main :: IO ()
main =
  let initCpu = fromInput input
      -- !_ = debug "init" initCpu
      finalCpu = run initCpu
   in putStrLn $ "reg B = " ++ show (regB finalCpu)

fromInput :: [Instruction] -> CPU
fromInput instructions =
  --CPU 0 0 0 (Map.fromList (zip [0 ..] instructions))
  CPU 1 0 0 (Map.fromList (zip [0 ..] instructions))

input :: [Instruction]
input =
  [JIO A 16, INC A, INC A, TPL A, TPL A, TPL A, INC A, INC A, TPL A, INC A, INC A, TPL A, TPL A, TPL A, INC A, JMP 23, TPL A, INC A, INC A, TPL A, INC A, INC A, TPL A, TPL A, INC A, INC A, TPL A, INC A, TPL A, INC A, TPL A, INC A, INC A, TPL A, INC A, TPL A, TPL A, INC A, JIO A 8, INC B, JIE A 4, TPL A, INC A, JMP 2, HLF A, JMP (-7)]
