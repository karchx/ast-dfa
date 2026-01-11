module VM
    (
        OPCode(..),
        initalVM,
        run
    ) where

import qualified Data.Map as M

data OPCode = NOP
            | PUSH Double
            | LOAD String
            | ADD
            | SUB
            | MUL
            | DIV
            | HALT
            deriving (Show, Eq)

data VMStatus
    = Running
    | Halted
    | Fault String
    deriving (Show, Eq)

data VMState = VMState
    { pc :: Int          -- Program Counter
    , stack :: [Double]     -- Stack
    , env :: M.Map String Double
    , program :: [OPCode]
    , status :: VMStatus
    } deriving Show


initalVM :: [OPCode] -> M.Map String Double -> VMState
initalVM prog env0 = VMState
    { pc = 0
    , stack = []
    , env = env0
    , program = prog
    , status = Running
    }

fetch :: VMState -> Either VMState OPCode
fetch vm
    | pc vm < 0 || pc vm >= length (program vm) =
      Left vm { status = Fault "PC out of bounds" }
    | otherwise = Right (program vm !! pc vm)

execute :: OPCode -> VMState -> VMState
execute NOP vm = vm { pc = pc vm + 1 }

execute (PUSH n) vm =
    vm { pc = pc vm + 1
       , stack = n : stack vm
       }

execute (LOAD x) vm =
    case M.lookup x (env vm) of
        Just v -> vm { pc = pc vm + 1, stack = v : stack vm }
        Nothing -> vm { status = Fault ("Unbound variable: " ++ x) }

execute ADD vm = 
    case stack vm of
        (x:y:xs) -> vm { pc = pc vm + 1, stack = (y + x) : xs }
        _        -> vm { status = Fault "Stack underflow on ADD" }

execute SUB vm =
   case stack vm of
        (x:y:xs) -> vm { pc = pc vm + 1, stack = (y - x) : xs }
        _        -> vm { status = Fault "Stack underflow on SUB" }

execute MUL vm =
    case stack vm of
        (x:y:xs) -> vm { pc = pc vm + 1, stack = (y * x) : xs }
        _        -> vm { status = Fault "Stack underflow on MUL" }

execute DIV vm =
    case stack vm of
        0:_ -> vm { status = Fault "Division by zero" }
        (x:y:xs) -> vm { pc = pc vm + 1, stack = (y / x) : xs }
        _        -> vm { status = Fault "Stack underflow on DIV" }

execute HALT vm = vm { status = Halted }

step :: VMState -> VMState
step vm
    | status vm /= Running = vm { status = Fault "Step on non-running VM" }
    | otherwise =
        case fetch vm of
            Left vm' -> vm'
            Right opcode   -> execute opcode vm

run :: VMState -> VMState
run vm = 
    case status vm of
        Running -> run (step vm)
        _       -> vm
