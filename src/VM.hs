module VM
    ( VMState(..)
    , OPCode(..)
    , initialState
    , step
    ) where

data VMState = VMState
    { pc :: Int          -- Program Counter
    , sp :: Int          -- Stack Pointer
    , stack :: [Int]     -- Stack
    , memory :: [Int]    -- Memory
    } deriving (Show, Eq)

data OPCode = NOP
            | PUSH Int
            | ADD
            | SUB
            | LOAD
            | STORE
            | JMP
            | JZ
            | HALT
            deriving (Show)


initialState :: VMState
initialState = VMState
    { pc = 0
    , sp = -1
    , stack = []
    , memory = replicate 256 0
    }

step :: VMState -> OPCode -> VMState
step state NOP = state { pc = pc state + 1 }
step state (PUSH val) =
    state { pc = pc state + 1
          , sp = sp state + 1
          , stack = val : stack state
          }
step state ADD =
    case stack state of
        (x:y:rest) ->
            state { pc = pc state + 1
                  , sp = sp state - 1
                  , stack = (y + x) : rest
                  }
        _ -> error "Stack underflow on ADD"

