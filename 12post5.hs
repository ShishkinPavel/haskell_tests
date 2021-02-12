import Data.Maybe ( fromMaybe )

-- v v v -- Do not change these types -- v v v --

data Memory a = Memory Integer [a] [a] deriving (Show, Eq)

type Value = Integer
data Output = S String | N Value deriving (Show, Eq)
data AddressType = Relative | Absolute deriving (Show, Eq)

data Condition = Neg | Zero | Pos deriving (Show, Eq)

data Instruction = Move RegisterName RegisterName
                 | Assign RegisterName Value
                 | Add RegisterName RegisterName
                 | Negate RegisterName

                 | Load RegisterName
                 | Store RegisterName
                 | Focus AddressType RegisterName

                 | Jump AddressType Integer
                 | JumpIf Condition RegisterName AddressType Integer

                 | Out RegisterName
                 | Trace String

                 | Halt
                 deriving (Show, Eq)


data RegisterName = R1 | R2 | R3 deriving (Show, Eq)
data Registers = Registers Value Value Value deriving (Show, Eq)

type Program = Memory Instruction
type Data = Memory Value

-- ^ ^ ^ -- Do not change these types -- ^ ^ ^ --

-- ------------------------------------------------------------------------- --
--                               YOUR SOLUTION                               --
-- ------------------------------------------------------------------------- --

getRegister :: Registers -> RegisterName -> Value
getRegister (Registers a _ _) R1 = a
getRegister (Registers _ b _) R2 = b
getRegister (Registers _ _ c) R3 = c


setRegister :: Registers -> RegisterName -> Value -> Registers
setRegister (Registers _ b c) R1 x = Registers x b c
setRegister (Registers a _ c) R2 x = Registers a x c
setRegister (Registers a b _) R3 x = Registers a b x

fromList :: [a] -> Memory a
fromList [] = Memory 0 [] []
fromList xs = Memory 0 [] xs

fromListInf :: a -> [a] -> Memory a
fromListInf x ys = Memory 0 xs $ (++) ys xs where xs = x : xs

focusRel :: Integer -> Memory a -> Memory a
focusRel n (Memory a xs ys)
   | (>) n 0 = 
   let (hs, ts) = splitAt (fromIntegral n) ys
   in Memory (a+n) ((++) (reverse hs) xs) ts 
   | (<) n 0 = 
   let (hs, ts) = splitAt (fromIntegral (abs n)) xs
   in Memory (a+n) ts $ (++) (reverse hs) ys
   | otherwise = Memory a xs ys


focusAbs :: Integer -> Memory a -> Memory a
focusAbs n (Memory a xs ys) = focusRel (n-a) (Memory a xs ys)



evalStep :: Program -> Data -> Registers -> (Maybe Output, Program, Data, Registers)
-- Trust me, you don't want to see this...
-- Let's just agree that I did it in one line and forget about the auxiliary function? :)
evalStep (prog) (dat) (regs) = evalStepper (prog) (dat) (regs)



--- I tried to do everything without recursions, but here I did not figure out how to do without it

evalHelper :: (Maybe Output, Program, Data, Registers) -> [Output]
evalHelper (Just(a), (Memory _ _ []), _, _) = [a]
evalHelper (Just(a), (Memory _ _ ((Halt):xs)), _, _) = [a]
evalHelper (Just(a), (prog), (datas), (regs)) = (++) [a] (evalHelper (evalStep(prog) (datas) (regs)))
evalHelper (Nothing, (Memory _ _ []), _, _) = []
evalHelper (Nothing, (Memory _ _ ((Halt):xs)), _, _) = []
evalHelper (Nothing, (prog), (datas), (regs)) = (evalHelper (evalStep(prog) (datas) (regs)))

eval :: [Instruction] -> [Value] -> [Output]
eval [] _ = []
eval (instr) (val) = evalHelper (evalStep (fromList instr) (fromListInf 0 val) (Registers 0 0 0))







-- ------------------------------------------------------------------------- --
--                                   TESTS                                   --
-- ------------------------------------------------------------------------- --
-- Set the boolean to False to skip testing that function (for example, because
-- you left it undefined). It's better than commenting it out, because the test
-- runner will remind you that you have skipped the test, so that you don't
-- forget to re-enable it once you've implemented the function.

t_enabledTests :: [TGroup]
t_enabledTests = [ t_fromList      True
                 , t_focusRel      True
                 , t_focusAbs      True
                 , t_getRegister   True
                 , t_setRegister   True
                 , t_evalStep      True
                 , t_eval          True
                 , t_programs      True
                 ]

t_fromList, t_focusRel, t_focusAbs, t_getRegister, t_setRegister,
            t_evalStep, t_eval, t_programs :: Bool -> TGroup

-- Test cases are a list of pairs (input value, expected result). Do add your
-- own! It is much more comfortable than putting it in the interpreter all over
-- again and checking whether the result is alright.

t_fromList = t_unary "fromList" fromList
    [ ([], Memory 0 [] [])
    , ([42, 42, 42], Memory 0 [] [42, 42, 42]) ]

t_focusRel = t_binary "focusRel" focusRel
    [ (2, Memory 0 [] [1, 2, 3, 4], Memory 2 [2, 1] [3, 4])
    , (1, Memory 2 [2, 1] [3, 4],   Memory 3 [3, 2, 1] [4]) ]

t_focusAbs = t_binary "focusAbs" focusAbs
    [ (2, Memory 0 [] [1, 2, 3, 4], Memory 2 [2, 1] [3, 4])
    , (2, Memory 3 [3, 2, 1] [4], Memory 2 [2, 1] [3, 4]) ]

t_getRegister = t_binary "getRegister" getRegister
    [ (Registers 42 66 0, R1, 42) ]

t_setRegister = t_ternary "setRegister" setRegister
    [ (Registers 42 66 0, R3, 42, Registers 42 66 42)
    , (Registers 42 66 0, R2, 15, Registers 42 15  0) ]


t_evalStep = t_ternary "evalStep" evalStep
   [ (Memory 0 [] [], Memory 0 [] [], Registers 0 0 0,
                    (Nothing, Memory 0 [] [], Memory 0 [] [], Registers 0 0 0))
   , (Memory 0 [] [Assign R1 42], Memory 0 [] [], Registers 0 0 0,
                    (Nothing, Memory 1 [Assign R1 42] [], Memory 0 [] [], Registers 42 0 0))
   , (Memory 0 [] [Assign R1 42, Halt], Memory 0 [] [], Registers 0 0 0,
                    (Nothing, Memory 1 [Assign R1 42] [Halt], Memory 0 [] [], Registers 42 0 0))
   , (Memory 0 [] [Halt], Memory 0 [] [], Registers 0 0 0,
                    (Nothing, Memory 0 [] [Halt], Memory 0 [] [], Registers 0 0 0))
   ]

t_eval = t_binary "eval" eval
   [ ([Trace "Hello", Assign R3 42, Trace " World!", Out R3], [], [S "Hello", S " World!", N 42]), ([Assign R1 42, Jump Absolute 3, Halt, Out R1], [], [N 42])
 , ([Assign R1 42, Assign R2 31, Halt, Out R1], [], [])
   ]

t_programs = t_unary "programs" id
   [ (take 10 fibs, map N [0,1,1,2,3,5,8,13,21,34])
   , (mulEval 67 879, [N 58893])
   , (powEval 8 12, [N 68719476736])
   , (bubblesort [5, 0, -1, 9, 0, 6], S "Finished sorting" : map N [-1,0,0,5,6,9])
   , (eratosthenes 42, map N [2,3,5,7,11,13,17,19,23,29,31,37,41])
   ]

-- Add your own tests!

--------------------------------------------------------------------
--                       A S S E M B L E R                        --
--------------------------------------------------------------------

-- These are some demo programs -- first using our Instruction, second using
-- our special type Assembly. Check out the assignment for more.

-- Infinite Fibonacci sequence (use ‹take 20 fibs› to test)
-- Input: none
fibInst :: [Instruction]
fibInst = [ Assign      R2  1
          , Out         R1
          , Move        R3  R2
          , Add         R2  R1
          , Move        R1  R3
          , Jump  Absolute  1
          ]

fibs :: [Output]
fibs = eval fibInst []

-- We are using bracket notation to refer to data in memory. So [0] refers to
-- data on index 0, [R1] to data on index inside R1. Furthermore, ‹F› means
-- “focused position”, so if some action or subroutine “scratches [F]”, it
-- overrides what is stored on the currently focused position. ‹Rs› means R1
-- through R3.

-- Loads [0] and [1] to R1 and R2, respectively, runs ‹sub› and outputs R1.
showcase :: [Instruction] -> [Instruction]
showcase sub = [ Load        R1
               , Assign      R3  1
               , Focus Relative  R3
               , Load        R2
               , Negate      R3
               , Focus Relative  R3
               ] ++ sub ++
               [ Out         R1
               ]

-- R1 := R1 * R2 (9 instructions; scratches Rs and [F])
mulR1R2 :: [Instruction]
mulR1R2 = [ Move        R3  R1
          , Assign      R1  0
          , JumpIf Zero R2 Relative 7
          , Add         R1  R3
          , Store       R1
          , Assign      R1 (-1)
          , Add         R2  R1
          , Load        R1
          , Jump  Relative  (-6)
          ]

mulEval :: Value -> Value -> [Output]
mulEval x y = eval (showcase mulR1R2) [x, y]

-- R1 := R1 ^ R2 (26 instructions; scratches Rs and [F-1]..[F+1])
powR1R2 :: [Instruction]
powR1R2 = [ Store       R1
          , Assign      R1  1
          , JumpIf Zero R2  Relative 24
          , Assign      R3  1
          , Focus Relative  R3
          , Negate      R3
          , Add         R2  R3
          , Store       R2
          , Focus Relative  R3
          , Load        R2
          , Focus Relative  R3
          ] ++ mulR1R2 ++
          [ Assign      R3  2
          , Focus Relative  R3
          , Load        R2
          , Assign      R3 (-1)
          , Focus Relative  R3
          , Jump  Relative (-23)
          ]
powEval :: Value -> Value -> [Output]
powEval x y = eval (showcase powR1R2) [x, y]

-- Bubble sort.
-- Input: [N, x1, x2, ..., xN] (i.e., number of values and then the values).
-- R1 is mostly used as pointer to the currently compared number,
-- [0] = length, [-1] = dirty (swap happened, another pass needed).
bubInst :: [Instruction]
bubInst = [ Load        R1                  -- dirty if input [0] is not empty
          , Assign      R3  (-1)
          , Focus Absolute  R3
          , Store       R1

          , Assign      R3 (-1)             -- 04: goto finish if not dirty
          , Focus Absolute  R3
          , Load        R2
          , JumpIf Zero R2 Absolute 32
          , Assign      R2 0
          , Store       R2
          , Focus Absolute  R2

          , Load        R1                  -- one pass

          , Focus Absolute  R1              -- 12: one pair
          , Load        R2
          , Assign      R3 (-1)
          , Add         R1  R3
          , JumpIf Zero R1  Absolute 4      -- finished a pass
          , Focus Relative  R3
          , Load        R3
          , Negate      R3                  -- compare pair
          , Add         R3  R2
          , JumpIf Zero R3  Relative 10     -- don't swap
          , JumpIf Neg  R3  Relative 9      -- don't swap

          , Load        R3                  -- swap
          , Store       R2
          , Assign      R2  1
          , Focus Relative  R2
          , Store       R3
          , Assign      R3 (-1)             -- mark dirty
          , Focus Absolute  R3
          , Store       R3

          , Jump  Absolute  12              -- finished a pair

          , Trace "Finished sorting"        -- 32: output the sorted list

          , Assign      R1  0
          , Focus Absolute  R1
          , Load        R1
          , Assign      R3  (-1)

          , JumpIf Zero R1  Relative 2      -- finished
          , Jump  Relative  3
          , Halt
          , Trace "Unreachable - ignored halt"

          , Focus Absolute  R1              -- print [R1--]
          , Load        R2
          , Out         R2
          , Add         R1  R3
          , Jump  Relative (-8)
          ]
bubblesort :: [Integer] -> [Output]
bubblesort xs = eval bubInst $ toInteger (length xs) : xs

-- See, the Assembly is quite similar, the difference is in the case of key words.
-- And as ‹negate› already exists, the instruction is called ‹neg› here.
-- Of course, the reason to introduce this are the labels which you can use in
-- place of numerical addresses in the jump instructions.
bubAsm :: [Assembly]
bubAsm = [     load        R1                  -- dirty if input [0] is not empty
         ,     assign      R3  (-1)
         ,     focus Absolute  R3
         ,     store       R1

         , "check dirty" :>
               assign      R3 (-1)
         ,     focus Absolute  R3
         ,     load        R2
         ,     jumpIf Zero R2 Absolute "finish"
         ,     assign      R2 0
         ,     store       R2
         ,     focus Absolute  R2

         , "pass" :>
               load        R1

         , "pair" :>
               focus Absolute  R1
         ,     load        R2
         ,     assign      R3 (-1)
         ,     add         R1  R3
         ,     jumpIf Zero R1  Absolute "check dirty"
         ,     focus Relative  R3
         ,     load        R3
         ,     neg         R3                  -- compare pair
         ,     add         R3  R2
         ,     jumpIf Zero R3  Relative "noswap"
         ,     jumpIf Neg  R3  Relative "also noswap"

         , "swap" :>
               load        R3                  -- swap
         ,     store       R2
         ,     assign      R2  1
         ,     focus Relative  R2
         ,     store       R3
         ,     assign      R3 (-1)             -- mark dirty
         ,     focus Absolute  R3
         ,     store       R3

         , "noswap" :>
           "also noswap" :>
               jump  Absolute  "pair"

         , "finish" :>
               trace "Finished sorting"

         ,     assign      R1  0
         ,     focus Absolute  R1
         ,     load        R1
         ,     assign      R3  (-1)

         , "print loop" :>
               jumpIf Zero R1  Relative 2      -- finished
         ,     jump  Relative  3
         ,     halt
         ,     trace "Unreachable - ignored halt"

         ,     focus Absolute  R1              -- print [R1--]
         ,     load        R2
         ,     out         R2
         ,     add         R1  R3
         ,     jump  Relative "print loop"
         ]

-- Primes up to some N using the Sieve of Eratosthenes.
-- Input: [N]
-- Used memory: [0] = max, [1] = p (current prime), [-1] = cur (current number
-- divisible by p), [2]..[max] = marking numbers (is zero for numbers not
-- divisible by any 1 < q <= p).
eratAsm :: [Assembly]
eratAsm = [     assign      R1  1           -- p = 1
          ,     focus Absolute  R1
          ,     store       R1
          ,     move        R2  R1
          , "nextprime" :>
                assign      R1  1
          ,     focus Absolute  R1
          ,     load        R2
          , "tryprime" :>
                add         R2  R1
          ,     focus Absolute  R2
          ,     load        R3
          ,     jumpIf Pos  R3  Relative "tryprime"
                                    -- now: R1 = 1, R2 = p, R3 = 0
          ,     focus Absolute  R1  -- store p
          ,     store       R2
          ,     neg         R1
          ,     focus Absolute  R1  -- store cur = p
          ,     store       R2

          ,     assign      R3  0           -- halt if p > max (R2 > R1)
          ,     focus Absolute  R3
          ,     load        R1
          ,     neg         R2
          ,     add         R2  R1
          ,     jumpIf Pos  R2  Relative 3
          ,     jumpIf Zero R2  Relative 2
          ,     halt

          ,     assign      R1  1
          ,     focus Absolute  R1
          ,     load        R2
          ,     out         R2
                                    -- while cur <= max
          , "loop" :>
                assign      R3  0
          ,     focus Absolute  R3
          ,     load        R2
          ,     assign      R3  (-1)
          ,     focus Absolute  R3
          ,     load        R1
          ,     neg         R1
          ,     add         R1  R2
          ,     jumpIf Neg  R1  Absolute "nextprime"

          ,     load        R1
          ,     focus Absolute  R1
          ,     store       R1
          ,     neg         R3
          ,     focus Absolute  R3
          ,     load        R2
          ,     add         R1  R2
          ,     neg         R3
          ,     focus Absolute  R3
          ,     store       R1
          ,     jump  Absolute  "loop"
          ]

eratosthenes :: Value -> [Output]
eratosthenes n = eval (assemble eratAsm) [n]

-- Add your own!


--------------------------------------------------------------------------
--                      S T O P   R E A D I N G                         --
--------------------------------------------------------------------------


evalStepper :: Program -> Data -> Registers -> (Maybe Output, Program, Data, Registers)

-- Okay, you decided to get in here yourself...

--- 1st block ---
--- Move ---
evalStepper (Memory m xxx ((Move reg1 reg2):xs)) (datas) (regs) = (Nothing, (Memory (m+1) ((Move reg1 reg2):xxx) xs), datas, (setRegister regs reg1 (getRegister regs reg2)))

--- Assign ---
evalStepper (Memory m xxx ((Assign reg1 num):xs)) (datas) (regs) = (Nothing, (Memory (m+1) ((Assign reg1 num):xxx) xs), datas, (setRegister (regs) reg1 num))

--- Add Register Name ---
evalStepper (Memory m xxx ((Add reg1 reg2):xs)) (datas) (regs) = (Nothing, (Memory (m+1) ((Add reg1 reg2):xxx) xs), datas, (setRegister (regs) reg1 ((getRegister (regs) reg1) + (getRegister (regs) reg2))))

--- Negate ---
evalStepper (Memory m xxx ((Negate reg1):xs)) (datas) (regs) = (Nothing, (Memory (m+1) ((Negate reg1):xxx) xs), datas, (setRegister regs reg1 (-(getRegister regs reg1))))

--- 2nd block ---
--- Load ---
evalStepper (Memory m xxx ((Load reg1):xs)) (Memory mem we (w:ew)) (regs) = (Nothing, (Memory (m+1) ((Load reg1):xxx) xs), (Memory mem we (w:ew)), (setRegister (regs) reg1 w))
--- Store ---
evalStepper (Memory m xxx ((Store reg1):xs)) (Memory mem we (w:ew)) (regs) = (Nothing, (Memory (m+1) ((Store reg1):xxx) xs), (Memory mem we ((getRegister (regs) reg1) : ew)), regs)
--- Focus ---
--- abs --
evalStepper (Memory m xxx ((Focus Absolute reg1):xs)) (datas) (regs) = (Nothing, (Memory (m+1) ((Focus Absolute reg1):xxx) xs), (focusAbs (getRegister (regs) reg1) (datas)), regs)
--- rel ---
evalStepper (Memory m xxx ((Focus Relative reg1):xs)) (datas) (regs) = (Nothing, (Memory (m+1) ((Focus Relative reg1):xxx) xs), (focusRel (getRegister (regs) reg1) (datas)), regs)

--- 3rd block (max cringe) ---
--- Jump ---
--- rel ---
evalStepper (Memory m xxx ((Jump Relative num):xs)) (datas) (regs) = (Nothing, (focusRel num (Memory m xxx ((Jump Relative num):xs))), datas, regs)
--- abs ---
evalStepper (Memory m xxx ((Jump Absolute num):xs)) (datas) (regs) = (Nothing, (focusAbs num (Memory m xxx ((Jump Absolute num):xs))), datas, regs)
--- JumpIf ---
evalStepper (Memory m xxx ((JumpIf n reg1 x num):xs)) (datas) (regs) 
   | (((n == Pos) && (getRegister regs reg1) > 0) || ((n == Neg) && (getRegister regs reg1) < 0) || ((n == Zero) && (getRegister regs reg1) == 0)) && (x == Relative) = 
   (Nothing, (focusRel num (Memory m xxx ((JumpIf n reg1 x num):xs))), datas, regs)
   | (((n == Pos) && (getRegister regs reg1) > 0) || ((n == Neg) && (getRegister regs reg1) < 0) || ((n == Zero) && (getRegister regs reg1) == 0)) && (x == Absolute) = 
   (Nothing, (focusAbs num (Memory m xxx ((JumpIf n reg1 x num):xs))), datas, regs)
   | otherwise = (Nothing,(Memory (m+1) ((JumpIf n reg1 x num):xxx) xs), datas, regs)

--- 4th block ---
   
evalStepper (Memory m xxx ((Out reg1):xs)) (datas) (regs) = (Just (N (getRegister regs reg1)), (Memory (m+1) ((Out reg1):xxx) xs), datas, regs)
evalStepper (Memory m xxx ((Trace st):xs)) (datas) (regs) = (Just (S (st)), (Memory (m+1) ((Trace st):xxx) xs), datas, regs)

--- otherwise ---

evalStepper (prog) (datas) (regs) = (Nothing, prog, datas, regs)



type AssInst = Integer -> [(String, Integer)] -> Instruction
data Assembly = A AssInst | (:>) String Assembly
infixr 6 :>


-- Ugly.
load, store, neg, out :: RegisterName -> Assembly
load  = A . const2 . Load
store = A . const2 . Store
neg   = A . const2 . Negate
out   = A . const2 . Out

move, add :: RegisterName -> RegisterName -> Assembly
move d = A . const2 . Move d
add d  = A . const2 . Add d

assign :: RegisterName -> Integer -> Assembly
assign d = A . const2 . Assign d

focus :: AddressType -> RegisterName -> Assembly
focus t = A . const2 . Focus t

trace :: String -> Assembly
trace = A . const2 . Trace

halt :: Assembly
halt = A . const2 $ Halt

jump :: Show addr => AddressType -> addr -> Assembly
jump t a = A $ mkJumpUgly Jump t a

jumpIf :: Show addr => Condition -> RegisterName -> AddressType -> addr -> Assembly
jumpIf c s t a = A $ mkJumpUgly (JumpIf c s) t a

const2 :: a -> b -> c -> a
const2 x _ _ = x

-- Exceptionally ugly. Do not try this at home :-).
-- Unfortunately, making this beautiful would require some GHC extensions which
-- we really don't want to force onto you. Generally, using ‹read› after ‹show›
-- is absolutely horrendous, but we need it here, because the ‹Show› class has
-- some built-in magic that we cannot replicate in the ‹Jumpee› class (type
-- defaulting, if you are interested). Without it, numbers in our jumps would
-- have to be explicitly typed, e.g. ‹jump Absolute (3 :: Integer)›.
mkJumpUgly :: Show addr => (AddressType -> Integer -> Instruction) -> AddressType -> addr -> AssInst
mkJumpUgly inst at dst = if isStr then mkJump inst at (read str :: String)
                                  else mkJump inst at (read str :: Integer)
        where str   = show dst
              isStr = head str == '"'

class Jumpee addr where
    mkJump :: (AddressType -> Integer -> Instruction) -> AddressType -> addr -> AssInst

instance Jumpee Integer where
    mkJump inst at dstAddr _ _ = inst at dstAddr

instance IsChar char => Jumpee [char] where
    mkJump inst at dstSym' srcAddr symbols = inst at dstAddr
        where dstSym = map char dstSym'
              symAddr = fromMaybe (error $ "Symbol not found: " ++ dstSym)
                                  (lookup dstSym symbols)
              dstAddr = case at of
                            Absolute -> symAddr
                            Relative -> symAddr - srcAddr

class IsChar c where
    char :: c -> Char
instance IsChar Char where
    char = id

-- This is kinda nice again.
assemble :: [Assembly] -> [Instruction]
assemble insts = map desym ixed
    where
        ixed :: [(Integer, Assembly)]
        ixed = zip [0..] insts
        syms :: [(String, Integer)]
        syms = concatMap (\(ix, inst) -> map (flip (,) ix) $ collectSyms inst) ixed

        desym :: (Integer, Assembly) -> Instruction
        desym (ix, A ass)  = ass ix syms
        desym (ix, _ :> i) = desym (ix, i)

        collectSyms :: Assembly -> [String]
        collectSyms (A _)    = []
        collectSyms (s :> i) = s : collectSyms i


-- Unit test utilities

type TGroup = IO ()

t_unary :: (Show a, Show b, Eq b) => String -> (a -> b) -> [(a, b)] -> Bool -> IO ()
t_unary name f cases enabled = putStr (name ++ ": ") >> go'
    where go' = if enabled then go 0 cases else putStrLn "SKIPPED!"

          go n [] = putStrLn . unwords $ ["all", show n, "tests passed."]
          go n ((c,e):cs) = let r = f c
                            in if r == e then go (n + 1) cs
                                         else putStrLn . unlines $ ["FAILED!"
                                                                   ,"Input: "    ++ show c
                                                                   ,"Expected: " ++ show e
                                                                   ,"But got: "  ++ show r
                                                                   ]

t_binary :: (Show a, Show b, Show c, Eq c)
         => String -> (a -> b -> c) -> [(a, b, c)] -> Bool -> IO ()
t_binary name f cases enabled = t_unary name (uncurry f) cases' enabled
    where
          cases' = map (\(a, b, c) -> ((a, b), c)) cases

t_ternary :: (Show a, Show b, Show c, Show d, Eq d)
          => String -> (a -> b -> c -> d) -> [(a, b, c, d)] -> Bool -> IO ()
t_ternary name f cases enabled = t_binary name (uncurry  f) cases' enabled
    where
          cases' = map (\(a, b, c, d) -> ((a, b), c, d)) cases

main :: IO ()
main = sequence_ t_enabledTests
