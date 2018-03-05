import Data.Maybe
data Op = Add | Sub | Store | Load | Jump | BranZ | BranP| Input | Nop deriving (Show, Eq)


data Ins = Ins Op Int deriving (Show, Eq)

halt :: Ins
halt = Ins Nop 0


parseInstruction :: Int -> Ins
parseInstruction n
  | n < 0 = halt
  | n < 99 = Ins Nop n
  | n < 200 = Ins Add (n - 100)
  | n < 300 = Ins Sub (n - 200)
  | n < 400 = Ins Store (n - 300)
  | n < 600 = Ins Load (n - 500)
  | n < 700 = Ins Jump (n - 600)
  | n < 800 = Ins BranZ (n - 700)
  | n < 900 = Ins BranP (n - 800)
  | n < 1000 = Ins Input (n - 900)
  | n > 999 = halt


data State = State {
                    ctr :: Int,
                    acc :: Int,
                    ram :: [Int]
                   } deriving (Show, Eq)



mutateRam :: [Int] -> Int -> Int -> [Int]
mutateRam ram idx val
  | idx <= length ram =
    let
      before = take idx ram
      after = drop (idx+1) ram
    in (before ++ [val] ++ after)
  | otherwise = ram


execute :: State -> State
execute s =
  let
    _ram = (ram s)
    _ctr = (ctr s)
    _acc = (acc s)
    currentInstruction = parseInstruction $ _ram !! _ctr
    nopIncreaseCounter = State{acc=_acc, ctr=(_ctr+1), ram=_ram}
    handleInst = case(currentInstruction) of
      Ins Nop _   -> nopIncreaseCounter
      Ins Add n   ->  State{acc=(_acc + _ram !! n), ctr = (_ctr+1), ram = _ram}
      Ins Sub n   ->  State{acc=(_acc - _ram !! n), ctr = (_ctr+1), ram = _ram}
      Ins Store n ->  State{acc = _acc, ctr = (_ctr + 1), ram = (mutateRam _ram n _acc)}
      Ins Load n  ->  State{acc = (_ram !! n), ctr = (_ctr + 1), ram = _ram}
      Ins Jump n  ->  State{acc = _acc, ctr = n, ram = _ram}
      Ins BranZ n -> if _acc == 0 then  State{acc=_acc, ctr = n, ram = _ram} else nopIncreaseCounter
      Ins BranP n -> if _acc >= 0 then  State{acc=_acc, ctr = n, ram = _ram} else nopIncreaseCounter
    in(handleInst)


fillOut x = x ++ map (\x -> 0) [1..(100 - (length x))]

statConstruct = State {acc=0, ctr=0, ram = fillOut []}


ex ::  IO (State) -> (State -> State) -> IO (State)
ex s f = do
  putStr "\n"
  putStr $ show <$> s
  ex (f <$> s) f





start xs = execute $ State {acc=0, ctr=0, ram= (fillOut xs)}

