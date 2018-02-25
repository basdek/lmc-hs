import Data.Maybe
data Op = Add | Sub | Store | Load | Jump | BranZ | BranP| Nop deriving (Show, Eq)


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
  | n < 500 = Ins Load (n - 400) 
  | n < 600 = Ins Jump (n - 500) 
  | n < 700 = Ins BranZ (n - 600)
  | n < 800 = Ins BranZ (n - 700)
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

execute :: State -> State
execute s =
  let
    _ram = (ram s)
    _ctr = (ctr s)
    _acc = (acc s)
    currentInstruction = parseInstruction $ _ram !! _ctr
    handleInst = case(currentInstruction) of
      Ins Nop _   -> State{acc=_acc, ctr = (_ctr+1), ram = _ram}
      Ins Add n   -> execute State{acc=(_acc + _ram !! n), ctr = (_ctr+1), ram = _ram}
      Ins Sub n   -> execute State{acc=(_acc - _ram !! n), ctr = (_ctr+1), ram = _ram}
      Ins Store n -> execute State{acc = _acc, ctr = (_ctr + 1), ram = (mutateRam _ram n _acc)}
      Ins Load n  -> execute State{acc = (_ram !! n), ctr = (_ctr + 1), ram = _ram}
      Ins Jump n  -> execute State{acc = _acc, ctr = n, ram = _ram}
      Ins BranZ n -> if _acc == 0 then execute State{acc=_acc, ctr = n, ram = _ram} else State{acc=_acc, ctr = (_ctr+1), ram = _ram}
    in(handleInst)


fillOut x = x ++ map (\x -> 0) [1..(100 - (length x))] 





start xs = execute $ State {acc=0, ctr=0, ram= (fillOut xs)}

