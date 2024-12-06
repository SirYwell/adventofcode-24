structure Vec where
  x : Int
  y : Int
deriving Repr, BEq

def toIdx (dims : Vec) (vec : Vec) : Nat := (vec.y * dims.x + vec.x).toNat
def toVec (dims : Vec) (nat : Nat) := Vec.mk (nat % dims.x) (nat / dims.x)
def dirs : Array Vec := #[(Vec.mk 0 (-1)), (Vec.mk 1 0), (Vec.mk 0 1), (Vec.mk (-1) 0)]
def dirToIdx (vec : Vec) : Fin 4 := match (vec.x, vec.y) with
| (0, -1) => 0
| (1,  0) => 1
| (0,  1) => 2
| _ => 3

def rot90 (dir : Vec) : Vec := dirs[dirToIdx dir + (Fin.ofNat 1)]

def collectPositions (max : Nat) (array : Array Char) (dims : Vec) (set : Array UInt8) (current : Vec) (direction : Vec) : Option (Array UInt8) :=
  match max with
  | 0 => some set
  | n + 1 =>
    let idx := (current.y * dims.x + current.x).toNat
    let bit := UInt8.shiftLeft 1 (UInt8.mk (dirToIdx direction |> Fin.ofNat)) -- ?????
    let ov := set.get! idx
    if ov &&& bit != 0 then none else -- already went into that direction at this pos => cycle
    let nv := ov ||| bit
    let ns := set.set! idx nv
    let nx := direction.x + current.x
    let ny := direction.y + current.y
    if nx < 0 || ny < 0 || nx >= dims.x || ny >= dims.y then some ns
    else if array.get! (toIdx dims (Vec.mk nx ny)) == '#' then collectPositions n array dims ns current (rot90 direction)
    else collectPositions n array dims ns (Vec.mk nx ny) direction

def toDirs (n : UInt8) (d : UInt8 := 4) : List Vec := match d.toNat with
| 0 => if n &&& (1 <<< d) != 0 then [dirs[0]] else []
| e + 1 => toDirs n e.toUInt8

def countForEntry (entry : Nat) (set : Array UInt8) : Nat :=
  let dirs := set.get! entry


def countCycles (entry : Nat) (set : Array UInt8) (acc : Nat) : Nat := match entry with
| 0 => acc + (countForEntry 0 set)
| n + 1 => countCycles n set (acc + countForEntry entry set)

def main : IO Unit := do
  let file <- IO.FS.readFile "input/day06.txt"
  let g := file.splitOn "\n" |>.map (·.toList.toArray) |>.toArray
  let dims := Vec.mk g[0]!.size g.size
  let l := g.flatten
  let guard := l.findIdx? (· == '^') |>.get!
  let arr := Array.mkArray (dims.x.toNat * dims.y.toNat) 0
  let ps := collectPositions (2 ^ 64) l dims arr (toVec dims guard) (Vec.mk 0 (-1)) |>.get!
  let s := ps.foldl (fun x a => if a == 0 then x else x + 1) 0
  IO.println s
  ps.