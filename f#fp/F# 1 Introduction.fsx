
// F# 1 -- Introduction

type Bit = Zero | One;;

//----

let AND x y = 
  match (x,y) with
  |(Zero, Zero) -> Zero
  |(Zero, One) -> Zero
  |(One, Zero) -> Zero
  |(One, One) -> One ;;

let AND x y = 
  match (x,y) with
  | (One, One) -> One
  | _ -> Zero;;

//----

let OR x y = 
  match (x,y) with
  | (Zero, Zero) -> Zero
  | _ -> One;;

let XOR x y = 
  match (x,y) with
  | (One, Zero) 
  | (Zero, One) -> One
  | _ -> Zero;;

//----

let Half_Adder x y =
 (XOR x y, AND x y);;

let Full_Adder x y z = 
  let (s1, c1) = Half_Adder x y
  let (s2,c2) = Half_Adder s1 z
  (s2, OR c1 c2);;

//---- 

(* introduce the built-in type, list : this is both recursive and polymorphic *)

// type 'a list =   [] 
//                | (::) of 'a * 'a list

let rec length l =
   match l with
   | [] -> 0
   | (x::xs) -> 1 + (length xs);;

length [ 5; 8; 13 ];;

//----

let rec append l1 l2 =
  match l1 with
  | [] -> l2
  | (x::xs) -> x::(append xs l2);;

let l1 = append [ 5; 8; 13 ] [4; 8; 16; 32];;

let rec rev l =
  match l with
  | [] -> []
  | (x::xs) -> append (rev xs) [x];;

rev l1;;

//----

let twice f x = f (f x);;

(fun x -> x * x);;

(fun x -> x * x) 4;;

twice (fun x -> x * x) 4;;

//----

let rec power f n x =
  if n <= 0
    then x
    else power f (n - 1) (f x);;

//----

let rec map f l =
  match l with
  |[] -> []
  |(x::xs) -> (f x)::(map f xs);;

map (fun i -> i + 1) l1;;

let rec reduce f c l =
  match l with
  |[] -> c
  |(x::xs) -> f x (reduce f c xs);;

let len' l = reduce ( + ) 0 (map (fun x -> 1) l);;

len' l1;;

//----

let rec R_Adder bl1 bl2 c =
  match (bl1, bl2) with
  | ([],[]) -> []
  | (x::xs, y::ys) -> 
      let (out, carry) = (Full_Adder x y c)
      out::(R_Adder xs ys carry)
  | _ -> [];;

//----

let rec dropleadingzeros l =
  match l with
  | [] -> []
  | (Zero::xs) -> dropleadingzeros xs
  | x -> x;;

// add n leading Zero's 
let rec padby n l = if n <= 0 then l else padby (n-1) (Zero::l);;

// pad two lists to the same length

let rec pad (l1, l2) = 
  let len1 = length l1
  let len2 = length l2
  if (len1 = len2) then (l1,l2)
  else if (len1 < len2) then (padby (len2-len1) l1, l2)
       else (l1, padby  (len1-len2) l2);;

//----

let Adder bl1 bl2 =
  let (l1,l2) = pad ((dropleadingzeros bl1), (dropleadingzeros bl2))
  rev(R_Adder (rev (Zero::l1)) (rev (Zero::l2)) Zero);;


let ll = [ One; One; One; One; One];; // 11111 = 31
let lr = [ Zero; One; Zero ];;        // 10 = 2

Adder ll lr;; // expect 100001 = 33

//----

let rec Mach f s l =
  match l with
  | [] -> []
  | (x::xs) -> 
    let (out, state) = (f x s)
    out::(Mach f state xs);;

let X_R_Adder bl1 bl2 = 
   let UC_Full_Adder (a,b) c = Full_Adder a b c
   Mach UC_Full_Adder Zero (List.zip bl1 bl2);;

let PAdder bl1 bl2 =
  let (l1,l2) = pad ((dropleadingzeros bl1), (dropleadingzeros bl2))
  rev(X_R_Adder (rev (Zero::l1)) (rev (Zero::l2)));;

PAdder lr ll;;



 