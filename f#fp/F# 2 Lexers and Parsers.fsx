let rec toList (i : string) = 
    if i.Length > 0 
      then ((i.Chars(0))::(toList (i.Substring(1))))
      else [];;

toList "abcde";;

type lexical = BRA | KET | DOT | SYMATOM of char list | NUMATOM of int ;;

let stopcases = toList "().\n\t " ;;

let rec mem x l = 
  match l with
  | [] -> false
  | (y::ys) -> if x = y 
                 then true 
                 else mem x ys ;;

let stopcase x =  mem x stopcases ;;

let digits = toList "1234567890" ;;

let digit x = mem x digits ;;

let alpha x = not(mem  x  (digits @ stopcases)) ;;

let digitToint i = 
   match i with
   |  '0' -> 0
   | '1' -> 1
   | '2' -> 2
   | '3' -> 3
   | '4' -> 4
   | '5' -> 5
   | '6' -> 6
   | '7' -> 7
   | '8' -> 8
   | '9' -> 9
   | _ -> 0 ;;

let mkint s =
 let rec mkint' s a =
   match s with 
   |[] -> a
   | (x::xs) ->  mkint' xs ((a*10) + (digitToint x))
 mkint' s 0;;


let rec lexSym s l =
  match l with 
  |  [] -> (SYMATOM s, [])
  | (x::xs) -> if stopcase x 
               then (SYMATOM s, x::xs) 
               else lexSym (s@[x]) xs ;;

let rec lexNumOrSym s l =
 match l with
    |[]-> (NUMATOM(mkint s), []) 
    |(x::xs) -> 
        (if (stopcase x) then ( NUMATOM(mkint s), x::xs)
        else if (alpha x) then lexSym (s@[x]) xs
        else lexNumOrSym  (s@[x]) xs ) ;;

let rec lexer l = 
  match l with
  | []       -> []
  | '\n'::xs -> lexer xs
  | ' '::xs  -> lexer xs
  | '\t'::xs -> lexer xs
  | '('::xs  -> BRA::(lexer xs)
  | ')'::xs  -> KET::(lexer xs)
  | '.'::xs  -> DOT::(lexer xs)
  | x::xs    -> 
    let (l,r) = if alpha x 
                then (lexSym [x]  xs)
                else (lexNumOrSym [x] xs)
    l:: lexer r;;

// ---

type 'a Maybe = Return of 'a | Fail;;

let parseSym  s =
 match s with
 | Return((SYMATOM y)::ys, _) ->
       Return(ys,[SYMATOM y]) 
 | _ ->  Fail ;;

let parseNum  s =
 match s with
 | Return((NUMATOM y)::ys, a) ->  Return(ys,[NUMATOM y]) 
 | _ ->  Fail ;;

let parseConst x s =
  match s with 
  | Return(y::ys, a) -> if x = y then Return(ys, []) else Fail 
  | _ ->  Fail ;;

let bra x = parseConst BRA x;;

let ket x = parseConst KET x;;

let dot x = parseConst DOT x;;

let OTHERWISE f g  x =
  let result = f x
  in
   if result = Fail then (g x) else result ;;

let (||) = OTHERWISE;;

let THENDO  f g x =
  let u = f x
  match u with
  | Return(rest1, result1) 
               -> let v = g u
                  match v with
                  | Return(rest2, result2) -> Return(rest2, result1 @ result2)
                  | Fail-> Fail
    | Fail -> Fail ;;

let (>>) = THENDO;;

let rec ITERATE f x = 
  let u = f x
  match u with 
  | Fail -> x
  | Return(rest2, result2) -> 
           match x with
           | Fail -> Fail
           | Return(rest1, result1) -> 
                         ITERATE f (Return(rest2, result1 @ result2));;

type SEXPR = SYMBOL of char list | NUMBER of int | NIL | CONS of SEXPR * SEXPR ;;

let TRANSFORM f h s =
  let u = f s
  match u with
  | Return(rest, result) 
               ->  Return(rest, h result)
  | Fail -> Fail ;;

let (>*) = TRANSFORM;;

let CONSPAIR [ x; y ] = [CONS(x,y)];;

let CONSLIST l =
 let rec conslist y =
   match y with
   | x::xs -> CONS(x, conslist xs)
   | [] -> NIL 
 [conslist l];;

let ATOMACTION v  =
  match v with
  | [SYMATOM u] ->  [SYMBOL u] 
  | [NUMATOM i] ->  [NUMBER i]
  | _ ->  [];;

let parseAtom  s = ((parseSym >* ATOMACTION) ||  (parseNum >* ATOMACTION)) s ;;

let rec parseSEXPR s = 
  (parseAtom ||   
   (bra >> parseSEXPR >> dot >> parseSEXPR >> ket >* CONSPAIR) ||
   (bra >>  (ITERATE parseSEXPR) >* CONSLIST >> ket)
  ) s;;

let parse str = 
  let r = parseSEXPR (Return (lexer (toList str), [])) 
  match r with 
  | Return(_ , result::_) -> result
  | Return(_, []) -> NIL
  | Fail -> NIL ;;

parse "( ((ABC)) . (123. 456))";;






