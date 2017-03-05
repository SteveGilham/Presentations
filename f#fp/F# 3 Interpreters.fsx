type [<StructuralEquality;NoComparison>] SEXPR = SYMBOL of string | NUMBER of int | NIL | CONS of (SEXPR * SEXPR) | COMPUTATION of COMP
and [<ReferenceEquality;NoComparison>] COMP = { func : SEXPR -> SEXPR } ;;

// Lexer and Parser go here

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
  | [SYMATOM u] ->  [SYMBOL (System.String(List.toArray u))] // Cheat
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


// -----

let EQUAL l1 l2 = 
  let rec tf s1 s2 =  match (s1, s2) with
                      | (SYMBOL xs, SYMBOL ys)  -> xs = ys
                      | (NIL, NIL)  -> true
                      | (NUMBER x, NUMBER y) -> x = y
                      | (CONS (a, b), CONS(c, d)) -> (tf a c) && (tf b d)
                      | _ -> false

  if tf l1 l2 then (SYMBOL "TRUE") else NIL


let rec lookup x l =
  match l with
  | CONS(CONS(a, b), c) -> match EQUAL x a with
                           | NIL -> lookup x c
                            | _ -> b
  | _ -> NIL;;

let rec MAP f l =
  match l with
  | CONS(a, b) -> CONS(f a, MAP f b)
  | _ -> NIL;;

let HD l =
  match l with
  | CONS(a, b) -> a
  | _ -> NIL;;

let TL l =
  match l with
  | CONS(a, b) -> b
  | _ -> NIL;;

let NULL l =
  match l with
  | NIL -> (SYMBOL "TRUE")
  | _ -> NIL;;

let ATOMP l =
  match l with
  | (SYMBOL _) -> (SYMBOL "TRUE")
  | (NUMBER _) -> (SYMBOL "TRUE")
  | _ -> NIL;;

let SYMBOLP l =
  match l with
  | (SYMBOL _) -> (SYMBOL "TRUE")
  | _ -> NIL;;

let NUMBERP l =
  match l with
  | (NUMBER _) -> (SYMBOL "TRUE")
  | _ -> NIL;;

let LISTP l =
  match l with
  | CONS(a, b) -> (SYMBOL "TRUE")
  | NIL -> (SYMBOL "TRUE")
  | _ -> NIL;;

let OPERATOR op k l = 
 let rec AUX l =
  match l with
  |  CONS((NUMBER a), b) -> op a (AUX (TL l))
  | _ -> k
 NUMBER (AUX l) ;;

let ADD l = OPERATOR  ( + ) 0 l;;

let MULT l = OPERATOR ( * ) 1 l;;

let SUB l = OPERATOR ( - ) 0 l;;

let DIV l = OPERATOR ( / ) 1 l;;

let ZEROP l = 
  match l with
  |  (NUMBER 0) -> (SYMBOL "TRUE")
  |_ -> NIL;;

let rec APPEND l1 l2 =
  match l1 with
  | CONS(a, b) -> CONS(a, APPEND b l2)
  | NIL -> l2
  | _ -> NIL;;

let rec ZIP l1 l2 =
  match  (l1, l2) with
  | (CONS(x, xs), CONS(y, ys)) -> CONS(CONS(x, y), ZIP xs ys)
  | _ -> NIL;;

let apply f l = 
  match f with
  | COMPUTATION g -> g.func l
  | _ -> NIL;;
 
let rec applySnd f l = 
  match l with
  |  CONS(CONS(n, v), r) -> CONS(CONS(n, f v), applySnd f r)
  | _ -> NIL;;


let BHD X = HD(HD X);;
let BTL X = TL(HD X);;
let BCONS X = CONS(HD X, HD(TL X));;
let BNULL X = NULL(HD X);;
let BATOMP X = NULL(HD X);;
let BSYMBOLP X = SYMBOLP (HD X);;
let BNUMBERP X = NUMBERP(HD X);;
let BLISTP X = LISTP(HD X);;
let BEQUAL X = EQUAL(HD X) (HD (TL X));;
let BADD X = ADD X;;
let BMULT X = MULT X;;
let BSUB X = SUB X;;
let BDIV X = DIV X;;
let BZEROP X = ZEROP (HD X);;

let builtin = 
    [SYMBOL "TRUE", SYMBOL "TRUE";
     SYMBOL "FALSE", NIL;
     SYMBOL "HD", COMPUTATION{func = BHD};
     SYMBOL "TL", COMPUTATION{func = BTL};
     SYMBOL "CONS", COMPUTATION{func = BCONS};
     SYMBOL "NULL", COMPUTATION{func = BNULL};
     SYMBOL "ATOMP", COMPUTATION{func = BATOMP};
     SYMBOL "SYMBOLP", COMPUTATION{func = BSYMBOLP};
     SYMBOL "NUMBERP", COMPUTATION{func = BNUMBERP};
     SYMBOL "LISTP", COMPUTATION{func = BLISTP};
     SYMBOL "EQUAL", COMPUTATION{func = BEQUAL};
     SYMBOL "ADD", COMPUTATION{func = BADD};
     SYMBOL "MULT", COMPUTATION{func = BMULT};
     SYMBOL "SUB", COMPUTATION{func = BSUB};
     SYMBOL "DIV", COMPUTATION{func = BDIV};
     SYMBOL "ZEROP", COMPUTATION{func = BZEROP}];;

let rec makeEnv l =
  match l with
  | [] -> NIL
  | (x1, x2)::xs -> CONS(CONS(x1, x2), makeEnv xs);;

let BASE = makeEnv builtin;;

(* add error routines so that errors in secial forms report error and
   impose error propogation in eval *)

let ERROR1 x =  CONS((SYMBOL "ERROR"), CONS(x, NIL));;
let ERROR2 name x =  CONS((SYMBOL "ERROR"), CONS(CONS(SYMBOL name, x), NIL));;

let rec eval e x = 
  match x with
  | (SYMBOL s) -> lookup (SYMBOL s) e
  | (NUMBER i) -> NUMBER i
  | NIL -> NIL
  | CONS((SYMBOL "IF"), b) -> match b with
                              | CONS(cond, CONS(thenpart, CONS(elsepart, NIL))) ->
                                    match (eval e cond) with 
                                    | NIL -> (eval e elsepart)
                                    | _ -> (eval e thenpart)
                              | _ -> ERROR2 "IF" b

  | CONS((SYMBOL "LET"), b) -> match b with
                               | CONS(bindings, CONS(body, NIL)) ->
                                    let pairs = applySnd (eval e) bindings
                                    eval (APPEND pairs e) body
                               | _ -> ERROR2 "LET" b


  | CONS((SYMBOL "LAMBDA"), b) -> match b with
                                  | CONS(formals, CONS(body, NIL)) ->
                                     COMPUTATION {func = (fun actuals -> let pairs = ZIP formals  actuals
                                                                         eval (APPEND pairs e) body)}
                                  | _ -> ERROR2 "LAMBDA" b


  | CONS((SYMBOL "REC"), b) -> match b with
                               |  CONS(F, CONS(formals, CONS(body, NIL))) ->
                                   let rec f =
                                     COMPUTATION {func = (fun actuals -> let pairs = ZIP formals actuals
                                                                         eval (APPEND pairs (CONS(CONS(F, f), e))) body)}
                                   f
                               | _ -> ERROR2 "REC" b

  | CONS((SYMBOL "QUOTE"), CONS(X, NIL)) -> X
  | CONS((SYMBOL "ENV"), NIL)  -> e
  | CONS((SYMBOL "EVAL"), CONS(X, NIL)) -> eval e (eval e X)
  | CONS((SYMBOL "EVAL"), CONS(X, CONS(Y, NIL))) ->  eval (eval e Y) (eval e X)
  | CONS(a, b) -> apply (eval e a) (MAP (eval e) b)
  | X -> ERROR1 X;;




(* tidy up *)

let exec str = eval BASE (parse str);;

(* tests *)
exec "(LET ((F . (LAMBDA (X) (MULT X X)))) (F 6))";;
exec "(LET ((F . (REC F (N) (IF (EQUAL N 0) 1 (MULT N (F (SUB N 1))))) )) (F 6))";;
exec "(LET ((LEN . (REC F (L) (IF (EQUAL L NIL) 0 (ADD 1 (F (TL L))) ) ) )) (LEN (QUOTE (1 2 3))))";;
