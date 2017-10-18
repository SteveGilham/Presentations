type Request = Getq | Putq of char
type Response = Getp of char | Putp
type Dialog = Response list -> Request list;;

let readChar f p =
    Getq ::
    (match p with 
     | (Getp c) :: p1 -> f c p1
     | _ -> []);;

type continuation = K of (unit -> unit)
let execute (K f) = f()
let stop = K (fun () -> ());;

// Producer: (X -> continuation) -> continuation
// Consumer: X -> continuation -> continuation // strictly a continuation generator, as it produces a continuation from the output of the producer.
// Operation: continuation -> continuation;;

let putc (c: char) (k: continuation) =
    let write () = System.Console.Write(c);
                   execute k
    K write
let getc (g: char -> continuation) =
    let read () = execute (g <| System.Console.ReadKey(true).KeyChar)
    K read;;

// let (<|) f x = (f x);;         

let rec echo k = 
    let echoChar c = if c = '\r'
                     then putc '\n' k
                     else putc c (echo k)
    getc echoChar;;