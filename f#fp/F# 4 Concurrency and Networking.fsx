let state = ref 0;;

// (!): ‘a ref -> ‘a
!state;;

//(:=) : ‘a ref -> ‘a -> unit
state := 1
state;;
let mutable state' = 0;;

state';;

state' <- 1
state';;

let gen f =
  let i = ref 0
  fun () -> (i := !i + 1; f !i);;

let sq x = x*x;;
let g = gen sq;;

g();;
g();;
g();;
g();;
let hd (x::xs) = x;;
let tl (x::xs) = xs;;

let stream init =
  let state = ref init
  let put x = (state := (!state)@[x])
  let get () = let r = hd !state
               state := tl !state
               r
  let isEmpty () = !state = []
  (put, get, isEmpty);;

open System.Threading;;
let spawn f = (Thread(new ThreadStart(f))).Start();; 

let getLock () = ReaderWriterLock();;
let acquireWrite (lock:ReaderWriterLock) = lock.AcquireWriterLock(-1);;
let releaseWrite (lock:ReaderWriterLock) = lock.ReleaseWriterLock();;
let acquireRead (lock:ReaderWriterLock) = lock.AcquireReaderLock(-1);;
let releaseRead (lock:ReaderWriterLock) = lock.ReleaseReaderLock();;
let upgrade (lock:ReaderWriterLock) = lock.UpgradeToWriterLock(-1);;
let downgrade (lock:ReaderWriterLock) cookie = lock.DowngradeFromWriterLock(cookie);;

let stream' init =
  let state = ref init
  let lock = getLock()
  let put x = acquireWrite lock
              state := (!state)@[x]
              releaseWrite lock
  let get () =
    let r = ref None
    acquireRead lock;
    if !state = []
        then releaseRead lock
             !r
        else let u = ref (upgrade lock)
             r := Some(hd !state)
             state := tl !state
             downgrade lock u;
             releaseRead lock
             !r
  (put, get);;

  // type 'a option = None | Some of 'a;;

let ( ||> ) f g =
 let (put,get) = stream' []
 let rec loop1 () = put( f ())
                    loop1 ()
 let rec loop2 () =
                match get() with
                  None -> ()
                | Some(x) -> g x 
                             loop2 () 
 spawn loop1
 spawn loop2;;

// for example:

let pp i = printfn "a number %d" i;;
(gen sq) ||> pp;;

// --------------------------&<----------------------------

open System.Net.Sockets;;
let read (s:NetworkStream) =
    let i = s.ReadByte()
    if i < 0 then None else Some(byte i);;
let write b (s:NetworkStream) = s.WriteByte(b);;
let close (s:NetworkStream) = s.Close();; 

(* read a null terminated stream of bytes into a list *)
let rec streamToList s =
  let b = read s
  match b with
    None  -> []
  | Some(x) -> if x = 0uy then [] else  x::(streamToList s);;
(* write  a list of bytes out as a stream *)
let rec listToStream l s =
   match l with
     [] -> ()
   | x::xs -> (write x s; listToStream xs s);;

// --------------------------&<----------------------------

open System.Net;;
open System.Net.Sockets;;
open System.Threading;;

let addressFamily = AddressFamily.InterNetwork;; 
// or AddressFamily.InterNetworkV6;;
let get_host_address() = Dns.GetHostEntry(Dns.GetHostName()).AddressList 
                         |> Seq.filter (fun (x:IPAddress) -> x.AddressFamily = addressFamily) 
                         |> Seq.head;

let local_host_address = get_host_address ();;
let spawn f = 
    (new Thread(new ThreadStart(f))).Start();;

let backlog = 2;;

let read (s:NetworkStream) =
    let i = s.ReadByte()
    if i < 0 then None else Some(int8 i);;

let write b (s:NetworkStream) = s.WriteByte(b);;

let close (s:NetworkStream) = s.Close();;

(* read a null terminated stream of bytes into a list *)

let rec streamToList s =
  let b = read s
  match b with
   None  -> []
  |Some(x) -> if x = 0y then [] else  (uint8 x)::(streamToList s);;

(* write  a list of bytes out as a stream *)

let rec listToStream l s =
   match l with
     [] -> ()
    |x::xs -> (write x s; listToStream xs s);;

(* -- create string variants for null terminated strings -- *)

let rec toList (i: string) = 
    if i.Length > 0 
      then ((i.Chars(0))::(toList (i.Substring(1))))
      else [] ;;

let rec map f l = 
  match l with
   x::xs -> (f x) :: (map f xs)
  | [] -> [];;


let rec toString (ls: char list) =
 match ls with
  [] -> ""
 |x::xs -> (string(x)) + (toString xs);;


let string_to_ascii (s:string) = System.Text.Encoding.ASCII.GetBytes(s);;

let ascii_to_string (b:byte[]) = System.Text.Encoding.ASCII.GetString(b);;

let charof b = System.Text.Encoding.ASCII.GetString([| b |]).Chars(0);;

let byteof c = System.Text.Encoding.ASCII.GetBytes([| c |] ).[0];;

let bytelist s = map byteof (toList s);;

let string bl = toString (map charof bl);;

let nullTerminated str = (bytelist str) @ [0uy];;



let writeString ns str = listToStream (nullTerminated str) ns;;

let readString ns = string (streamToList ns);;


// asynch server
let rec listen f (sock:Socket) = 
  let csock = sock.Accept()
  spawn(fun () -> (f (readString (new NetworkStream(csock))))) ; listen f sock ;;

// request-response server
let rec echo f (sock:Socket) =
  let csock = sock.Accept()
  let ns = new NetworkStream(csock)
  (spawn(fun () -> writeString ns (f (readString ns))) ; echo f sock) ;;


let listenOn f portnum  =
  let sock = new Socket(addressFamily, SocketType.Stream, ProtocolType.IP)
  sock.Bind(IPEndPoint(local_host_address, portnum));
  sock.Listen(backlog);
  listen f sock
  
let echoOn f portnum =
  let sock = new Socket(addressFamily, SocketType.Stream, ProtocolType.IP)
  sock.Bind(IPEndPoint(local_host_address, portnum));
  sock.Listen(backlog);
  echo f sock;;

let asynchListenOn f portnum = spawn(fun () -> listenOn f portnum);;

let asynchEchoOn f portnum = spawn(fun () -> echoOn f portnum);;


(* ----- client bits  ---*)

// asynch client
let message portnum mess = 
 let client_socket =  new Socket(addressFamily, SocketType.Stream, ProtocolType.IP)
 client_socket.Connect(local_host_address, portnum);
 let client_stream = new NetworkStream(client_socket)
 writeString client_stream mess;;

let converse portnum mess = 
 let client_socket =  new Socket(addressFamily, SocketType.Stream, ProtocolType.IP)
 client_socket.Connect(local_host_address, portnum);
 let client_stream = new NetworkStream(client_socket)
 writeString client_stream mess; readString client_stream;;

(* ----- example  ---*)

 // server =
let pp x = (printfn "%s" x; "Echo::" + x);;
asynchEchoOn pp 8001;;

// client =
converse 8001 "ahoy";;

