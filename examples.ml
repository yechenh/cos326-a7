open Future
open Mpi 

module F = Future.PFuture


(* Apply f to (low,high) segments of numbers from 0 to n.
 * Here we use a default of 3 chunks.
 *)
let chunks (f: int * int -> 'a) (n:int): 'a array = 
  let num_chunks = 3 in 
  Array.init num_chunks (fun i -> 
    let lo = (n * i) / num_chunks in 
    let hi = (n * (i+1) / num_chunks) - 1 in 
    f (lo,hi) 
  )


(*******************************************************
 * Example of how to operate on an array in parallel
 *******************************************************)

let example_chunks () = 
  let rec sum lo hi = 
    if lo = hi then lo
    else lo + sum (lo+1) hi
  in

  let cs = chunks (fun (lo,hi) -> F.future (sum lo) hi) 9 in 
  assert (Array.length cs = 3);
  print_endline "sum of 0,1,2 is 3";
  assert (F.force cs.(0) = 3);
  print_endline "sum of 3,4,5 is 12";
  assert (F.force cs.(1) = 12);
  print_endline "sum of 6,7,8 is 21";
  assert (F.force cs.(2) = 21)


(*******************************************************
 * Example of how to use message passing
 *******************************************************)

(* type of message sent from the parent process *)
type compute_message = 
  | Add of int*int
  | Subtract of int*int
  | Kill

type result_message = int

let example_mpi () = 

  (* This is the function the child process will execute ch has type 
     (result_message,compute_message) Mpi.channel 
     meaning the child process can send messages of type: result_message
     and receive messages of type: compute_message *)
  let handler (ch: (result_message,compute_message) Mpi.channel) () = 
    let rec aux () =
      match Mpi.receive ch with 
	| Add (x,y) -> Mpi.send ch (x+y); aux ()
	| Subtract (x,y) -> Mpi.send ch (x-y); aux ()
        | Kill -> ()
    in 
    aux ()
  in

  (* the parent process has a different channel ch, which has the type:
     (compute_message,result_message) Mpi.channel
     meaning the parent process can send messages of type: compute_message
     and receive messages of type: result_message *)
  let ch : (compute_message,result_message) Mpi.channel = Mpi.spawn handler () in 

  Mpi.send ch (Add (3,4));
  let answer = Mpi.receive ch in
  Printf.printf "3+4 is %d\n" answer;
                
  Mpi.send ch (Subtract (3,4));
  let answer = Mpi.receive ch in
  Printf.printf "3-4 is %d\n" answer;

  Mpi.send ch Kill;
                
  (* VERY important, we must clean up the child *)
  Mpi.wait_die ch

let () = 
  example_chunks ();
  example_mpi ()










