open Sequence

(* doc_id is a string, representing the document id, 
 * which will be used in the values in the index. *)
module DocID = struct type t=int let compare = compare end
type doc_id = DocID.t

(* doc_index is the basic type of index, a mapping with:
 * strings as keys
 * Sets of doc_ids as values *)

(* see https://caml.inria.fr/pub/docs/manual-ocaml/libref/Map.Make.html *)
module DMap = Map.Make(String)

(* doc_loc_index  a mapping with:
 * strings as keys
 * Sets of locations -- each location has
     doc_id:  which document this word is in
     int:  this word appears as the nth word in the file
     int:  this word starts at the nth character of the file
 *)

type location = doc_id * int * int

type doc_loc_index = (location S.t) DMap.t 

(* A web-search for a phrase (list of words) returns a list of results;
  each result indicates in which document the word appears,
  and the start-character and end-character of the phrase in the doc.
  If the phrase appears more than once in a document, then
  there is more than one result with the same doc_id. *)

type result = {res_docid: doc_id; res_begin: int; res_end: int}

let show_result (docs: Util.document S.t) {res_docid=i;res_begin=b;res_end=e} =
  let {Util.id=i';Util.title=t;Util.contents=cts} = S.nth docs i in
  let b' = max 0 (b-20) in
  let e' = min (String.length cts) (e+20) in
  let excerpt = String.sub cts b' (e'-b')  in
  (print_int i; print_string ": "; print_string excerpt; print_string "\n")


(* These methods each compute an inverted index of the specified type 
 * for the contents of a file. The filename is the given string. The
 * result is the computed inverted index. *)
(* use PSeq.map_reduce to construct your index in parallel *)
(* use DMap to create a map from words to a {D,DL}Set of document ids *)
(* Some handy functions: 
 *  Util.split_words converts document contents to a list of words 
 *  String.lowercase_ascii converts a word uniformly to lower case *)

(**** START OF SOLUTION ****)
let filter (s: 'a S.t) (leq: 'a -> bool): ('a S.t * 'a S.t) = 
  let element_to_pair (input: 'a): ('a S.t * 'a S.t) = 
    if leq input then (S.singleton input, S.empty ()) else 
      (S.empty (), S.singleton input)
  in 
  let combine_two_pairs p1 p2 = 
    let fst_p1, snd_p1 = p1 in 
    let fst_p2, snd_p2 = p2 in 
    (S.append fst_p1 fst_p2, S.append snd_p1 snd_p2) 
  in 
  S.map_reduce element_to_pair combine_two_pairs (S.empty (), S.empty ()) s

let quicksort (s: 'a S.t) (compare:'a -> 'a -> int): 'a S.t = 
  let rec quicksort_aux (s: 'a S.t): 'a S.t = 
    if S.length s <= 1 then s
    (* else if S.length s = 2 then 
      if compare (S.nth s 0) (S.nth s 1) < 0 then s else 
      S.cons (S.nth s 1) (S.singleton (S.nth s 2)) *)
    else 
    let pivot = S.nth s 0 in 
    let leq x = if compare x pivot <= 0 then true else false in 
    let (_, to_be_filtered) = S.split s 1 in 
    let (smaller, bigger) = filter to_be_filtered leq in 
    let two_sub_sequences = S.cons smaller (S.singleton bigger) in
    let sorted_two_subsequences = S.map quicksort_aux two_sub_sequences in 
    S.append (S.append (S.nth sorted_two_subsequences 0) (S.singleton pivot))
      (S.nth sorted_two_subsequences 1)
  in 
  quicksort_aux s


let make_index (docs: Util.document S.t) : doc_loc_index =
   (* Process each document in parallel and turn each doc into a sequence of 
    (word, location) *)
    let process_doc (doc: Util.document): (string * location) S.t = 
      let {Util.id=i';Util.title=t;Util.contents=cts} = doc in 
      let word_seq = S.seq_of_array (Array.of_list (Util.split_words cts)) in 
      
      let one_to_n = S.tabulate (fun i -> i+1) (S.length word_seq) in 
      let zipped_word_seq = S.zip (word_seq, one_to_n) in 

      let aux pair = 
        let (word, char_count), word_count = pair in 
        ((String.lowercase_ascii word), (i', word_count, char_count))
      in 
      S.map aux zipped_word_seq
    in 
    let word_seq_seq = S.map process_doc docs in 

    (* sort the word_seq, which is a sequence of (string * location) *)
    let word_seq = S.flatten word_seq_seq in 
    let comparator (x: (string * location)) (y: (string * location)): int = 
      let (s1, _) = x in 
      let (s2, _) = y in 
      String.compare s1 s2
    in 
    let sorted_word_seq = quicksort word_seq comparator in 

    (* Combine the sorted (word, location) tuple sequences *)
    let turn_loc_to_loc_seq 
      (x: (string * location)): (string * location S.t) S.t = 
      let word, loc = x in S.singleton (word, S.singleton loc)
    in 
    let combine_two_seq (xs: (string * location S.t) S.t) 
      (ys: (string * location S.t) S.t): (string * location S.t) S.t = 
      if S.length xs = 0 then ys 
      else if S.length ys = 0 then xs 
      else 
      let fst_seq_last = S.nth xs ((S.length xs) - 1) in 
      let snd_seq_first = S.nth ys 0 in 
      let first_word, first_loc_seq = fst_seq_last in 
      let second_word, second_loc_seq = snd_seq_first in 
      if String.equal first_word second_word then 
        let first_part, _ = S.split xs ((S.length xs) - 1) in 
        let _, last_part = S.split ys 1 in 
        let middle_part = S.singleton (first_word, 
          S.append first_loc_seq second_loc_seq) 
        in 
        S.append (S.append first_part middle_part) last_part
      else
        S.append xs ys
    in 
    let base: (string * location S.t) S.t = S.empty () in 
    let final_word_seq = S.map_reduce turn_loc_to_loc_seq 
      combine_two_seq base sorted_word_seq in 
      
  (* Insert each (word, location_sequence) pair into a map *)
    let map_ref = ref(DMap.empty) in 
    let _ = S.iter (fun x -> let w, loc = x in 
      map_ref := DMap.add w loc !map_ref) final_word_seq 
    in 
    !map_ref
    (* let turn_pair_to_map (x: (string * location S.t)): doc_loc_index = 
      let word, loc = x in DMap.singleton word loc
    in 
    let merge_two_map (xs:doc_loc_index) (ys: doc_loc_index) = 
      let merge_aux k x y = 
        match x, y with 
        | Some x, Some y -> failwith "Should not have duplicate words"
        | Some _, None -> x
        | None, _ -> y
      in 
      DMap.merge merge_aux xs ys 
    in 
    S.map_reduce turn_pair_to_map merge_two_map DMap.empty final_word_seq *)



(* Unit Testing *)
(* let word_seq = S.seq_of_array (Array.of_list[("a", (1,2,3)); ("b", (2,3,4))])
let comparator (x: (string * location)) (y: (string * location)): int = 
  let (s1, _) = x in 
  let (s2, _) = y in 
  String.compare s1 s2
let sorted_word_seq = quicksort word_seq compare 
let _ = S.iter (fun x -> 
let a, (b, c, d) = x in 
let _ = print_string "(" in 
let _ = print_string a in 
let _ = print_string "(" in 
let _ = print_int b in 
let _ = print_int c in 
let _ = print_int d in 
print_string "))"
) sorted_word_seq *)

(* let int_comparator x y = x-y
let res = quicksort (S.seq_of_array (Array.of_list [1;4;2;3;5])) int_comparator
let _ = S.iter (fun x->print_int x) res *)

(* let leq x = x <=3
let (after_f1, after_f2) = 
  filter (S.seq_of_array (Array.of_list [1;4;2;3;5])) leq
let _ = S.iter (fun x->print_int x) after_f1
let _ = print_string "divider"
let _ = S.iter (fun x->print_int x) after_f2 *)


let search (dex: doc_loc_index) (query: string list) : result list =
   failwith "implement me"
   
(* debugging framework 
let tolist s = Array.to_list (S.array_of_seq s)
let docs = S.seq_of_array (Util.load_documents "data/test_index_1000.txt")
let dex = make_index docs
let rs = search dex ["for";"a";"year"]
let _ = List.iter (show_result docs) rs
*)