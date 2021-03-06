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
  (print_int i; print_string "/"; print_int b; print_string ": "; print_string excerpt; print_string "\n")


(* These methods each compute an inverted index of the specified type 
 * for the contents of a file. The filename is the given string. The
 * result is the computed inverted index. *)
(* use PSeq.map_reduce to construct your index in parallel *)
(* use DMap to create a map from words to a {D,DL}Set of document ids *)
(* Some handy functions: 
 *  Util.split_words converts document contents to a list of words 
 *  String.lowercase_ascii converts a word uniformly to lower case *)

(**** START OF SOLUTION ****)

  let filter (s: (string * location) S.t) (pivot: string): 
    ((string * location) S.t S.t * location S.t) = 
    let aux_smaller x = let word, _ = x in if String.compare word pivot < 0 
      then S.singleton x else S. empty () in 
    let aux_bigger x = let word, _ = x in if String.compare word pivot > 0 
      then S.singleton x else S.empty () in 
    let subsequences = S.map S.flatten (S.cons (S.map aux_smaller s) 
      (S.singleton (S.map aux_bigger s))) 
    in 
    let pivot_seq = S.flatten (S.map (fun x -> let word, loc = x in 
      if word = pivot then S.singleton loc else S.empty ()) s)
    in
    (subsequences, pivot_seq)
      
  let quicksort (s: (string * location) S.t): (string * location S.t) S.t = 
    let rec quicksort_aux (s: (string * location) S.t): 
      (string * location S.t) S.t = 
      if S.length s = 0 then S.empty () 
      else 
      let (pivot_word, pivot_loc) = S.nth s 0 in 
      let (_, to_be_filtered) = S.split s 1 in 
      let (two_sub_sequences, pivot_loc_seq) = 
        filter to_be_filtered pivot_word 
      in
      let sorted_two_subsequences = S.map quicksort_aux two_sub_sequences in 
      S.append (S.append (S.nth sorted_two_subsequences 0) 
        (S.singleton (pivot_word, S.cons pivot_loc pivot_loc_seq)))
        (S.nth sorted_two_subsequences 1)
    in 
    quicksort_aux s


let make_index (docs: Util.document S.t) : doc_loc_index =
   (* Process each document in parallel and turn each doc into a sequence of 
    (word, location sequence) *)
    let reverse tt = 
      let l = S.length tt in 
      S.tabulate (fun i -> S.nth tt (l-i-1)) (S.length tt)
    in 
    let docs = reverse docs in 
    let process_doc (doc: Util.document): (string * location) S.t = 
      let {Util.id=i';Util.title=t;Util.contents=cts} = doc in 
      let word_seq = S.seq_of_array (Array.of_list (Util.split_words cts)) in 
      
      let zero_to_n = S.tabulate (fun i -> i) (S.length word_seq) in 
      let zipped_word_seq = S.zip (word_seq, zero_to_n) in 

      let aux pair = 
        let (word, char_count), word_count = pair in 
        ((String.lowercase_ascii word), (i', word_count, char_count))
      in 
      S.map aux zipped_word_seq
    in 
    let word_seq_seq = S.map process_doc docs in 
    let word_seq = S.flatten word_seq_seq in
    let final_word_seq = quicksort word_seq in 
    let map_ref = ref(DMap.empty) in 
    let _ = S.iter (fun x -> let w, loc = x in 
      map_ref := DMap.add w loc !map_ref) final_word_seq 
    in 
    !map_ref



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


let search (dex: doc_loc_index) (query: string list) : result list =
  let query_seq =  S.seq_of_array (Array.of_list query) in 
  (* Turn each location into docID, word_start_position, word_end_position, char_start_position, char_end_position*)
  let map_aux (loc: location): (int * int * int * int * int) = 
    let a,b,c = loc in (a,b,b,c,c)
  in 
  let word_result_seq = S.map 
    (fun x -> match DMap.find_opt (String.lowercase_ascii x) dex with 
      | None -> S.empty ()
      | Some v -> S.map map_aux v) 
      query_seq
  in
  let union_of_two_loc_sequences (xs: (int * int * int * int * int) S.t)
    (ys: (int * int * int * int * int) S.t): (int * int * int * int * int) S.t =
    (* If it is the union with the base case then no need for the rest *)
    if S.length xs = 0 then ys 
    else if S.length ys = 0 then xs
    else
    let _, st, _, _, _ = S.nth xs 0 in 
    if st = 0 then ys else
    (* First, map each element in xs to be S.singleton (docID, 
       word_start_position, new_word_end_position, char_start_position, new_char_end_position) if there is a position in ys that immediately 
       follows the element in xs. Else, map it to be empty sequence.*)
    let map_xs (x: (int * int * int * int * int)): 
      (int * int * int * int * int) S.t = 
      let docID, word_st, word_end, char_st, char_end = x in 
      let go_through_ys_map (y: (int * int * int * int * int)): 
        (int * int * int * int * int) S.t = 
        let docID_y, word_st_y, word_end_y, char_st_y, char_end_y = y in
        if docID = docID_y && word_st_y = word_end + 1 then 
          S.singleton (docID_y, word_st, word_end_y, char_st, char_end_y) 
        else S.empty()
      in 
      (* Second, flatten the sequence of sequence, which will then be either 
         empty if there is nothing is ys that immedaitely follows the previous 
         word, or a singleton sequence containing that position.*)
      let temp = S.flatten (S.map go_through_ys_map ys) in
      temp
    in 
    S.flatten (S.map map_xs xs) 
  in 
  let base = S.singleton (0, 0, 0, 0, 0) in
  let seq = S.reduce union_of_two_loc_sequences base word_result_seq in 
  let result_seq = S.map 
    (fun x -> 
    let docID, word_st, word_end, char_st, char_end = x in 
    {res_docid = docID; res_begin = char_st; res_end = char_end}
    ) seq 
  in 
  Array.to_list (S.array_of_seq result_seq)
  
(* debugging framework  *)
(* let tolist s = Array.to_list (S.array_of_seq s) *)
(* let docs = S.seq_of_array (Util.load_documents "data/test_index_1000.txt") *)
(* let docs = S.seq_of_array (Util.load_documents "data/test_index_1.txt") *)
(* let docs = S.seq_of_array (Util.load_documents "data/test_index_special.txt") *)
(* let dex = make_index docs *)
(* let rs = search dex ["statement";"with"]
let rs = search dex ["one"; "one"]
let rs = search dex ["for"; "a"] *)
(* let rs = search dex ["for"; "a"; "year"]
let _ = List.iter (show_result docs) rs *)
