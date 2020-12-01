open Query
open Sequence

type groupseq = Query.group Sequence.S.t

let get_groups input_file = (Parse.parse input_file Query.group_of_data) 
|> Array.of_list 
|> S.seq_of_array;;

let search_pop (groups:groupseq) = 
  let all_area = Query.encompassing_area groups in 
  Query.population_search groups all_area

let lookup_pop (groups:groupseq) = 
  let all_area = Query.encompassing_area groups in 
  Query.population_lookup (Query.precompute groups all_area (20,20)) (1,1,20,20)

let get_pop (groups:groupseq) = 
  let scanned_groups = S.scan (+) 0 (Query.get_pop groups) in
  S.nth scanned_groups ((S.length scanned_groups) - 1)

let test (): unit = 
  let all_groups = get_groups "data/CenPop2010.txt" in
  let pop_by_search = search_pop all_groups in
  let pop_by_scan = get_pop all_groups in
  let pop_by_precomp = lookup_pop all_groups in
  print_string (" Pop by scan: " ^ (string_of_int pop_by_scan) ^
  " Pop by search: " ^ (string_of_int pop_by_search) ^ 
  " Pop by lookup: " ^ (string_of_int pop_by_precomp) );;

test ();;