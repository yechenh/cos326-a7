(* Insert each (word, location_sequence) pair into a map *)
    (* Method One: Insert each (word, location_sequence) one by one. 
       Time Complexity (N is the number of such pairs): 
       log1 + log2 + .. + logN = log(N!) (for big N, log(N!) ~ NlogN)
    *)
    (* let map_ref = ref(DMap.empty) in 
    let _ = S.iter (fun x -> let w, loc = x in 
      map_ref := DMap.add w loc !map_ref) final_word_seq 
    in 
    !map_ref *)

    (* Method Two: Using reduce function to construct the map.
       Time Complexity (N is the number of such pairs):
       1 + 2log2 + 4log4 + 8log8 + ... + (N/2)log(N/2). Note that ilog(i) comes 
       from combining two map both containing i elements. This time complexity
       can be rewritten into the form 
       T = sum_{i=0}^{(log_2 N)-1} ((2^i)*log(2^i))
         ~ sum_{i=0}^{(log_2 N)-1} (i * 2^i)
       T/2 = (log2) * sum_{i=0}^{(log_2 N)-1} (i * 2^(i-1))
           ~ sum_{i=-1}^{(log_2 N)-2} ((i+1) * 2^i)
       T-T/2 = T/2 ~ sum_{i=0}^{(log_2 N)-2} (-1 * 2^i) + (N/2)*((log_2 N)-1)
             = （-1）* (1-2^((log_2 N)-1))/(1-2) + (N/2)*((log_2 N)-1)
             = (1- N/2) + (N/2)*((log_2 N)-1)
             ~ (N/2)*((log_2 N)-2) 
       T ~ N(log_2 N)) - 2N 
    *)

    (* let filter (s: 'a S.t) (leq: 'a -> bool): ('a S.t * 'a S.t) = 
  let element_to_pair (input: 'a): ('a S.t * 'a S.t) = 
    if leq input then (S.singleton input, S.empty ()) else 
      (S.empty (), S.singleton input)
  in 
  let combine_two_pairs p1 p2 = 
    let fst_p1, snd_p1 = p1 in 
    let fst_p2, snd_p2 = p2 in 
    (S.append fst_p1 fst_p2, S.append snd_p1 snd_p2) 
  in 
  S.map_reduce element_to_pair combine_two_pairs (S.empty (), S.empty ()) s *)


  (* let reduced = S.map_reduce join_map join_reduce (S.empty ()) sorted in 
  let fill_zero_map (x: (int * int)): (int * int) S.t = 
    S.singleton x 
  in 
  let fill_zero_reduce (x: (int*int) S.t) (y: (int*int) S.t): (int*int) S.t =  
    if S.length x = 0 then y else 
    if S.length y = 0 then x else
    let (_, x_index) = S.nth x ((S.length x) - 1) in 
    let (_, y_index) = S.nth y 0 in 
    S.append (S.append x (S.repeat (0, 0) (y_index - x_index - 1))) y 
  in 
  let filled_zero = S.map_reduce fill_zero_map 
    fill_zero_reduce (S.empty ()) reduced 
  in  *)




  (* let filter (s: 'a S.t) (leq: 'a -> bool): 'a S.t S.t = 
    let aux_smaller x = if leq x then S.singleton x else S. empty () in 
    let aux_bigger x = if leq x then S.empty () else S.singleton x in 
    S.map (S.flatten) (S.cons (S.map aux_smaller s) 
      (S.singleton (S.map aux_bigger s)))
  
  let quicksort (s: 'a S.t) (compare:'a -> 'a -> int): 'a S.t = 
    let rec quicksort_aux (s: 'a S.t): 'a S.t = 
      if S.length s <= 1 then s
      else 
      let pivot = S.nth s 0 in 
      let leq x = if compare x pivot <= 0 then true else false in 
      let (_, to_be_filtered) = S.split s 1 in 
      let two_sub_sequences = filter to_be_filtered leq in
      let sorted_two_subsequences = S.map quicksort_aux two_sub_sequences in 
      S.append (S.append (S.nth sorted_two_subsequences 0) (S.singleton pivot))
        (S.nth sorted_two_subsequences 1)
    in 
    quicksort_aux s
  
  let make_onedim_grid (s: (int*int) S.t) (max: int) : int S.t = 
    if S.length s = 0 then S.repeat 0 max else
    let comparator (x: int*int) (y: int*int): int =
      let _, i1 = x in 
      let _, i2 = y in 
      i1-i2 
    in 
    let sorted = quicksort s comparator in 
    let join_map (x: (int*int)): (int*int) S.t = S.singleton x in 
    let join_reduce (b: (int * int) S.t) (x: (int*int) S.t) = 
      if S.length b = 0 then x else
      if S.length x = 0 then b else
      let (b_last_pop, b_last_index) = S.nth b ((S.length b) - 1) in 
      let (x_first_pop, x_first_index) = S.nth x 0 in 
      if b_last_index = x_first_index then 
        let first, _ = S.split b ((S.length b) - 1) in 
        let _, last = S.split x 1 in 
        S.append first (S.cons (b_last_pop + x_first_pop, b_last_index) last)
      else S.append
      (S.append b (S.repeat (0, 0) (x_first_index - b_last_index - 1))) x
    in 
    let filled_zero = S.map_reduce join_map join_reduce (S.empty ()) sorted in 
    let (_, first_index) = S.nth filled_zero 0 in 
    let (_, last_index) = S.nth filled_zero ((S.length filled_zero) - 1) in
    let filled_zero = S.append 
      (S.append (S.repeat (0, 0) first_index) filled_zero) 
      (S.repeat (0, 0) (max - last_index - 1))
    in 
    S.map (fun x -> let a, _ = x in a) filled_zero  *)


    let precompute (groups: group S.t) (us_area: area) (rows,cols) : int S.t S.t = 
      (* this algorithm has two parts: 
         (1) create a grid where position (r,c) has the population
             living at those coordinates
         (2) from that, create a grid where at position (r,c) you
             have the total population living south of r and west of c.
       *)
      let group_with_r_c = S.map (fun x ->
        let {pop=pop; lat=lat; lon=lon} = x in 
        let r,c = rowcol_of_latlon us_area (rows,cols) (lat,lon) in 
        (pop, r*cols + c)) groups 
      in 
      let onedim = make_onedim_grid group_with_r_c (rows * cols) in 
      let twodim = create_2d_matrix (rows, cols) onedim in
      let prefix_sum x = S.scan (+) 0 x in 
      let cols_summed = S.map prefix_sum twodim in 
      let add_up_two_rows x y = 
        let zipped = S.zip (x, y) in 
        S.map (fun x -> let a,b=x in a+b) zipped 
      in 
      (* let reversed_cols_summed = reverse cols_summed in  *)
      let _ = S.iter (fun x -> S.iter (fun y -> print_int y; print_string " ") x; print_string "\n") twodim in 
    
      let reversed_cols_summed = cols_summed in 
      let reversed_rows_summed = S.scan add_up_two_rows 
        (S.repeat 0 cols) reversed_cols_summed
      in 
      reversed_rows_summed
      (* reverse reversed_rows_summed *)