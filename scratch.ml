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