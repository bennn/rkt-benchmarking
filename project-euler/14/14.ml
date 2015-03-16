(**
 * The following iterative sequence is defined for the set of positive integers:
 *   n  n/2 (n is even)
 *   n  3n + 1 (n is odd)
 * Which starting number, under one million, produces the longest chain?
 *)
  (* let rec collatz (n:int) (acc:int list) : int list =  *)
  (*   if Hashtbl.mem chains n then *)
  (*     Hashtbl.find chains n *)
  (*   else begin *)
  (*     let r = begin match n with *)
  (*       | n when n = 1 -> *)
  (*         (\* done! *\) *)
  (*         List.fold_left (fun acc x -> x::acc) [n] acc *)
  (*         (\* List.rev_append acc [n] *\) *)
  (*       | n when n mod 2 = 0 -> *)
  (*         (\* even *\) *)
  (*         collatz (n / 2) (n::acc) *)
  (*       | _ -> *)
  (*         (\* odd *\) *)
  (*         collatz ((3 * n) + 1) (n::acc) end in *)
  (*     Hashtbl.add chains n r; *)
  (*     r end *)
  (* in *)
let collatz : int -> int = 
  let chain_lengths = Hashtbl.create 64 in
  let rec collatz_length (n:int) (acc:int) : int =
    match n with
      | n when n = 1 ->
          (* done! *)
        acc + 1
      | n when n mod 2 = 0 ->
          (* even *)
        collatz_length (n / 2) (1+acc)
      | _ ->
          (* odd *)
        collatz_length ((3 * n) + 1) (1+acc)
  in
  let memo_collatz_length (n:int) : int =
    if Hashtbl.mem chain_lengths n then
      Hashtbl.find chain_lengths n
    else begin 
      let r = collatz_length n 0 in
      Hashtbl.add chain_lengths n r;
      r end
  in
  (fun n -> memo_collatz_length n)
      
(* Return the number with the 
 * longest collatz chain starting at a number less than `n` *)
let longest_collatz (limit:int) : int =
  let compare_chains ((num, chain_length):int*int) (x:int) =
    let l = collatz x in
    Printf.printf "%d\n" x;
    if l > chain_length then
      (x, l)
    else 
      (num, chain_length)
  in 
  let start = 77670 in
  let step = 1 in
  let rec longest n acc = 
    if n >= limit then 
      fst acc 
    else
      let r = compare_chains acc n in
      longest (n+step) r
  in
  longest start (0, 0)
  (* fst (fold_while compare_chains ((>) n) (0, 0) odds) *)
