
let generate arr f =
 for i=0 to Array.length arr do
    arr.(i) <- arr.(i) |> f
 done

let filter lst f =
 let rec aux lst f acc =
  match lst with
  | [] -> List.rev acc
  | h::t when f h = true -> aux t f (h::acc)
  | _::t -> aux t f acc
 in aux lst f []

let transform lst f =
 let rec aux lst f acc =
  match lst with
  | [] -> List.rev acc
  | h::t -> aux t f ((h|>f) :: acc)
 in aux lst f []

let rec exists lst target =
 match lst with
 | [] -> false
 | h::t when h=target-> true
 | _::t-> exists t target

let rec drop lst amount =
 match lst with
 | [] -> []
 | any when amount<=0 -> any
 | _::t -> drop t (amount-1)

let take lst amount =
 let rec aux lst amount acc =
  match lst with
  | [] -> List.rev acc
  | h::t when amount>=0 -> aux t (amount-1) (h::acc)
  | _::t -> aux t (amount-1) (acc)
 in aux lst amount []

let unique lst =
 let rec aux lst acc =
  match lst with
  | [] -> List.rev acc
  | h::t when exists t h = true -> aux t acc
  | h::t -> aux t (h::acc)
 in aux lst []

let rec for_each lst f =
 match lst with
 | [] -> ()
 | h::t ->
 begin
    f h;
    for_each t f
 end

let replace lst f value =
 let rec aux lst f value acc =
  match lst with
  | [] -> List.rev acc
  | h::t when h |> f = true -> aux t f value (value::acc)
  | h::t -> aux t f value (h::acc)
 in aux lst f value []

let duplicate lst amount =
 let rec aux lst acc =
  match lst with
  | [] -> List.rev acc
  | head :: tail ->
   let rec duplicate_within acc value counter =
    if counter = amount
     then acc
      else duplicate_within (value :: acc) value (counter + 1)
  in
   aux tail (duplicate_within acc head 0)
 in
 aux lst []

let rec count lst f =
 let counter = ref 0 in
  match lst with
  | [] -> !counter
  | h::t -> if f h then
  begin
    counter := !counter + 1;
    count t f
  end
  else count t f

let accumulate lst op =
 let rec aux lst op value =
  match lst with
  | [] -> value
  | h::t -> aux t op (op value h)
 in aux lst op 0

let find_smallest lst =
 let rec aux lst smallest_value =
  match lst with
  | [] -> smallest_value
  | h::t ->
  aux t (if h < smallest_value
  then h
  else smallest_value)
 in aux lst 0

let find_largest lst =
 let rec aux lst largest_value =
  match lst with
  | [] -> largest_value
  | h::t ->
  aux t (if h < largest_value
  then largest_value
  else h)
 in aux lst 0

let sort lst =
 let rec aux lst acc =
  match lst with
  | [] -> List.rev acc
  | h:: t ->
  begin
    let res = t|>find_smallest in
    aux t (if res > h then h::acc else res::acc)
  end
 in aux lst []
