
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
