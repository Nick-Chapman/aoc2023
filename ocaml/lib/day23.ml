
open Printf

exception Panic of string
let panic s = raise (Panic s)

let check : int -> int -> int =
  fun a b ->
  if a = b then a else panic (sprintf "check failed: %d not the same as %d" a b)

let upto : int -> int -> int list =
  fun i j ->
  let rec loop acc i = if i > j then List.rev acc else loop (i::acc) (i+1) in
  loop [] i

let explode : string -> char list =
  fun s ->
  List.map (fun i -> s.[i]) (upto 0 (String.length s - 1))

let lines : (string -> string list) =
  fun filename ->
  In_channel.with_open_text filename In_channel.input_lines

type dir = U | D | L | R
type tile = Path | Forest | Slope of dir
type pos = int * int
type world = pos * (pos -> tile)

let show_dir : dir -> string = function
  | L -> "<"
  | R -> ">"
  | U -> "^"
  | D -> "v"

let show_tile : tile -> string = function
  | Path -> "."
  | Forest -> "#"
  | Slope dir -> show_dir dir

let _print_world : world -> unit =
  fun ((w,h),f) ->
  List.iter (printf "%s\n")
    (List.map (fun y ->
         String.concat ""
           (List.map (fun x -> show_tile (f (x,y)))
              (upto 0 (w-1)))
       ) (upto 0 (h-1)))

let parse_tile : char -> tile = function
  | '.' -> Path
  | '#' -> Forest
  | '<' -> Slope L
  | '>' -> Slope R
  | '^' -> Slope U
  | 'v' -> Slope D
  | c -> panic (sprintf "parse_tile:%c" c)

let parse : string -> world =
  fun filename ->
  let xss = List.map (fun s -> List.map parse_tile (explode s)) (lines filename) in
  let w = List.length (List.hd xss) in
  let h = List.length xss in
  let a = Array.of_list (List.map Array.of_list xss) in
  let f (x,y) = a.(y).(x) in
  ((w,h),f)

let step : pos -> dir -> pos =
  fun (x,y) ->
  function
  | L -> (x-1,y)
  | R -> (x+1,y)
  | U -> (x,y-1)
  | D -> (x,y+1)

let show_pos (x,y) = sprintf "(%d,%d)" x y

let opposite : dir -> dir = function
  | L -> R
  | R -> L
  | U -> D
  | D -> U

type edge = int*pos

let show_edge (n,pos) = sprintf "--[%d]-> %s" n (show_pos pos)

type graph = (pos * edge list) list

let _print_graph : graph -> unit =
  fun g ->
  List.iter (fun (p,es) ->
      printf "%s\n" (show_pos p);
      List.iter (fun e ->
          printf "%s\n" (show_edge e)
      ) es
    ) g

let is_source : pos -> graph -> bool =
  fun pos ->
  let rec loop = function
    | [] -> false
    | (p,_)::g -> if (p = pos) then true else loop g
  in
  loop

let goal : world -> pos  =
  fun ((w,h),_) ->
  (w-2,h-1)

let explore_world : world -> graph =
  fun (((w,h),f) as world) ->
  let goal = goal world in
  let out_of_bounds (x,y) = (x < 0) || (x >= w) || (y < 0) || (y >= h) in

  let rec wanderK : int -> dir -> pos -> (unit -> int*pos) -> int*pos =
    fun n inDir pos k ->
    if pos = goal then n-1,goal else
    if out_of_bounds pos then k () else
    let tile = f pos in
    match tile with
    | Forest -> k ()
    | Slope dir ->
       if dir == inDir
       then (n, step pos dir)
       else panic "unexpected slippy"
    | Path ->
       let rec allways = function
         | [] -> k ()
         | dir::dirs ->
            if dir == opposite inDir
            then allways dirs
            else wanderK (n+1) dir (step pos dir) (fun () -> allways dirs)
       in
       allways [L;R;U;D]
  in

  let wander : int -> dir -> pos -> edge =  (* until we reach an intersection *)
    fun n dir pos ->
    wanderK n dir pos (fun () -> panic "wander-got-nowhere")
  in

  let stride : pos -> edge list = (* from one intersection to more intersections *)
    fun intersection ->
    let rec loop = function
      | [] -> []
      | dir::dirs ->
         let pos = step intersection dir in
         let tile = f pos in
         match tile with
         | Path -> panic "stride:Path"
         | Forest -> loop dirs
         | Slope d ->
            if dir = d
            then
              let edge = wander 3 dir (step pos dir) in
              edge :: loop dirs
            else
              loop dirs
    in
    loop [L;R;U;D]
  in

  let rec scout : graph -> pos list -> graph = (* scout out the entire graph *)
    fun acc xs ->
    match xs with
    | [] -> List.rev acc
    | i::xs ->
       if i = goal then scout acc xs else
       if is_source i acc then scout acc xs else
       let es = stride i in
       let xs = xs @ List.map snd es in
       scout ((i,es)::acc) xs
  in

  let start = (1,0) in
  let (_,i1) as e1 = wander 1 D start in
  scout [(start,[e1])] [i1]


module Pos = struct
  type t = pos
  let compare : t -> t -> int =
    fun (x1,y1) (x2,y2) ->
    let v = Int.compare x1 x2 in
    if v = 0 then Int.compare y1 y2 else v
end

module M = Map.Make(Pos)

module Set = Set.Make(Pos)

let longest_path : graph -> pos -> pos -> int =
  fun g start goal ->
  let m : edge list M.t = M.of_list g in
  let look : pos -> edge list =
    fun pos ->
    match M.find_opt pos m with
    | None -> panic "look"
    | Some es -> es
  in
  let rec trav : best:int -> Set.t -> int -> pos -> (best:int -> int) -> int =
    fun ~best visited len pos k ->
    if pos = goal
    then
      if len<best
      then k ~best
      else
        (*let () = printf "NEW best=%d\n%!" len in*)
        k ~best:len
    else
      if Set.mem pos visited then k ~best else
        let visited = Set.add pos visited in
        let rec inner : best:int -> (best:int -> int) -> edge list -> int =
          fun ~best k ->
          function
          | [] -> k ~best
          | (n,dest)::es -> trav ~best visited (len+n) dest (fun ~best -> inner ~best k es)
        in
        inner ~best k (look pos)
  in
  trav ~best:0 Set.empty 0 start (fun ~best -> best)

let both_ways : graph -> graph =
  let rec loop : edge list M.t -> graph -> edge list M.t =
    fun acc ->
    function
    | [] -> acc
    | (src,es0)::gRest ->
       let rec inner : edge list M.t -> edge list -> edge list M.t =
         fun acc ->
         function
         | [] -> loop acc gRest
         | (len,dest)::es ->
            let old = match M.find_opt dest acc with | None -> [] | Some xs -> xs in
            let acc = M.add dest ((len,src)::old) acc in (* add reverse edge here *)
            inner acc es
       in
       inner acc es0
  in
  fun g ->
  let acc : edge list M.t = M.of_list g in
  M.to_list (loop acc g)

let part1 : world -> int =
  fun world ->
  let g = explore_world world in
  let goal = goal world in
  let start = (1,0) in
  longest_path g start goal

let part2 : world -> int =
  fun world ->
  let g = explore_world world in
  let g = both_ways g in
  let goal = goal world in
  let start = (1,0) in
  longest_path g start goal

let main() =
  let sam = parse "../input/day23-sample.input" in
  (*_print_world sam;*)
  (*_print_graph (explore_world sam);*)
  let inp = parse "../input/day23.input" in
  printf "day23, part1 (sam): %d\n%!" (check 94 (part1 sam));
  printf "day23, part1: %d\n%!" (check 2018 (part1 inp));
  printf "day23, part2 (sam): %d\n%!" (check 154 (part2 sam));
  printf "day23, part2: %d\n%!" (check 6406 (part2 inp));
  ()
