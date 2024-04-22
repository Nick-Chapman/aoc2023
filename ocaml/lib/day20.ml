
open Printf

module M = Map.Make(String)

exception Panic of string
let panic s = raise (Panic s)

let check : int -> int -> int =
  fun a b ->
  if a = b then a else panic (sprintf "check failed: %d not the same as %d" a b)

let lines : (string -> string list) =
  fun filename ->
  In_channel.with_open_text filename In_channel.input_lines

type name = string
type ty = FF | Conj | Broadcaster
type line = ty * name * name list
type spec0 = Spec0 of line list

let show_ty : ty -> string =
  function
  | FF -> "%"
  | Conj -> "&"
  | Broadcaster -> "(BROADCASTER)"

let parse_lhs : string -> (ty * string) =
  fun s ->
  let tail() = String.sub s 1 (String.length s - 1) in
  match s.[0] with
  | '%' -> (FF,tail())
  | '&' -> (Conj,tail())
  | _ -> (Broadcaster,s)

let trim_trailing_comma : string -> string =
  fun s ->
  match s.[String.length s - 1] with
  | ',' -> String.sub s 0 (String.length s - 1)
  | _ -> panic "trim_trailing_comma: expected comma"

let trim_rhs : string list -> string list =
  fun xs ->
  match List.rev xs with
  | [] -> []
  | x::xs -> List.rev (x :: List.map trim_trailing_comma xs)

let parse_line : string -> line =
  fun s ->
  match String.split_on_char ' ' s with
  | lhs::"->"::rhs ->
     let (ty,name) = parse_lhs lhs in
     let names = trim_rhs rhs in
     (ty,name,names)
  | _ ->
     panic "parse_line: expected ->"

let parse_spec : string -> spec0 =
  fun filename ->
  let xs = lines filename in
  Spec0 (List.map parse_line xs)

let show_list : string list -> string =
  fun xs ->
  sprintf "%s" (String.concat ", " xs)

let print_line : line -> unit =
  fun (ty,name,names) ->
  printf "%s%s -> %s\n" (show_ty ty) name (show_list names)

let _print_spec0 : spec0 -> unit =
  fun (Spec0 xs) ->
  List.iter print_line xs

type spec = Spec of line M.t

let make_spec : spec0 -> spec =
  fun (Spec0 lines) ->
  let f ((_,name,_) as line) = (name,line) in
  let m = M.of_list (List.map f lines) in
  Spec m

let lookup_spec_name : spec -> name -> line =
  fun (Spec m) name ->
  match M.find_opt name m with
  | None -> panic (sprintf "lookup_spec_name: %s" name)
  | Some line -> line

let lookup_rhss : spec -> name -> name list =
  fun spec name ->
  let (_,_,xs) = lookup_spec_name spec name in
  xs

type event = EVENT of name * bool * name

let _show_event : event -> string =
  function EVENT (source,v,target) ->
  let vStr = if v then "high" else "low" in
  sprintf "%s -%s-> %s" source vStr target

type conj_state = CS of bool M.t

let init_cs : name list -> conj_state =
  fun xs -> CS (M.of_list (List.map (fun x -> (x,false)) xs))

let update_cs : conj_state -> name -> bool -> conj_state =
  fun (CS m) name b ->
  match M.find_opt name m with
  | None -> panic (sprintf "update_cs: missing %s" name)
  | Some _ -> CS (M.add name b m)

let collapse_cs : conj_state -> bool =
  fun (CS m) ->
  let rec loop = function
    | [] -> false (* all are high *)
    | (_,b)::xs -> if b then loop xs else true
  in
  loop (M.to_list m)

type node = NODE_FF of bool | NODE_Conj of conj_state | NODE_Broadcaster

type state = STATE of node M.t * int * int

let make_sources : line list -> (name -> name list) =
  let rec loop m = function
    | [] -> m
    | (_,source,dests)::lines ->
       let fDest m dest =
         let xs =
           match M.find_opt dest m with
           | None -> []
           | Some xs -> xs
         in
         M.add dest (source::xs) m
       in
       let m = List.fold_left fDest m dests in
       loop m lines
  in
  fun lines ->
  let m = loop M.empty lines in
  let f dest =
    match M.find_opt dest m with
    | None -> []
    | Some xs -> xs
  in
  f

let make_init_state : spec0 -> state =
  fun (Spec0 lines) ->
  let sources = make_sources lines in
  let f (ty,name,_) =
    let node = match ty with
      | FF -> NODE_FF false
      | Conj -> NODE_Conj (init_cs (sources name))
      | Broadcaster -> NODE_Broadcaster
    in
    (name,node)
  in
  let m = M.of_list (List.map f lines) in
  STATE (m,0,0)

let lookup_state_node : state -> name -> node =
  fun (STATE (m,_,_)) name ->
  match M.find_opt name m with
  | None -> panic (sprintf "lookup_state_node: %s" name)
  | Some node -> node

let set_state_node : state -> name -> node -> state =
  fun (STATE (m,l,h)) name node ->
  STATE (M.add name node m, l, h)


let count_pulse : state -> event -> state =
  fun (STATE(m,l,h)) (EVENT(_,b,_)) ->
  let (l,h) = if b then (l,h+1) else (l+1,h) in
  STATE (m,l,h)

let step : spec -> state * event -> state * event list =
  fun spec (s,e) ->
  (*printf "%s\n" (_show_event e);*)
  let s = count_pulse s e in
  let EVENT(source,bool,target) = e in
  match target with
  | "rx" ->
     (*printf "(rx: %b)\n" bool;*)
     (s,[])
  | "output" ->
     (*printf "(OUTPUT: %b)\n" bool;*)
     (s,[])
  | _ ->
     let rhss = lookup_rhss spec target in
     let n = lookup_state_node s target in
     match n with

     | NODE_Broadcaster ->
        let events = List.map (fun x -> EVENT (target,bool,x)) rhss in
        (s,events)

     | NODE_FF v0 ->
        if bool
        then (s,[])
        else
          let v = not v0 in
          let s = set_state_node s target (NODE_FF v) in
          let events = List.map (fun x -> EVENT (target,v,x)) rhss in
          (s,events)

     | NODE_Conj cs ->
        let cs = update_cs cs source bool in
        let v = collapse_cs cs in
        let s = set_state_node s target (NODE_Conj cs) in
        let events = List.map (fun x -> EVENT (target,v,x)) rhss in
        (s,events)

let sim_one_button_press : spec -> state -> state =
  fun spec ->
  let rec loop s =
    function
    | [] -> s
    | e1::es ->
       let (s,esMore) = step spec (s,e1) in
       loop s (es @ esMore)
  in
  let event0 = EVENT ("button",false,"broadcaster") in
  fun s ->
  loop s [event0]

let sim_many_button_presses : spec -> int -> state -> state =
  fun spec ->
  let rec loop n s =
    if n == 0 then s else
      (*let () = printf "----------\n" in
      let () = printf "(PRESS %d)\n" n in*)
      let s = sim_one_button_press spec s in
      loop (n-1) s
  in
  loop

let part1 : string -> int =
  fun filename ->
  let spec0 = parse_spec filename in
  (*let () = _print_spec0 spec0 in*)
  let s0 = make_init_state spec0 in
  let spec = make_spec spec0 in
  let STATE(_,l,h) = sim_many_button_presses spec 1000 s0 in
  (*printf "final-pulse-count: low=%d, high=%d\n" l h;*)
  let res = l * h in
  res


let main() =
  printf "day20, part1 (samA): %d\n" (check 32000000 (part1 "../input/day20-sample1.input"));
  printf "day20, part1 (samB): %d\n" (check 11687500 (part1 "../input/day20-sample2.input"));
  printf "day20, part1 %d\n" (check 821985143 (part1 "../input/day20.input"));
  ()
