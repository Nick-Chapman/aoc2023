
open Printf

exception Panic of string
let panic s = raise (Panic s)

let lines : (string -> string list) =
  fun filename ->
  In_channel.with_open_text filename In_channel.input_lines

type name = string
type ty = FF | Conj | Broadcaster
type line = ty * name * name list
type spec = Spec of line list

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
  | _ -> panic "expected comma"

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
     panic "parse_line"

let parse_spec : string -> spec =
  fun filename ->
  let xs = lines filename in
  Spec (List.map parse_line xs)

let show_list : string list -> string =
  fun xs ->
  sprintf "%s" (String.concat ", " xs)

let print_line : line -> unit =
  fun (ty,name,names) ->
  printf "%s%s -> %s\n" (show_ty ty) name (show_list names)

let print_spec : spec -> unit =
  fun (Spec xs) ->
  List.iter print_line xs

let main() =
  let () = print_endline "Day 20!" in
  let _filename = "../input/day20.input" in
  let filename = "../input/day20-sample1.input" in
  let spec = parse_spec filename in
  let () = print_spec spec in
  ()
