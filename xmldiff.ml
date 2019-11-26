open Xml

module SMap = Map.Make(String)

module T : sig
  type t_attr_diff =
    | RightOnly of string
    | LeftOnly of string
    | Same of string
    | Conflicting of string*string

  type t_diff =
    | Mismatch of xml list*xml list
    | MatchedNode of string*t_attr_diff SMap.t*t_diff_list*bool*int
    | MatchedLeaf of string

  and t_diff_list = private t_diff list

  val empty : t_diff_list
  val cons : t_diff -> t_diff_list -> t_diff_list
  val print : t_diff -> unit
end = struct
  type t_attr_diff =
    | RightOnly of string
    | LeftOnly of string
    | Same of string
    | Conflicting of string*string

  type t_diff =
    | Mismatch of xml list*xml list
    | MatchedNode of string*t_attr_diff SMap.t*t_diff_list*bool*int
    | MatchedLeaf of string

  and t_diff_list = t_diff list

  let empty : t_diff_list = []

  let cons (d:t_diff) (dlst:t_diff_list) : t_diff_list =
    match d, dlst with
    | Mismatch (x1,x2), Mismatch (y1,y2)::tl -> Mismatch (x1@y1,x2@y2)::tl
    | _, _ -> d::dlst

  let indent (ind:int) : unit = print_string (String.make (ind*2) ' ')

  let rec attr_str = function
    | [] -> ""
    | (name,value)::tl -> " " ^ name ^ "=\"" ^ value ^ "\"" ^ attr_str tl

  let rec print_xml (ind:int) : xml -> unit = function
    | Element (name,attr,lst) ->
      begin
        indent ind; print_endline ("&lt;" ^ name ^ attr_str attr ^ "&gt;");
        List.iter (print_xml (ind+1)) lst;
        indent ind; print_endline ("<&lt;" ^ name ^ "&gt;")
      end
    | PCData s -> indent ind; print_endline s

  let attr_diff_str map =
    let aux name value str =
      match value with
      | RightOnly value -> str ^ " <span style=\"color:blue\">" ^ name ^ "=\"" ^ value ^ "\"</span>"
      | LeftOnly value -> str ^ " <span style=\"color:red\">" ^ name ^ "=\"" ^ value ^ "\"</span>"
      | Same value -> str ^ " " ^ name ^ "=\"" ^ value ^ "\""
      | Conflicting (value1,value2) ->
        str
        ^ " <span style=\"color:red\">" ^ name ^ "=\"" ^ value1 ^ "\"</span>"
        ^ " <span style=\"color:blue\">" ^ name ^ "=\"" ^ value2 ^ "\"</span>"
    in
    SMap.fold aux map ""
    
  let print (d:t_diff) : unit =
    let rec loop ind = function
      | MatchedNode (name,attr,lst,_,_) ->
        begin
          indent ind; print_endline ("&lt;" ^ name ^ attr_diff_str attr ^ "&gt;");
          List.iter (loop (ind+1)) lst;
          indent ind; print_endline ("&lt;/" ^ name ^ "&gt;")
        end
      | MatchedLeaf s -> ( indent ind; print_endline s )
      | Mismatch (lst1,lst2) ->
        begin
          print_string "<span style=\"color:red;\">";
          List.iter (print_xml ind) lst1;
          print_string "</span>";
          print_string "<span style=\"color:blue;\">";
          List.iter (print_xml ind) lst2;
          print_string "</span>";
        end
    in
    print_endline "<?xml version=\"1.0\" encoding=\"UTF-8\" ?>";
    print_endline "<!DOCTYPE html>";
    print_endline "<html xmlns=\"http://www.w3.org/1999/xhtml\" xml:lang=\"fr\" lang=\"fr\" dir=\"ltr\">";
    print_endline "<head>";
    print_endline "  <title>Exemple</title>";
    print_endline "</head>";
    print_endline "<body>";
    print_endline "<pre>";
    loop 1 d;
    print_endline "</pre>";
    print_endline "</body>";
    print_endline "</html>"
end

type t_dir = Right | Bottom | Diag | Start

let max3 (x,_:int*_) (y,_:int*_) (z,_:int*_) (i:int) : int*t_dir =
  let z = z+i in
  if x >= y then
    if x >= z then
      (x,Bottom) (* x >= y && x >= z *) 
    else
      (z,Diag) (* x >= y && z > x *) 
  else if y >= z then
    (y,Right) (* y > x && y >= z *) 
  else 
    (z,Diag) (* y > x && z > y *) 

let max2 (x,_:int*_) (y,_:int*_) : int*t_dir =
  if x >= y then (x,Bottom) else (y,Right)

(*
let pp out = function
  | PCData _ -> Printf.fprintf out "PCData"
  | Element (name,_,_) -> Printf.fprintf out "%s" name
*)

let diff_attr attr1 attr2 =
  let aux1 map (name,value) = SMap.add name (T.LeftOnly value) map in
  let aux2 map (name,value2) =
    match SMap.find_opt name map with
    | None -> SMap.add name (T.RightOnly value2) map
    | Some T.LeftOnly value1 ->
      if String.equal value1 value2 then
        SMap.add name (T.Same value1) map
      else
        SMap.add name (T.Conflicting (value1,value2)) map
    | _ -> assert false (*FIXME*)
  in
  let map = List.fold_left aux1 SMap.empty attr1 in
  List.fold_left aux2 map attr2

let rec diff (xml1:xml) (xml2:xml) : T.t_diff =
  match xml1, xml2 with
  | Element (name1,attr1,children1), Element (name2,attr2,children2) when String.equal name1 name2 ->
    let (lst,perfect,size) = diff_seq children1 children2 in
    T.MatchedNode(name1,diff_attr attr1 attr2,lst,perfect,size+1)
  | PCData s1, PCData s2 when String.equal s1 s2 ->
    T.MatchedLeaf s1
  | _, _ -> T.Mismatch([xml1],[xml2])

and diff_seq (lst1:xml list) (lst2:xml list) : T.t_diff_list*bool*int = 
  match lst1, lst2 with
  | [], [] -> (T.empty,true,0)
  | [], _ | _, [] -> (T.cons (Mismatch(lst1,lst2)) T.empty,false,0)
  | hd1::tl1, hd2::tl2 ->
    begin match diff hd1 hd2 with
      | T.MatchedLeaf _ as d ->
        let (lst,perfect,size) = diff_seq tl1 tl2 in
        (T.cons d lst,perfect,size+1)
      | T.MatchedNode (_,_,_,true,size1) as d ->
        let (lst,perfect,size2) = diff_seq tl1 tl2 in
        (T.cons d lst,perfect,size1+size2)
      | d ->
        let (lst,size) = dp (Array.of_list lst1) (Array.of_list lst2) d in
        (lst,false,size)
    end

and dp (t1:xml array) (t2:xml array) d : T.t_diff_list*int =
  let s1 = Array.length t1 in
  let s2 = Array.length t2 in
  let arr: T.t_diff array array =
    Array.init s1 (fun i ->
        Array.init s2 (fun j -> if i=0 && j=0 then d else diff t1.(i) t2.(j)))
  in
  let dist : (int*t_dir) array array = Array.make_matrix (s1+1) (s2+1) (0,Start) in
  let rec get_optimal_path (i:int) (j:int) (accu:T.t_diff_list) : T.t_diff_list =
(*     assert(i>=0); assert(j>=0); assert(i<=s1); assert(j<=s2); *)
    match dist.(i).(j) with
    | _, Start -> accu
    | _, Diag -> get_optimal_path (i-1) (j-1) (T.cons arr.(i-1).(j-1) accu)
    | _, Right -> get_optimal_path i (j-1) (T.cons (T.Mismatch([],[t2.(j-1)])) accu)
    | _, Bottom -> get_optimal_path (i-1) j (T.cons (T.Mismatch ([t1.(i-1)],[])) accu)
  in
  for i=0 to s1 do
    for j=0 to s2 do
      if i=0 && j=0 then ()
      else if i=0 then
        dist.(i).(j) <- (fst dist.(i).(j-1), Right)
      else if j=0 then
        dist.(i).(j) <- (fst dist.(i-1).(j), Bottom)
      else
        match arr.(i-1).(j-1) with
        | MatchedNode (_,_,_,_,sz) -> dist.(i).(j) <- max3 dist.(i-1).(j) dist.(i).(j-1) dist.(i).(j) sz
        | MatchedLeaf _ -> dist.(i).(j) <- max3 dist.(i-1).(j) dist.(i).(j-1) dist.(i).(j) 1
        | Mismatch _ -> dist.(i).(j) <- max2 dist.(i-1).(j) dist.(i).(j-1)
    done
  done;
(*
  Printf.eprintf "--- t1\n";
  for i=0 to s1-1 do
    Printf.eprintf "%i --> %a\n" i pp t1.(i)
  done;
  Printf.eprintf "--- t2\n";
  for i=0 to s2-1 do
    Printf.eprintf "%i --> %a\n" i pp t2.(i)
  done;
  Printf.eprintf "--- arr\n";
  for i=0 to s1-1 do
    for j=0 to s2-1 do
      match arr.(i).(j) with
      | MatchedNode (_,_,_,sz) -> Printf.eprintf "[%i,%i] Node sz=%i\n" i j sz
      | MatchedLeaf _ -> Printf.eprintf "[%i,%i] Leaf\n" i j
      | Mismatch _ -> Printf.eprintf "[%i,%i] Mismatch\n" i j
    done
  done;
  Printf.eprintf "--- dist\n";
  for i=0 to s1 do
    for j=0 to s2 do
      match dist.(i).(j) with
      | _, Start -> Printf.eprintf "[%i,%i] Start\n" i j
      | _, Diag -> Printf.eprintf "[%i,%i] Diag\n" i j
      | _, Right -> Printf.eprintf "[%i,%i] Right\n" i j
      | _, Bottom -> Printf.eprintf "[%i,%i] Bottom\n" i j
    done
  done;
*)
  (get_optimal_path s1 s2 T.empty, fst dist.(s1).(s2))

let _ =
  if Array.length Sys.argv = 3 then
    let xml1 = parse_file Sys.argv.(1) in
    let xml2 = parse_file Sys.argv.(2) in
    T.print (diff xml1 xml2)
  else
    failwith "Expecting 2 arguments"
