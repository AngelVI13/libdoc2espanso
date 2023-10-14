type keyword = { name : string; docs : string; args : string list }
[@@deriving show]

let print_keywords lst =
  List.map show_keyword lst |> String.concat "\n" |> print_endline

let find_node_with_tag search_tag node =
  List.find
    (function
      | n ->
          let tag = try Xml.tag n with _ -> "" in
          tag = search_tag)
    (Xml.children node)

let get_node_text node =
  let children = Xml.children node in
  match children with [] -> "" | hd :: _ -> Xml.pcdata hd

let rec get_keywords_aux acc node search_tag =
  match node with
  | [] -> acc
  | hd :: tl ->
      let tag = try Xml.tag hd with _ -> "unknown" in
      let acc =
        match tag with
        | t when t = search_tag -> hd :: acc
        | "unknown" -> acc
        | _ -> acc @ get_keywords_aux [] (Xml.children hd) search_tag
      in

      get_keywords_aux acc tl search_tag

let get_keywords xml search_tag =
  get_keywords_aux [] (Xml.children xml) search_tag

let parse_node node =
  let name = Xml.attrib node "name" in
  let docs = find_node_with_tag "doc" node |> get_node_text in
  let args =
    get_keywords node "arg"
    |> List.map @@ find_node_with_tag "name"
    |> List.map get_node_text
  in
  { name; docs; args }

(*
let matchTemplate = "
  - trigger: \"%s\"
    form: \"%s\"
    ";;
*)

let rec format_args acc args =
    match args with 
    | [] -> String.concat "" acc
    | hd::tl -> format_args ((Printf.sprintf " %s=[[%s]]" hd hd)::acc) tl

let rec convert_kwds_to_espanso acc kwds = 
    match kwds with 
    | [] -> acc
    | hd::tl -> (
        let trigger = String.lowercase_ascii hd.name in
        let form = hd.name in
        let form = form ^ ( if List.length hd.args > 0 then " |" else "" ) in
        let form = form ^ (format_args [] hd.args) in
        let acc = (Printf.sprintf "\n  - trigger: \"%s\"\n    form: \"%s\"\n" trigger form)::acc in
        convert_kwds_to_espanso acc tl)


let () =
  let xml_path = "./keywords.xml" in
  let xml = Xml.parse_file xml_path in
  let kwds = get_keywords xml "kw" |> List.map parse_node in
  print_keywords kwds;
  let out = convert_kwds_to_espanso [] kwds in
  print_string @@ String.concat "\n" out
