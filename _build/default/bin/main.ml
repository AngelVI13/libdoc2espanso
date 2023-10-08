type keyword = { name : string; docs : string } (* ; args: string list} *)

let find_node_with_tag node search_tag =
  List.find
    (function
      | n ->
          let tag = try Xml.tag n with _ -> "" in
          tag = search_tag)
    (Xml.children node)

let get_node_text node = 
    let children = Xml.children node in
    match children with 
    | [] -> ""
    | hd::_ -> Xml.pcdata hd

let parse_node node =
  let name = Xml.attrib node "name" in
  let docsNode = find_node_with_tag node "doc" in

  print_endline "------------";
  let docs = get_node_text docsNode in
  (* {name=name; docs=""; args=[]} *)
  { name; docs }

let rec get_keywords_aux acc node =
  match node with
  | [] -> acc
  | hd :: tl ->
      let tag = try Xml.tag hd with _ -> "unknown" in
      let acc =
        match tag with
        | "kw" -> parse_node hd :: acc
        | "unknown" -> acc
        | _ -> acc @ get_keywords_aux [] (Xml.children hd)
      in

      get_keywords_aux acc tl

let get_keywords xml = get_keywords_aux [] @@ Xml.children xml

let () =
  let xml_path = "./keywords.xml" in
  let xml = Xml.parse_file xml_path in
  let kwds = get_keywords xml in
  List.iter
    (function el -> print_endline @@ Printf.sprintf "%s = %s" el.name el.docs)
    kwds;
  print_int @@ List.length kwds;
  print_newline ()
