
let write_out lst =
  let filename = Sys.argv.(1) in
  let out_filename =
    let base_name = Filename.chop_extension filename in
    base_name ^ "_out" ^ Filename.extension filename
  in
  let oc = open_out out_filename in  
  List.iter (fun inner_list ->
    output_string oc (String.concat " " inner_list ^ "\n")
  ) lst;
  close_out oc
;;


module GString = Graph.Make(String);;

let data = Analyse.phase1 ();;

let (src,dst,edg) = data ;;

let graph = GString.create_graph_non_ponderer edg ;;

let min_way = GString.floyd_warshall_paths graph src dst

let _ = write_out min_way ;;