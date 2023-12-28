module G = Graph.Make(String);;

let write_out graph src dst  =
(* Création du nom de fichier *)
  let filename = Sys.argv.(1) in
  let out_filename =
    let base_name = Filename.chop_extension filename in
    base_name ^ "_out" ^ Filename.extension filename
  in

  let oc = open_out out_filename in

  let flow = G.max_flow graph src in 
  let _ = Printf.fprintf oc "%d\n" flow in 

  let nb_edg = G.nb_edg_with_flow graph in 
  let _ = Printf.fprintf oc "%d\n" nb_edg in

  List.iter (fun (node1, node2, value) ->
    Printf.fprintf oc "%s %s %d\n" node1 node2 value
  ) (G.creat_edg_list graph);
  close_out oc


let (src,dst,edg) = Analyse.phase2 ()

let graph = G.create_graph_ponderer edg 

let graph_with_max_flow = G.dinic graph src dst

let _ = write_out graph_with_max_flow src dst 