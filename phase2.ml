(* Utilisation d'un module de graphe paramétré pour des nœuds de type String *)
module G = Graph.Make(String);;

(**
 * @requires un graphe et la source des flux
 * @ensures Crée un fichier où sur la première lignee on a le flow total qui transite, en deuxième ligne 
   le nombre d'arrete impliqué et en dernier tout la liste de toute  les arrete avec leur pondérations
 * @raises Rien
 *)
let write_out graph src =
  (* Création du nom de fichier de sortie basé sur le nom de fichier d'entrée *)
  let filename = Sys.argv.(1) in
  let out_filename =
    let base_name = Filename.chop_extension filename in
    base_name ^ "_out" ^ Filename.extension filename
  in

  (* Ouverture du fichier de sortie pour l'écriture *)
  let oc = open_out out_filename in

  (* Calcul du flux maximal et écriture dans le fichier *)
  let flow = G.max_flow graph src in 
  let _ = Printf.fprintf oc "%d\n" flow in 

  (* Obtention du nombre d'arêtes avec un flux positif et écriture *)
  let nb_edg = G.nb_edg_with_flow graph in 
  let _ = Printf.fprintf oc "%d\n" nb_edg in

  (* Écriture des détails des arêtes avec flux dans le fichier *)
  List.iter (fun (node1, node2, value) ->
    Printf.fprintf oc "%s %s %d\n" node1 node2 value
  ) (G.creat_edg_list graph);
  close_out oc

(* Analyse et récupération des données initiales pour la phase 2 *)
let (src,dst,edg) = Analyse.phase2 ()

(* Création d'un graphe pondéré à partir des arêtes récupérées *)
let graph = G.create_graph_ponderer edg 

(* Application de l'algorithme de Dinic pour calculer le flux maximal *)
let graph_with_max_flow = G.dinic graph src dst

(* Écriture des résultats dans un fichier de sortie *)
let _ = write_out graph_with_max_flow src
