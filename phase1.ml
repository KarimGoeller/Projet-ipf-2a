(* Définition d'une fonction pour écrire les résultats dans un fichier *)
let write_out lst =
  (* Récupération du nom de fichier d'entrée depuis les arguments du programme *)
  let filename = Sys.argv.(1) in

  (* Création du nom de fichier de sortie basé sur le fichier d'entrée *)
  let out_filename =
    let base_name = Filename.chop_extension filename in
    base_name ^ "_out" ^ Filename.extension filename
  in

  (* Ouverture du fichier de sortie pour l'écriture *)
  let oc = open_out out_filename in

  (* Écriture de chaque liste (représentant un chemin) dans le fichier de sortie *)
  List.iter (fun inner_list ->
    output_string oc (String.concat " " inner_list ^ "\n")
  ) lst;

  (* Fermeture du fichier de sortie *)
  close_out oc
;;

(* Création d'un module de graphe avec des chaînes de caractères comme nœuds *)
module G = Graph.Make(String);;

(* Analyse et récupération des données initiales : source, destination et arêtes *)
let (src,dst,edg) = Analyse.phase1 ()

(* Création d'un graphe non pondéré à partir des arêtes récupérées *)
let graph = G.create_graph_non_ponderer edg 

(* Recherche de tous les chemins les plus courts du graphe *)
let shortest_path = G.shortest_path graph src dst 

(* Écriture des chemins les plus courts dans un fichier de sortie *)
let _ = write_out shortest_path
