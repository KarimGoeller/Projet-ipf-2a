exception EmptySet
exception EmptyMap
exception NoWay

module type OrderedType =
sig
  type t

  val compare : t -> t -> int
end

module type Graph =
sig

  module NodeSet : Set.S 

  module NodeMap : Map.S 

  type node
    
  type graph

  val empty : graph

  val is_empty : graph -> bool

  val succs_Set : node -> graph -> NodeSet.t

  val succs_Map : node -> graph -> (int * int) NodeMap.t

  val mem_vertex : node -> graph -> bool

  val mem_edge : node -> node -> graph -> bool

  val add_vertex : node -> graph -> graph

  val add_edge : node -> node -> int -> int -> graph -> graph

  val remove_vertex : node -> graph -> graph

  val remove_edge : node -> node -> graph -> graph

  val fold_vertex : (node -> 'a -> 'a) -> graph -> 'a -> 'a

  val fold_edge : (node -> node -> (int * int) -> 'a -> 'a) -> graph -> 'a -> 'a

  val create_graph_ponderer : (node*node*int*int) list -> graph

  val create_graph_non_ponderer : (node*node) list -> graph

  val floyd_warshall_paths : graph -> node -> node -> (node list) list

  val create_residual_graph : graph -> graph 
  
  val build_level_graph : graph -> node -> graph
  
  val print_graph : graph -> unit

end

module Make(X : OrderedType) =
struct

  module NodeSet = Set.Make(X)


  module NodeMap = Map.Make(X)

  type node = X.t

  (** Graphes non pondérés
   * Les clés du graphes correspondent aux sommets présents dans le graphe
   * La valeur d'une clé, d'un sommet, correspond à l'ensemble des successeurs de ce sommet dans le graphe.
   * Quand on ajoute un nouveau sommet, son ensemble qui lui est associé est l'ensemble vide.
   *)
  type graph = ((int*int) NodeMap.t) NodeMap.t

  (**************************************************************************************************************************)

  let empty = 
    NodeMap.empty

  let is_empty graph = 
    NodeMap.is_empty graph

  let succs_Set vertex graph = 
    NodeMap.fold (fun key _ acc -> NodeSet.add key acc) (NodeMap.find vertex graph) NodeSet.empty;;

  let succs_Map vertex graph = 
    NodeMap.find vertex graph ;;

  let mem_vertex vertex graph =
    NodeMap.mem vertex graph

  let mem_edge vertex1 vertex2 graph =
    if not (NodeMap.mem vertex1 graph) || not (NodeMap.mem vertex2 graph) then
      false
    else
      let succ_vertex1 = succs_Set vertex1 graph in
      NodeSet.mem vertex2 succ_vertex1
    ;;
      
  let add_vertex vertex graph = 
    if not (mem_vertex vertex graph) then
      NodeMap.add vertex NodeMap.empty graph
    else
      graph
    ;;

  let add_edge vertex1 vertex2 p p_max graph = 
    let succ_vertex1 = NodeMap.find vertex1 graph in
    let new_succ_vertex1 =NodeMap.add vertex2 (p,p_max) succ_vertex1 in 
    NodeMap.add vertex1 new_succ_vertex1 graph
  ;;


  let remove_vertex vertex graph =
    if NodeMap.mem vertex graph then
      (** Suppression de vertex dans les successeurs des autres noeuds s'il y est *)
      let graph2 = NodeMap.fold (fun vertex_fold succs_fold acc ->  NodeMap.add vertex_fold (NodeSet.remove vertex succs_fold) acc) graph empty in
      
      (** Suppression du noeud *)
      NodeMap.remove vertex graph2
        
    else
      graph
    ;;
  let remove_vertex vertex graph = 
    if (mem_vertex vertex graph) then 
    let graph2 = NodeMap.fold (fun vertex_fold succs_fold acc ->  NodeMap.add vertex_fold (NodeMap.remove vertex succs_fold) acc) graph empty in
    NodeMap.remove vertex graph2
  else
    graph 
  ;;
  
  let remove_edge vertex1 vertex2 graph = 
    if (mem_edge vertex1 vertex2 graph) then 
      let succs = NodeMap.find vertex1 graph in 
      let new_succs = NodeMap.remove vertex2 succs in 
    NodeMap.add vertex1 new_succs graph
    else
      graph
;;

let fold_vertex f graph acc = 
  NodeMap.fold (fun vertex _ acc' -> f vertex acc') graph acc 
  ;; 

let fold_edge f graph acc = 
  NodeMap.fold (fun vertex succs acc' -> NodeMap.fold (fun vertex_succs pond acc_fold -> f vertex vertex_succs pond acc_fold) succs acc') graph acc 
  ;; 

  let create_graph_ponderer list_edges =
    let rec aux list graph = match list with
      |[] -> graph
      |(vertex1,vertex2,p,p_max)::t -> let graph2 = (add_edge vertex1 vertex2 p p_max (add_vertex vertex1 (add_vertex vertex2 graph))) in
                                 aux t graph2
    in
    aux list_edges empty
  ;;

  let create_graph_non_ponderer list_edges =
    let rec aux list graph = match list with
      |[] -> graph
      |(vertex1,vertex2)::t -> let graph2 = (add_edge vertex1 vertex2 0 0 (add_vertex vertex1 (add_vertex vertex2 graph))) in
                                 aux t graph2
    in
    aux list_edges empty
  ;;

  let rec find_paths_rec graph visited source destination current_path all_paths  =
  (*Si on a déjà visiter le sommet sur le quel on se trouve on arrête*)
    if NodeSet.mem source visited then
      all_paths
    else
      (*sinon on l'ajoute a la liste des sommet visité*)
      let visited' = NodeSet.add source visited in
      (*on recupère tout les successeur de se sommet *)
      let succs_source = succs_Set source graph in
        NodeSet.fold (fun succ acc ->
          (*on crée pour chaque succeseur, une liste representant le chemin qu'on 
          parcour avec a son bon le succeseur*)
          let new_path = succ :: current_path in
          (*si le succeseur se trouve être notre destination, on s'arrete et 
          on ajoute le chemin reliant la source a la destination à all_path*)
          if succ = destination then
            (List.rev new_path) :: acc
          else
            (*si on est pas arrivé à la destination, on relance la fonction avec comme nouveau 
            sommet courant un succeseur de l'ancien, et comme chemin actuel le nouveau avec le successeur en bout de chaine*)
            find_paths_rec graph visited' succ destination new_path acc 
        ) succs_source all_paths
  ;;
    
  
  let find_paths graph source destination =
    find_paths_rec graph NodeSet.empty source destination [source] []
  ;;
  

  let filter_shortest_lists lists =
    (*on recherche la taille du plus petit chemin*)
    let min_length = List.fold_left (fun acc lst -> min acc (List.length lst)) max_int lists in
    (*puis on elève tout les chemin qui ne sont pas de taille minimal*)
    List.filter (fun lst -> List.length lst = min_length) lists
  ;;  

  let floyd_warshall_paths graph src dst =
    let all_path =find_paths graph src dst in
    (*si il n'y a pas de chemin on lève une exception*) 
    if all_path = [] then 
      let _ = Printf.printf "Il n'y a aucun chemin de la source jusqu'au puits \n" in 
      raise NoWay 
    else
      (*si il existe au moin un chemin on filtre pour ne garder que les plus petit*)
      filter_shortest_lists all_path
  ;;

  let create_residual_graph graph =
    fold_edge (fun u v capacity residual_graph ->
      let (flow,flow_max) = capacity in  
      let residual_capacity = flow_max - flow in 
      if residual_capacity > 0 then 
        let residual_graph' = add_edge u v residual_capacity flow_max (add_vertex u (add_vertex v residual_graph)) in 
        add_edge v u flow flow_max residual_graph'
      else 
        residual_graph
    ) graph NodeMap.empty
    ;;  


    (* Fonction pour construire un graphe de niveau à partir du graphe résiduel *)
let build_level_graph residual_graph src =
  (* Initialisation de la carte des niveaux avec des valeurs par défaut -1 *)
  let level_map = ref (NodeMap.map (fun _ -> -1) residual_graph) in

  (* Fonction récursive pour parcourir le graphe en largeur (BFS) *)
  let rec bfs queue =
    match queue with
    | [] -> ()
    | (u, lvl) :: rest ->
      (* Mettre à jour le niveau de u dans la carte des niveaux *)
      level_map := NodeMap.add u lvl !level_map;
      
      (* Trouver les voisins de u avec un flux positif et un niveau non défini *)
      let neighbors = NodeMap.fold (fun v (flow, _) acc ->
        if flow > 0 && NodeMap.find v !level_map = -1 then (v, lvl + 1) :: acc else acc
      ) (NodeMap.find u residual_graph) rest in
      
      (* Appel récursif avec les voisins trouvés *)
      bfs neighbors
  in

  (* Appel initial de BFS avec la source et le niveau 0 *)
  bfs [(src, 0)];

  (* Construction du graphe de niveau *)
  let level_graph = NodeMap.fold (fun u _ acc ->
    NodeMap.fold (fun v (flow, flow_max) acc_inner ->
      (* Ajouter une arête au graphe de niveau si les conditions sont remplies *)
      if flow > 0 && NodeMap.find v !level_map = (NodeMap.find u !level_map) + 1 then
        add_edge u v flow flow_max acc_inner
      else
        acc_inner
    ) (NodeMap.find u residual_graph) acc
  ) residual_graph empty in

  level_graph (* Retourner le graphe de niveau résultant *)
;;



let print_graph graph =
  fold_vertex (fun node _ ->
    Printf.printf "Node %s: " node ; 
    let succs = succs_Map node graph in
    NodeMap.iter (fun succ (flow, capacity) ->
      Printf.printf "-> %s (flow: %d, capacity: %d) " succ flow capacity;
    ) succs;
    print_newline ();
  ) graph ()
;;




end