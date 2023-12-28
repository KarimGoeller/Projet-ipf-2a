
exception EmptySet
exception EmptyMap
exception NoWay

module type OrderedType =
sig
  type t
  val compare : t -> t -> int
  (* val to_string : t -> string *)
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
  val create_graph_ponderer : (node*node*int) list -> graph
  val create_graph_non_ponderer : (node*node) list -> graph
  val shortest_path : graph -> node -> node -> (node list) list
  val create_residual_graph : graph -> graph 
  val build_level_graph : graph -> node -> graph
  val find_blocking_flow : graph -> (node list) -> int 
  val update_graph : graph -> node list -> int -> graph  
  val dinic : graph -> node -> node -> graph
  val max_flow : graph -> node -> int 
  val nb_edg_with_flow : graph -> int
  val creat_edg_list : graph -> (node * node * int) list
  (* val print_graph : graph -> unit *)
end

module Make(X : OrderedType) =
struct
  module NodeSet = Set.Make(X)
  module NodeMap = Map.Make(X)

  type node = X.t
  type graph = ((int*int) NodeMap.t) NodeMap.t

  (**************************************************************************************************************************)

  let empty = 
    NodeMap.empty

  let is_empty graph = 
    NodeMap.is_empty graph

  let succs_Set vertex graph = 
    NodeMap.fold (fun key _ acc -> NodeSet.add key acc) (NodeMap.find vertex graph) NodeSet.empty

  let succs_Map vertex graph = 
    NodeMap.find vertex graph 

  let mem_vertex vertex graph =
    NodeMap.mem vertex graph

  let mem_edge vertex1 vertex2 graph =
    if not (NodeMap.mem vertex1 graph) || not (NodeMap.mem vertex2 graph) then
      false
    else
      let succ_vertex1 = succs_Set vertex1 graph in
      NodeSet.mem vertex2 succ_vertex1
      
  let add_vertex vertex graph = 
    if not (mem_vertex vertex graph) then
      NodeMap.add vertex NodeMap.empty graph
    else
      graph

  let add_edge vertex1 vertex2 p p_max graph =
    (* Assurez-vous que vertex1 est dans le graphe. S'il ne l'est pas, ajoutez-le. *)
    let graph = if not (NodeMap.mem vertex1 graph) then
                  NodeMap.add vertex1 NodeMap.empty graph
                else
                  graph
    in
    (* Assurez-vous que vertex2 est dans le graphe. S'il ne l'est pas, ajoutez-le. *)
    let graph = if not (NodeMap.mem vertex2 graph) then
                  NodeMap.add vertex2 NodeMap.empty graph
                else
                  graph
    in
    (* Maintenant, ajoutez ou mettez à jour l'arête entre vertex1 et vertex2. *)
    let succ_vertex1 = NodeMap.find vertex1 graph in
    let new_succ_vertex1 = NodeMap.add vertex2 (p, p_max) succ_vertex1 in
    NodeMap.add vertex1 new_succ_vertex1 graph

  let remove_vertex vertex graph =
    if NodeMap.mem vertex graph then
      (* Suppression de vertex dans les successeurs des autres noeuds s'il y est *)
      let graph2 = NodeMap.fold (fun vertex_fold succs_fold acc -> NodeMap.add vertex_fold (NodeSet.remove vertex succs_fold) acc) graph empty in
      (* Suppression du noeud *)
      NodeMap.remove vertex graph2
    else
      graph
    
  let remove_vertex vertex graph = 
    if (mem_vertex vertex graph) then 
      let

 graph2 = NodeMap.fold (fun vertex_fold succs_fold acc -> NodeMap.add vertex_fold (NodeMap.remove vertex succs_fold) acc) graph empty in
      NodeMap.remove vertex graph2
    else
      graph 
  
  let remove_edge vertex1 vertex2 graph = 
    if (mem_edge vertex1 vertex2 graph) then 
      let succs = NodeMap.find vertex1 graph in 
      let new_succs = NodeMap.remove vertex2 succs in 
      NodeMap.add vertex1 new_succs graph
    else
      graph

  let fold_vertex f graph acc = 
    NodeMap.fold (fun vertex _ acc' -> f vertex acc') graph acc 

  let fold_edge f graph acc = 
    NodeMap.fold (fun vertex succs acc' -> NodeMap.fold (fun vertex_succs pond acc_fold -> f vertex vertex_succs pond acc_fold) succs acc') graph acc 

  let create_graph_ponderer list_edges =
    let rec aux list graph = match list with
      | [] -> graph
      | (vertex1, vertex2, p_max) :: t -> let graph2 = (add_edge vertex1 vertex2 0 p_max graph) in
                                           aux t graph2
    in
    aux list_edges empty

  let create_graph_non_ponderer list_edges =
    let rec aux list graph = match list with
      | [] -> graph
      | (vertex1, vertex2) :: t -> let graph2 = (add_edge vertex1 vertex2 0 0 (add_vertex vertex1 (add_vertex vertex2 graph))) in
                                   aux t graph2
    in
    aux list_edges empty

  let shortest_path graph source destination =
  let rec bfs queue visited paths shortest_path_length =
    match queue with
    | [] -> paths  (* Lorsque la file est vide, retourne les chemins trouvés *)
    | (current, path) :: rest ->
      let new_path = current :: path in  (* Ajoute le nœud courant au chemin *)
      if current = destination then
        (* Si la destination est atteinte, ajoute le chemin aux chemins trouvés *)
        let new_shortest_path_length = List.length new_path in
        if new_shortest_path_length > shortest_path_length then
          bfs rest visited paths shortest_path_length  (* Ignore les chemins plus longs que le plus court trouvé *)
        else
          bfs rest visited (List.rev new_path :: paths) new_shortest_path_length
      else if NodeSet.mem current visited then
        bfs rest visited paths shortest_path_length  (* Passe au suivant si le nœud est déjà visité *)
      else
        let new_visited = NodeSet.add current visited in
        let neighbors = succs_Set current graph in
        let new_queue = NodeSet.fold (fun neighbor acc ->
          if not (NodeSet.mem neighbor visited) then
            (neighbor, new_path) :: acc
          else
            acc
        ) neighbors rest in
        bfs new_queue new_visited paths shortest_path_length
  in
  bfs [(source, [])] NodeSet.empty [] max_int 

  let create_residual_graph graph =
    fold_edge (fun u v capacity residual_graph ->
      let (flow, flow_max) = capacity in  
      let residual_capacity = flow_max - flow in 
      if residual_capacity > 0 then 
        (* Ajoute l'arête avec la capacité résiduelle si elle est positive. *)
        let residual_graph' = add_edge u v residual_capacity flow_max (add_vertex u (add_vertex v residual_graph)) in 
        if flow > 0 then 
          (* Ajoute une arête inverse pour le flux existant. *)
          add_edge v u flow flow_max residual_graph'
        else
          residual_graph'
      else 
        (* Ignore les arêtes saturées. *)
        residual_graph
    ) graph NodeMap.empty

  let build_level_graph residual_graph src =
    (* Initialisation de la carte des niveaux avec des valeurs par défaut -1 *)
    let level_map = NodeMap.map (fun _ -> -1) residual_graph in
    (* Fonction récursive pour parcourir le graphe en largeur (BFS) *)
    let rec bfs current_map queue =
      match queue with
      | [] -> current_map
      | (u, lvl) :: rest ->
        let current_level = NodeMap.find u current_map in
        (* Mettre à jour le niveau de u si le nœud n'a pas encore été visité ou si le nouveau niveau est inférieur *)
        let updated_map =
          if current_level = -1 || lvl < current_level then
            NodeMap.add u lvl current_map
          else
            current_map
        in
        let neighbors = NodeMap.fold (fun v (flow, _) acc ->
          let neighbor_level = NodeMap.find v updated_map in
          if flow >  0 && (neighbor_level = -1 || lvl + 1 < neighbor_level) then
          (v, lvl + 1) :: acc
        else
          acc
      ) (NodeMap.find u residual_graph) rest in

      bfs updated_map neighbors
  in

  (* Appel initial de BFS avec la source et le niveau 0 *)
  let initial_queue = [(src, 0)] in
  let final_level_map = bfs level_map initial_queue in

  (* Construction du graphe de niveau *)
  let level_graph = NodeMap.fold (fun u _ acc ->
    NodeMap.fold (fun v (flow, flow_max) acc_inner ->
      (* Ajouter une arête au graphe de niveau si les conditions sont remplies *)
      if flow > 0 && NodeMap.find v final_level_map = (NodeMap.find u final_level_map) + 1 then
        add_edge u v flow flow_max acc_inner
      else
        acc_inner
    ) (NodeMap.find u residual_graph) acc
  ) residual_graph empty in

  level_graph (* Retourner le graphe de niveau résultant *)

  let update_residual_graph residual_graph path flow =
    let rec update_edges graph path =
      match path with
      | u :: v :: rest ->
        (* Récupère et met à jour le flux de l'arête de u à v *)
        let (flow_u_v, max_u_v) = NodeMap.find v (NodeMap.find u graph) in
        let new_graph = 
          NodeMap.add u (NodeMap.add v (flow_u_v - flow, max_u_v) (NodeMap.find u graph)) graph 
        in
        
        (* Si l'arête de retour de v à u existe, la met à jour ; sinon, conserve le graphique tel quel *)
        let new_graph' = 
          if NodeMap.mem v new_graph && NodeMap.mem u (NodeMap.find v new_graph) then
            let (flow_v_u, max_v_u) = NodeMap.find u (NodeMap.find v new_graph) in
            NodeMap.add v (NodeMap.add u (flow_v_u + flow, max_v_u) (NodeMap.find v new_graph)) new_graph
          else
            new_graph
        in
  
        (* Appelle récursivement pour les autres arêtes du chemin *)
        update_edges new_graph' rest
      | _ -> graph  (* Termine la récursion quand il n'y a plus d'arêtes à traiter *)
    in
    update_edges residual_graph path
  
  
  let find_blocking_flow level_graph path = 
    (* Fonction auxiliaire récursive pour traverser le chemin et trouver le flux minimal *)
    let rec aux graph path flow =
      match path with 
      | u :: v :: rest ->
        (* Obtient le flux et la capacité maximale de l'arête de u à v *)
        let (flow_u_v, _) = NodeMap.find v (NodeMap.find u graph) in
  
        (* Compare le flux actuel avec le flux de l'arête et choisit le plus petit *)
        if flow > flow_u_v then 
          aux graph (v::rest) flow_u_v
        else
          aux graph (v::rest) flow
      | _ -> flow  (* Retourne le flux bloquant trouvé lorsque le chemin est complètement parcouru *)
    in
    (* Commence la recherche avec le flux initialisé à la valeur maximale possible *)
    aux level_graph path max_int
  
  
  let update_graph graph path blocking_flow = 
    (* Fonction récursive auxiliaire pour mettre à jour chaque arête du chemin *)
    let rec aux_update graph flow path' =
      match path' with 
      | u :: v :: rest ->
        (* Récupère le flux actuel et la capacité maximale de l'arête de u à v *)
        let (flow_u_v, max_u_v) = NodeMap.find v (NodeMap.find u graph) in
            
        (* Met à jour le graphe avec le nouveau flux pour l'arête de u à v *)
        let new_graph = 
          NodeMap.add u (NodeMap.add v (flow_u_v + flow, max_u_v) (NodeMap.find u graph)) graph 
        in
            
        (* Poursuit la mise à jour pour le reste du chemin *)
        aux_update new_graph flow (v::rest)
  
      | _ -> graph  (* Termine la mise à jour lorsque tous les éléments du chemin ont été traités *)
    in
    aux_update graph blocking_flow path
  
  
  let rec dinic_loop graph residual_graph src dst =  
    (* Vérifie si le graphe résiduel n'est pas vide *)
    if not (NodeMap.is_empty residual_graph) then  
      (* Construit un graphe de niveau à partir du graphe résiduel *)
      let level_graph = build_level_graph residual_graph src in 
  
      (* Trouve le plus court chemin (chemin augmentant) dans le graphe de niveau *)
      let all_path = shortest_path level_graph src dst in 
  
      match all_path with
      | [] -> graph  (* Si aucun chemin n'est trouvé, renvoie le graphe actuel *)
      | path :: _ -> 
        (* Met à jour le graphe en poussant le flux bloquant à travers le chemin trouvé *)
        let blocking_flow = find_blocking_flow level_graph path in 
        let updated_graph = update_graph graph path blocking_flow in 
  
        (* Appelle récursivement la fonction avec le graphe mis à jour et son graphe résiduel *)
        dinic_loop updated_graph (create_residual_graph updated_graph) src dst
    else
      graph  (* Si le graphe résiduel est vide, renvoie le graphe actuel *)
  
  
  let dinic graph src dst = 
    dinic_loop graph (create_residual_graph graph) src dst
  
  
  let max_flow graph src = 
    NodeMap.fold (fun _ (p,_) acc -> acc + p) (succs_Map src graph) 0
  
  
  let nb_edg_with_flow graph = 
    NodeMap.fold (fun u succs acc' ->
      NodeMap.fold (fun v (flow, flow_max) acc ->
        if flow > 0 then
          acc + 1
        else
          acc
      ) (succs_Map u graph) acc'
    ) graph 0 
  
  
  let creat_edg_list graph = 
    NodeMap.fold (fun u succs acc' ->
      NodeMap.fold (fun v (flow, flow_max) acc ->
        if flow > 0 then
          (u, v, flow) :: acc
        else
          acc
      ) (succs_Map u graph) acc'
    ) graph []
  
  (*let print_graph graph =
    fold_vertex (fun node _ ->
      Printf.printf "Node %s: " (X.to_string node); 
      let succs = succs_Map node graph in
      NodeMap.iter (fun succ (flow, capacity) ->
        Printf.printf "-> %s (flow: %d, capacity: %d) " (X.to_string succ) flow capacity;
      ) succs;
      print_newline ();
    ) graph ()*)
  
  end
  