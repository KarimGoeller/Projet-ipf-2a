
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
    (*on place toute les clefs (donc les successeur) dans un Set*)
    NodeMap.fold (fun key _ acc -> NodeSet.add key acc) (NodeMap.find vertex graph) NodeSet.empty

  let succs_Map vertex graph = 
    (*on récupère la map de tout les successeurs de vertex*)
    NodeMap.find vertex graph 

  let mem_vertex vertex graph =
    NodeMap.mem vertex graph

  let mem_edge vertex1 vertex2 graph =
    (*on s'assure d'abord que vertex1 et vertex2 sont bien dans le graph*)
    if not (NodeMap.mem vertex1 graph) || not (NodeMap.mem vertex2 graph) then
      false
    else
      (*on vérifie ensuite que vertex2 est bien dans les successeurs de vertex1*)
      let succ_vertex1 = succs_Set vertex1 graph in
      NodeSet.mem vertex2 succ_vertex1
      
  let add_vertex vertex graph = 
    (*si le sommets n'est pas dans ke graph on le rajoute et on ne lui mets aucun successeur*)
    if not (mem_vertex vertex graph) then
      NodeMap.add vertex NodeMap.empty graph
    (*si le sommet est déjà dans le graph on ne fait rien*)
    else
      graph

  let add_edge vertex1 vertex2 p p_max graph =
    (* on s'assure que vertex1 et vertex2 sont bien dans le grrpah, si non on les rajoute *)
    let graph = if not (NodeMap.mem vertex1 graph) then
                  NodeMap.add vertex1 NodeMap.empty graph
                else
                  graph
    in
    let graph = if not (NodeMap.mem vertex2 graph) then
                  NodeMap.add vertex2 NodeMap.empty graph
                else
                  graph
    in
    (* Maintenant, que on est sur que vertex1 et vertex2 on rajoute l'arrête demande ou on l'actualise *)
    let succ_vertex1 = NodeMap.find vertex1 graph in
    let new_succ_vertex1 = NodeMap.add vertex2 (p, p_max) succ_vertex1 in
    NodeMap.add vertex1 new_succ_vertex1 graph

  let remove_vertex vertex graph =
    if NodeMap.mem vertex graph then
      (* Suppression de vertex dans les successeurs des autres noeuds s'il y est *)
      let graph2 = NodeMap.fold (fun vertex_fold succs_fold acc -> NodeMap.add vertex_fold (NodeMap.remove vertex succs_fold) acc) graph empty in
      (* Suppression du noeud *)
      NodeMap.remove vertex graph2
    else
      graph
  
  let remove_edge vertex1 vertex2 graph = 
    (*on s'assure que l'arrête existe bien sinon on ne fait rien*)
    if (mem_edge vertex1 vertex2 graph) then 
      (*on enlève vertex2 dans les successeur de vertex1*)
      let succs = NodeMap.find vertex1 graph in 
      let new_succs = NodeMap.remove vertex2 succs in 
      (*on actualise la map asssocier a vertex 1*)
      NodeMap.add vertex1 new_succs graph
    else
      graph

  let fold_vertex f graph acc = 
    NodeMap.fold (fun vertex _ acc' -> f vertex acc') graph acc 

  let fold_edge f graph acc = 
    (*on parcour toute les arrête et on applique la focntions forunis*)
    NodeMap.fold (fun vertex succs acc' -> NodeMap.fold (fun vertex_succs pond acc_fold -> f vertex vertex_succs pond acc_fold) succs acc') graph acc 

  let create_graph_ponderer list_edges =
    let rec aux list graph = match list with
      (*si il n'y pas plus d'arrête à mettre on s'arrete*)
      | [] -> graph
      (*sinon on ajoute la nouvelle arrête au graph  *)
      | (vertex1, vertex2, p_max) :: t -> let graph2 = (add_edge vertex1 vertex2 0 p_max graph) in
                                           aux t graph2
    in
    aux list_edges empty

  let create_graph_non_ponderer list_edges =
    let rec aux list graph = match list with
      | [] -> graph
      | (vertex1, vertex2) :: t -> let graph2 =  (add_edge vertex1 vertex2 0 0 graph) in
                                   aux t graph2
    in
    aux list_edges empty

(**********Début code spécifique à la phase 1*******************)

let shortest_path graph source destination =
  let rec bfs queue visited paths shortest_path_length =
    match queue with
    (*la file d'attente est vide, on a exploré tout les chemins*)
    | [] -> paths
    | (current, path) :: rest ->
        let new_path = current :: path in  (* Construit le nouveau chemin en ajoutant le nœud courant *)
        (* Si on atteint la destination, on traite le chemin trouvé *)
        if current = destination then
          let new_shortest_path_length = List.length new_path in
          if new_shortest_path_length > shortest_path_length then
            (* Si la longueur du nouveau chemin est plus longue que les plus courts chemin trouvé, on l'ignore *)
            bfs rest visited paths shortest_path_length
          else
            (* Si le nouveau chemin est plus court ou égal au plus court chemin connu, l'ajoute aux chemins trouvés *)
            bfs rest visited (List.rev new_path :: paths) new_shortest_path_length
        else
          (* Si le nœud courant n'est pas la destination *)
          if NodeSet.mem current visited then
            (* Si le nœud courant a déjà été visité, passe au suivant dans la file d'attente *)
            bfs rest visited paths shortest_path_length
          else
            (* Si le nœud courant n'a pas été visité *)
            let new_visited = NodeSet.add current visited in  (* Ajoute le nœud courant aux nœuds visités *)
            let neighbors = succs_Set current graph in  (* Obtient les voisins du nœud courant *)
            let new_queue = NodeSet.fold (fun neighbor acc ->
              if not (NodeSet.mem neighbor visited) then
                (* Ajoute les voisins non visités à la file d'attente pour exploration future *)
                (neighbor, new_path) :: acc
              else
                acc
            ) neighbors rest in
            bfs new_queue new_visited paths shortest_path_length
  in
  bfs [(source, [])] NodeSet.empty [] max_int  (* Initialisation de la BFS avec la source *)

  (**********Fin du code spécifique à la phase 1*******************)

  (**********Début code spécifique à la phase 2*******************)

  let create_residual_graph graph =
    (* On utilise fold_edge pour parcourir toutes les arêtes du graphe donné *)
    fold_edge (fun u v capacity residual_graph ->
      let (flow, flow_max) = capacity in  
      let residual_capacity = flow_max - flow in 
      (* Calcule la capacité résiduelle de l'arête *)
      if residual_capacity > 0 then 
        (* Si la capacité résiduelle est positive, on traite l'arête *)
        let residual_graph' = 
          (* Ajoute l'arête avec la capacité résiduelle dans le graphe résiduel *)
          add_edge u v residual_capacity flow_max residual_graph in 
        if flow > 0 then 
          (* Si un flux existe déjà dans l'arête, ajoute une arête inverse dans le graphe résiduel *)
          add_edge v u flow flow_max residual_graph'
        else
          (* Si aucun flux n'existe, garde le graphe résiduel tel quel *)
          residual_graph'
      else 
        (* Si la capacité résiduelle est nulle ou négative, ignore cette arête *)
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
    (* Fonction récursive pour mettre à jour les arêtes le long du chemin *)
    let rec update_edges graph path =
      match path with
      | u :: v :: rest ->
        (* Traite chaque paire d'arêtes consécutives dans le chemin *)
        (* Récupère le flux actuel et le flux maximum de l'arête de u à v *)
        let (flow_u_v, max_u_v) = NodeMap.find v (NodeMap.find u graph) in
        let new_graph = 
          (* Met à jour le graphe en diminuant le flux de l'arête de u à v par le flux trouvé *)
          NodeMap.add u (NodeMap.add v (flow_u_v - flow, max_u_v) (NodeMap.find u graph)) graph 
        in
        (* Vérifie et met à jour l'arête inverse de v à u si elle existe *)
        let new_graph' = 
          if NodeMap.mem v new_graph && NodeMap.mem u (NodeMap.find v new_graph) then
            let (flow_v_u, max_v_u) = NodeMap.find u (NodeMap.find v new_graph) in
            (* Augmente le flux de l'arête inverse de v à u par le flux trouvé *)
            NodeMap.add v (NodeMap.add u (flow_v_u + flow, max_v_u) (NodeMap.find v new_graph)) new_graph
          else
            new_graph
        (* Termine la récursion lorsque le chemin est vide ou ne contient qu'un seul nœud *)
        | _ -> graph  
        in
        (* Continue de mettre à jour le graphe le long du reste du chemin *)
        update_edges new_graph' rest
    in
    (* Commence la mise à jour des arêtes en utilisant le graphe résiduel et le chemin donné *)
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
(**********Fin du code spécifique à la phase 2*******************)
  
(*******************Fonction auxillair dont je me sert pour écrire les fichier de la bonne manière***************************)
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
  
  end
  