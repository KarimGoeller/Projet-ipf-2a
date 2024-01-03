
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
(**
  * @requires Rien
  * @ensures Retourne true si le graphe en paramètre est vide, false sinon
  * @raises Rien
  *)
  let is_empty graph = 
    NodeMap.is_empty graph


(**
 * @requires Le deuxième paramètre, un graphe, contient le premier paramètre, un sommet, et le graphe est non vide
 * @ensures Renvoie l'ensemble des successeurs d'un sommet (premier paramètre) dans un graphe (deuxième paramètre)
 * @raises Rien
 *)
  let succs_Set vertex graph = 
    (*on place toutes les clefs (donc les successeurs) dans un Set*)
    NodeMap.fold (fun key _ acc -> NodeSet.add key acc) (NodeMap.find vertex graph) NodeSet.empty
    
    
    (**
      * @requires Le deuxième paramètre, un graphe, contient le premier paramètre, un sommet, et le graphe est non vide
      * @ensures Renvoie la map des successeurs d'un sommet (premier paramètre) dans un graphe (deuxième paramètre)
      * @raises Rien
      *)
  let succs_Map vertex graph = 
    (*on récupère la map de tous les successeurs de vertex*)
    NodeMap.find vertex graph 


     (**
      * @requires Rien
      * @ensures Indique si un sommet (premier paramètre) est présent dans un graphe (deuxième paramètre)
      * @raises Rien
      *)
  let mem_vertex vertex graph =
    NodeMap.mem vertex graph


     (**
      * @requires Rien
      * @ensures Indique si un arc d'un sommet (premier paramètre) à un autre (deuxième paramètre) est présent dans un graphe (troisième paramètre)
      *          Retourne également false si un des deux sommets en paramètres n'appartiennent pas au graphe
      * @raises Rien
      *)

  let mem_edge vertex1 vertex2 graph =
    (*on s'assure d'abord que vertex1 et vertex2 sont bien dans le graph*)
    if not (NodeMap.mem vertex1 graph) || not (NodeMap.mem vertex2 graph) then
      false
    else
      (*on vérifie ensuite que vertex2 est bien dans les successeurs de vertex1*)
      let succ_vertex1 = succs_Set vertex1 graph in
      NodeSet.mem vertex2 succ_vertex1
      


     (**
      * @requires Rien
      * @ensures Ajoute un sommet (premier paramètre) à un graphe (deuxième paramètre)
      * @raises Rien
      *)
  let add_vertex vertex graph = 
    (*si le sommet n'est pas dans le graph on le rajoute et on ne lui met aucun successeur*)
    if not (mem_vertex vertex graph) then
      NodeMap.add vertex NodeMap.empty graph
    (*si le sommet est déjà dans le graphe on ne fait rien*)
    else
      graph


     (**
      * @requires rien
      * @ensures Ajoute un arc d'un sommet vers un autre (deux premiers paramètres) d'un graphe (troisième paramètre)
      * @raises Rien
      *)
  let add_edge vertex1 vertex2 p p_max graph =
    (* on s'assure que vertex1 et vertex2 sont bien dans le graphe, sinon on les rajoute *)
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
    (* Maintenant, que vertex1 et vertex2  sont dans le graphe, on rajoute l'arrête demandée ou on l'actualise *)
    let succ_vertex1 = NodeMap.find vertex1 graph in
    let new_succ_vertex1 = NodeMap.add vertex2 (p, p_max) succ_vertex1 in
    NodeMap.add vertex1 new_succ_vertex1 graph


     (**
      * @requires Rien
      * @ensures Retire un sommet (premier paramètre) d'un graphe (deuxième paramètre)
      * @raises Rien
      *)
  let remove_vertex vertex graph =
    if NodeMap.mem vertex graph then
      (* Suppression de vertex dans les successeurs des autres noeuds s'il y est *)
      let graph2 = NodeMap.fold (fun vertex_fold succs_fold acc -> NodeMap.add vertex_fold (NodeMap.remove vertex succs_fold) acc) graph empty in
      (* Suppression du noeud *)
      NodeMap.remove vertex graph2
    else
      graph
  

     (**
      * @requires Les deux premiers paramètres (des sommets) appartiennent au troisième paramètre (un graphe), le graphe est non vide
      * @ensures Retire un arc d'un sommet vers un autre (deux premiers paramètres) d'un graphe (troisième paramètre)
      * @raises Rien
      *)
  let remove_edge vertex1 vertex2 graph = 
    (*on s'assure que l'arrête existe bien sinon on ne fait rien*)
    if (mem_edge vertex1 vertex2 graph) then 
      (*on enlève vertex2 dans les successeurs de vertex1*)
      let succs = NodeMap.find vertex1 graph in 
      let new_succs = NodeMap.remove vertex2 succs in 
      (*on actualise la map asssocié à vertex 1*)
      NodeMap.add vertex1 new_succs graph
    else
      graph



     (**
      * @requires Rien
      * @ensures Réalise un fold sur les sommets d'un graphe
      * @raises Rien
      *)
  let fold_vertex f graph acc = 
    NodeMap.fold (fun vertex _ acc' -> f vertex acc') graph acc 


     (**
      * @requires Rien
      * @ensures Réalise un fold sur les arcs d'un graphe pondéré
      * @raises Rien
      *)
  let fold_edge f graph acc = 
    (*on parcourt toutes les arêtes et on applique la fonction fournie*)
    NodeMap.fold (fun vertex succs acc' -> NodeMap.fold (fun vertex_succs pond acc_fold -> f vertex vertex_succs pond acc_fold) succs acc') graph acc 


     (**
     * @requires Rien
     * @ensures Crée un graphe pondéré à partir d'une liste d'arêtes. Chaque arête est un triplet (node1, node2, poids).
     * @raises Rien
     *)
  let create_graph_ponderer list_edges =
    let rec aux list graph = match list with
      (*s'il n'y pas plus d'arêtes à mettre on s'arrete*)
      | [] -> graph
      (*sinon on ajoute la nouvelle arête au graphe  *)
      | (vertex1, vertex2, p_max) :: t -> let graph2 = (add_edge vertex1 vertex2 0 p_max graph) in
                                           aux t graph2
    in
    aux list_edges empty


    (**
     * @requires Rien
     * @ensures Crée un graphe non pondéré(c'est un graphe pondéré mais avec toutes les pondérations nul) à partir d'une liste d'arêtes. Chaque arête est un couple (node1, node2).
     * @raises Rien
     *)
  let create_graph_non_ponderer list_edges =
    let rec aux list graph = match list with
      | [] -> graph
      | (vertex1, vertex2) :: t -> let graph2 =  (add_edge vertex1 vertex2 0 0 graph) in
                                   aux t graph2
    in
    aux list_edges empty

(**********Début code spécifique à la phase 1*******************)


    (**
     * @requires rien
     * @ensures Trouve tous les plus courts chemins entre deux sommets (source et destination) dans un graphe.
     * @raises NoWay
     *)
let shortest_path graph source destination =
  let rec bfs queue visited paths shortest_path_length =
    match queue with
    (*la file d'attente est vide, on a exploré tous les chemins*)
    | [] -> paths
    | (current, path) :: rest ->
        let new_path = current :: path in  (* Construit le nouveau chemin en ajoutant le nœud courant *)
        (* Si on atteint la destination, on traite le chemin trouvé *)
        if current = destination then
          let new_shortest_path_length = List.length new_path in
          if new_shortest_path_length > shortest_path_length then
            (* Si la longueur du nouveau chemin est plus grande que celles des plus courts chemins trouvés, on l'ignore *)
            bfs rest visited paths shortest_path_length
          else
            (* Si le nouveau chemin est plus court ou égal au plus court chemin connu, on l'ajoute aux chemins trouvés *)
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
  let all_path = bfs [(source, [])] NodeSet.empty [] max_int in   (* Initialisation de la BFS avec la source *)
  if all_path = [] then 
    raise NoWay
  else
    all_path 

  (**********Fin du code spécifique à la phase 1*******************)

  (**********Début code spécifique à la phase 2*******************)


    (**
     * @requires Rien
     * @ensures Crée un graphe résiduel à partir d'un graphe donné, utilisé dans les calculs de flux réseau.
     * @raises Rien
     *)
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
          (* Si aucun flux n'existe, on garde le graphe résiduel tel quel *)
          residual_graph'
      else 
        (* Si la capacité résiduelle est nulle ou négative, on ignore cette arête *)
        residual_graph
    ) graph NodeMap.empty



    (**
    * @requires Rien
    * @ensures Construit un graphe de niveau à partir d'un graphe résiduel, utilisé dans l'algorithme de Dinic.
    * @raises Rien
    *)
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

(**
 * @requires Un graphe résiduel, un chemin dans ce graphe et la valeur du flow bloquant.
 * @ensures actualise le graphe résiduel
 * @raises Rien
 *)
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
      in
      (* Continue de mettre à jour le graphe le long du reste du chemin *)
      update_edges new_graph' rest
    | _ -> graph  
  in
  (* Commence la mise à jour des arêtes en utilisant le graphe résiduel et le chemin donné *)
  update_edges residual_graph path

  
    (**
    * @requires Un graphe et un chemin dans ce graphe.
    * @ensures Trouve le flux maximal (bloquant) qui peut être envoyé à travers un chemin donné dans le graphe.
    * @raises rien
    *)
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
  


    (**
    * @requires Un graphe, un chemin dans ce graphe, et un flux à ajouter à ce chemin.
    * @ensures Met à jour le graphe avec le nouveau flux le long du chemin spécifié.
    * @raises Rien
    *)
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
  
   (**
    * @requires Un graphe, un graphe résiduel, un sommet source et un sommet puits.
    * @ensures Applique l'algorithme de Dinic pour trouver le flux maximal du sommet source au sommet puits.
    * @raises NoWay
    *)
  let rec dinic_loop graph residual_graph src dst =  
    (* Vérifie si le graphe résiduel n'est pas vide *)
    if not (NodeMap.is_empty residual_graph) then  
      (* Construit un graphe de niveau à partir du graphe résiduel *)
      let level_graph = build_level_graph residual_graph src in 
  
      (* Trouve le plus court chemin dans le graphe de niveau *)
      let all_path = try shortest_path level_graph src dst with NoWay -> [] in 
  
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
  


    (**
    * @requires Un graphe, un sommet source et un sommet puits.
    * @ensures Applique l'algorithme de Dinic pour trouver le flux maximal du sommet source au sommet puits.
    * @raises NoWay
    *)
  let dinic graph src dst = 
    dinic_loop graph (create_residual_graph graph) src dst
(**********Fin du code spécifique à la phase 2*******************)
  
(*******************Fonction auxilliaire dont je me sers pour écrire les fichiers de la bonne manière***************************)

    (**
    * @requires Un graphe et un sommet source.
    * @ensures Calcule le flux maximal sortant du sommet source dans le graphe.
    * @raises Rien
    *)
  let max_flow graph src = 
    NodeMap.fold (fun _ (p,_) acc -> acc + p) (succs_Map src graph) 0
  

    (**
    * @requires Un graphe.
    * @ensures Compte le nombre d'arêtes dans le graphe ayant un flux non nul.
    * @raises Rien
    *)
  let nb_edg_with_flow graph = 
    NodeMap.fold (fun u succs acc' ->
      NodeMap.fold (fun v (flow, flow_max) acc ->
        if flow > 0 then
          acc + 1
        else
          acc
      ) (succs_Map u graph) acc'
    ) graph 0 
  
    (**
    * @requires Un graphe.
    * @ensures Crée une liste des arêtes avec leur flux dans le graphe.
    * @raises Rien
    *)
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
  