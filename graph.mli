(**
 * @brief Une exception pour les ensembles vides
 *)
 exception EmptySet

 (**
  * @brief Une exception pour les dictionnaires vides
  *)
 exception EmptyMap
 
 (**
  * @brief Un module type ordonnée
  *)
 module type OrderedType =
 sig
 
     (**
      * @brief Un type t abstrait
      *)
     type t
 
     (**
      * @requires Rien
      * @ensures Renvoie un entier strictement positif si le premier paramètre est plus grand que le deuxième paramètre,
      *          un entier strictement négatif si le premier paramètre est plus petit que le deuxième paramètre,
      *          0 si les deux paramètres sont égaux.
      * @raises Rien
      *)
     val compare : t -> t -> int
     
 
 end
 
 
 (**
  * @brief Un module type Graph pour représenter des graphes pondérés
  *)
 module type Graph =
 sig
 
 
     (**
      * @brief Un module permettant de représenter un ensemble de successeurs de sommets dans un graphe avec leurs étiquettes, nécessaire pour la fonction succs
      * @see succs_Set
      *)
     module NodeSet : Set.S
 
      (**
      * @brief Un module permettant de représenter un ensemble de successeurs de sommets dans un graphe avec leurs étiquettes
      * @see succs_Map
      *)
     module NodeMap : Map.S 
 
     (**
      * @brief Un type node abstrait correspondant au type des sommets dans un graphe
      *)
     type node
     
     (**
      * @brief Un type graph abstrait correspondant au type d'un graphe pondéré
      *)
     type graph
 
 
     (**************************************************************************************************************************)
 
 
     (**
      * @requires Rien
      * @ensures Initialise la variable globale empty à un graphe vide
      * @raises Rien
      *)
    val empty : graph
 
     (**
      * @requires Rien
      * @ensures Retourne true si le graphe en paramètre est vide, false sinon
      * @raises Rien
      *)
    val is_empty : graph -> bool
 
     (**
      * @requires Le deuxième paramètre, un graphe, contient le premier paramètre, un sommet, et le graphe est non vide
      * @ensures Renvoie l'ensemble des successeurs d'un sommet (premier paramètre) dans un graphe (deuxième paramètre)
      * @raises Rien
      *)
    val succs_Set : node -> graph -> NodeSet.t


    (**
      * @requires Le deuxième paramètre, un graphe, contient le premier paramètre, un sommet, et le graphe est non vide
      * @ensures Renvoie la map des successeurs d'un sommet (premier paramètre) dans un graphe (deuxième paramètre)
      * @raises Rien
      *)
    val succs_Map : node -> graph -> (int * int) NodeMap.t
 
     (**
      * @requires Rien
      * @ensures Indique si un sommet (premier paramètre) est présent dans un graphe (deuxième paramètre)
      * @raises Rien
      *)
     val mem_vertex : node -> graph -> bool
 
     (**
      * @requires Rien
      * @ensures Indique si un arc d'un sommet (premier paramètre) à un autre (deuxième paramètre) est présent dans un graphe (troisième paramètre)
      *          Retourne également false si un des deux sommets en paramètres n'appartiennent pas au graphe
      * @raises Rien
      *)
     val mem_edge : node -> node -> graph -> bool
 
     (**
      * @requires Rien
      * @ensures Ajoute un sommet (premier paramètre) à un graphe (deuxième paramètre)
      * @raises Rien
      *)
     val add_vertex : node -> graph -> graph
 
     (**
      * @requires rien
      * @ensures Ajoute un arc d'un sommet vers un autre (deux premiers paramètres) d'un graphe (troisième paramètre)
      * @raises Rien
      *)
      val add_edge : node -> node -> int -> int -> graph -> graph
 
 
     (**
      * @requires Rien
      * @ensures Retire un sommet (premier paramètre) d'un graphe (deuxième paramètre)
      * @raises Rien
      *)
     val remove_vertex : node -> graph -> graph
 
     (**
      * @requires Les deux premiers paramètres (des sommets) appartiennent au troisième paramètre (un graphe), le graphe est non vide
      * @ensures Retire un arc d'un sommet vers un autre (deux premiers paramètres) d'un graphe (troisième paramètre)
      * @raises Rien
      *)
     val remove_edge : node -> node -> graph -> graph
 
 
     (**
      * @requires Rien
      * @ensures Réalise un fold sur les sommets d'un graphe
      * @raises Rien
      *)
     val fold_vertex : (node -> 'a -> 'a) -> graph -> 'a -> 'a
 
     (**
      * @requires Rien
      * @ensures Réalise un fold sur les arcs d'un graphe pondéré
      * @raises Rien
      *)
      val fold_edge : (node -> node -> (int * int) -> 'a -> 'a) -> graph -> 'a -> 'a
  
     (**
     * @requires Rien
     * @ensures Crée un graphe pondéré à partir d'une liste d'arêtes. Chaque arête est un triplet (node1, node2, poids).
     * @raises Rien
     *)
    val create_graph_ponderer : (node*node*int) list -> graph

    (**
     * @requires Rien
     * @ensures Crée un graphe non pondéré(c'est un graphe pondéré mais avec toute les pondérations nul) à partir d'une liste d'arêtes. Chaque arête est un couple (node1, node2).
     * @raises Rien
     *)
    val create_graph_non_ponderer : (node*node) list -> graph

    (**
     * @requires rien
     * @ensures Trouve tous les plus courts chemins entre deux sommets (source et destination) dans un graphe.
     * @raises NoWay
     *)
    val shortest_path : graph -> node -> node -> (node list) list

    (**
     * @requires Rien
     * @ensures Crée un graphe résiduel à partir d'un graphe donné, utilisé dans les calculs de flux réseau.
     * @raises Rien
     *)
    val create_residual_graph : graph -> graph 

    (**
    * @requires Rien
    * @ensures Construit un graphe de niveau à partir d'un graphe résiduel, utilisé dans l'algorithme de Dinic.
    * @raises Rien
    *)
    val build_level_graph : graph -> node -> graph

    (**
    * @requires Un graphe et un chemin dans ce graphe.
    * @ensures Trouve le flux maximal (bloquant) qui peut être envoyé à travers un chemin donné dans le graphe.
    * @raises NoWay
    *)
    val find_blocking_flow : graph -> (node list) -> int 

    (**
    * @requires Un graphe, un chemin dans ce graphe, et un flux à ajouter à ce chemin.
    * @ensures Met à jour le graphe avec le nouveau flux le long du chemin spécifié.
    * @raises Rien
    *)
    val update_graph : graph -> node list -> int -> graph  

    (**
    * @requires Un graphe, un sommet source et un sommet puits.
    * @ensures Applique l'algorithme de Dinic pour trouver le flux maximal du sommet source au sommet puits.
    * @raises NoWay
    *)
    val dinic : graph -> node -> node -> graph 

    (**
    * @requires Un graphe et un sommet source.
    * @ensures Calcule le flux maximal sortant du sommet source dans le graphe.
    * @raises Rien
    *)
    val max_flow : graph ->  node -> int 

    (**
    * @requires Un graphe.
    * @ensures Compte le nombre d'arêtes dans le graphe ayant un flux non nul.
    * @raises Rien
    *)
    val nb_edg_with_flow : graph -> int

    (**
    * @requires Un graphe.
    * @ensures Crée une liste des arêtes avec leur flux dans le graphe.
    * @raises Rien
    *)
    val creat_edg_list : graph -> (node * node * int) list


end
 
 
 (**
  * @brief Un foncteur permettant de renvoyer un module de type Graph dont les noeuds sont du type du module entré en paramètre de ce foncteur
  *)
 module Make(X : OrderedType) : Graph with type node = X.t