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

     (*val to_string : t -> string*)
     
 
 end
 
 
 (**
  * @brief Un module type Graph pour représenter des graphes non pondérés (Le sujet ne spécifie pas qu'ils seront pondérés)
  *)
 module type Graph =
 sig
 
 
     (**
      * @brief Un module permettant de représenter un ensemble de successeurs de sommets dans un graphe avec leurs étiquettes, nécessaire pour la fonction succs
               (pour les graphes non pondérés)
      * @see succs
      *)
     module NodeSet : Set.S
 
      (**
      * @brief Un module permettant de représenter un ensemble de successeurs de sommets dans un graphe avec leurs étiquettes, nécessaire pour la fonction succs
               (pour les graphes pondérés)
      * @see succs
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
      val succs_Set : node -> graph -> NodeSet.t(** "int NodeMap.t" au lieu de NodeSet.t si les graphes étaient orientés **) 

      val succs_Map : node -> graph -> (int * int) NodeMap.t
 
     (**
      * @requires Rien
      * @ensures Indique si un sommet (premier paramètre) est présent dans un graphe (deuxième paramètre)
      * @raises Rien
      *)
     val mem_vertex : node -> graph -> bool
 
     (**
      * @requires Rien
      * @ensures Indique si un arc d'un sommet (premier paramètre) à un autre (deuxième paramètre) avec pour poids le troisième paramètre est présent dans un graphe (quatrième paramètre)
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
      * @requires Les deux premiers paramètres (des sommets) appartiennent au quatrième paramètre (un graphe), le graphe est non vide
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
      * @ensures Renvoie un graphe correspondant à la liste d'arête donnée en paramètre
      * @raises Rien
      *)
      val create_graph_ponderer : (node*node*int) list -> graph

      val create_graph_non_ponderer : (node*node) list -> graph

    

     val floyd_warshall_paths : graph -> node -> node -> (node list) list

     val create_residual_graph : graph -> graph 
  
     val build_level_graph : graph -> node -> graph
     
     val find_blocking_flow : graph -> (node list) -> int 

     val update_graph : graph -> node list -> int -> graph  

     val dinic : graph -> node -> node -> graph 

     val max_flow : graph ->  node -> int 

     val nb_edg_with_flow : graph -> int

     val creat_edg_list : graph -> (node * node * int) list
  
    (*val print_graph : graph -> unit*)

 
end
 
 
 (**
  * @brief Un foncteur permettant de renvoyer un module de type Graph dont les noeuds sont du type du module entré en paramètre de ce foncteur
  *)
 module Make(X : OrderedType) : Graph with type node = X.t