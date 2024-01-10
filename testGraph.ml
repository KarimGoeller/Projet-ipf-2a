module G = Graph.Make(String)

let _ =
  Printf.printf "is_empty / empty : %b\n%!" (G.is_empty G.empty)

let _ =
  Printf.printf "is_empty / add_vertex : %b\n%!" ((G.is_empty (G.add_vertex "3" G.empty)) = false)

let _ =
  Printf.printf
    "is_empty / add_edge : %b\n%!"
    ((G.is_empty (G.add_edge "1" "2" 3 5 G.empty)) = false)


let _ =
  Printf.printf "mem_vertex / add_vertex : %b\n%!" ((G.mem_vertex "3" (G.add_vertex "3" G.empty)))

let _ =
  Printf.printf "mem_vertex / add_vertex : %b\n%!" ((G.mem_vertex "2" (G.add_vertex "3" G.empty)) = false)

let _ =
  Printf.printf
    "mem_edge / add_edge : %b\n%!"
    (G.mem_edge "2" "3" (G.add_edge "2" "3" 2 6 G.empty))

let _ =
  Printf.printf
    "mem_edge / add_edge : %b\n%!"
    ((G.mem_edge "2" "2" (G.add_edge "2" "3" 2 6 G.empty)) = false)

let _ =
  Printf.printf
    "mem_vertex / remove_vertex /add_edge : %b\n%!" 
    ((G.mem_vertex "2" (G.remove_vertex "2" (G.add_edge "2" "3" 2 6 G.empty))) = false)

let _ =
  Printf.printf
    "mem_edge / remove_edge /add_edge : %b\n%!"
    ((G.mem_edge "2" "3" (G.remove_edge "2" "3" (G.add_edge "2" "3" 2 6 G.empty))) = false)



