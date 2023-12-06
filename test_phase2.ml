

module G = Graph.Make(String);;



let data = [("s","1",0,10);("s","2",0,10);("1","3",0,4);("1","4",0,8);("1","2",0,2);
("2","4",0,9);("4","t",0,10);("4","3",0,6);("3","t",0,10)]

let graph_test = G.create_graph_ponderer data ;;

let residual_graph = G.create_residual_graph graph_test;;

let level_graph = G.build_level_graph residual_graph;;

let _ = G.print_graph graph_test;;