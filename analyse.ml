
let rec parse_line_1 n cin =
  try
    let s = input_line cin in
    let a,b = Scanf.sscanf s "%s %s" (fun a b -> a,b) in 
    (a,b)::parse_line_1 (n+1) cin
  with End_of_file -> []



    
let rec parse_line_2 n cin =
  try
    let s = input_line cin in 
    let a,b,c = Scanf.sscanf s "%s %s %d" (fun a b c -> a,b,c) in 
    (a,b,c)::parse_line_2 (n+1) cin
  with End_of_file -> []

let phase1 () =
    if Array.length Sys.argv <> 2 
    then 
      let _ = Format.fprintf Format.err_formatter "usage : %s <file>@." Sys.argv.(0) in
      assert false
    else
  let cin = open_in Sys.argv.(1) in
  let src = input_line cin in
  let dst = input_line cin in
  let len = input_line cin in
  let n = int_of_string len in
  let le = parse_line_1 3 cin in
  let _ = assert (n=List.length le) in
  let _ = close_in cin in
  (src,dst,le)     
  
  

  
let phase2 () =
    if Array.length Sys.argv <> 2 
    then 
      let _ = Format.fprintf Format.err_formatter "usage : %s <file>@." Sys.argv.(0) in
      assert false
    else
    let cin = open_in Sys.argv.(1) in
    let src = input_line cin in
    let dst = input_line cin in
    let _ = input_line cin in
    let le = parse_line_2 3 cin in
    let _ = close_in cin in
    (src,dst,le)