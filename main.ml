open Yojson.Basic
open Yojson.Basic.Util

let debug = false
       
(******* Useful functions and constants*******)
let distance (x,y) (x',y') =
  sqrt( (x-.x')**2. +. (y-.y')**2.)

let d = distance
	  
let aire a b c =
  let da, db, dc = d b c, d a c, d a b in
  let p = (da +. db +. dc)/. 2.
  in
  sqrt (p *. (p-.da) *. (p-.db) *. (p-.dc))
       
let rayonCercleCirconscrit a b c =
  (d a b) *. (d b c) *. (d a c) /. (4. *. (aire a b c))

let const_pi = 2. *. (acos 0.)

let angle a b c =
  let xa,ya = a
  and xb,yb = b
  and xc,yc = c
  in
  acos (
      ((xa-.xb)*.(xc-.xb) +. (ya-.yb)*.(yc-.yb))
      /.
	((d b a) *. (d b c))
    )


let quadrant (x,y) =
  match x>=0., y>=0. with
  | true, true -> 0
  | true,false -> if x = 0. then 2 else 3
  | false,true -> 1
  |false,false -> 2
		    

let number_quadrant i q_pts pts goal=
  let quads = [|false; false; false; false|] in
  let current = ref 0 in
  let rec number_quadrantRec k =
    if k = q_pts
    then !current >= goal
    else (let q = quadrant pts.(i+k)
	  in if (not(quads.(q)))
	     then (quads.(q)<-true; incr current);
	     if (!current >= goal)
	     then true
	     else number_quadrantRec (k+1))
  in
  number_quadrantRec 0
		     

let print s =
  if debug then print_endline s








(******* The real stuff ***************)
		     
let getFile () =
  try 
    Sys.argv.(1)
  with
    _ -> print_endline "Error : No file given"; exit 0

    
let parse_json json_file =
  let numpoints = to_int (member "NUMPOINTS" json_file) in
  let points = Array.map (fun l -> to_number (List.hd (to_list l)), to_number (List.hd (List.tl (to_list l)))) (Array.of_list (to_list (member "points" json_file))) in
  let parameters = member "PARAMETERS" json_file in
  let lcm = Array.init 15 (fun i -> Array.map to_string (Array.of_list (to_list (member (string_of_int i) (member "LCM" json_file))))) in
  let puv = Array.map to_bool (Array.of_list (to_list (member "PUV" json_file))) in

  numpoints, points, parameters, lcm, puv

					
let decide () =
  let json = from_file (getFile ())
  in
  let numpoints, points, parameters, lcm, puv = parse_json json
  in
  
  (*** calculate CMV ***)
  
  let cmv = Array.make 15 false in

  let find i f cond =
    let rec findRec n =
      (n <= f) && ((cond n) || findRec (n+1))
    in
    findRec i
  in
  
  (* cond 0 *)
  let length1 = to_number (member "LENGTH1" parameters)
  in
  cmv.(0) <- find 0 (numpoints-2) (fun i -> distance points.(i) points.(i+1) > length1);
  
  (* cond 1 *)
  let radius1 = to_number (member "RADIUS1" parameters)
  in
  cmv.(1) <- find 0 (numpoints-3) (fun i -> rayonCercleCirconscrit points.(i) points.(i+1) points.(i+2) > radius1);
  
  (* cond 2 *)
  let epsilon = to_number (member "EPSILON" parameters)
  in
  cmv.(2) <- find 0 (numpoints-3) (fun i -> let p1,p2,p3 = points.(i), points.(i+1), points.(i+2)
					    in p1 <> p2 && p2 <> p3 && let alpha = angle p1 p2 p3
								       in alpha < const_pi -. epsilon
									  || alpha > const_pi +. epsilon);
  
  (* cond 3 *)
  let area1 = to_number (member "AREA1" parameters)
  in
  cmv.(3) <- find 0 (numpoints-3) (fun i -> aire points.(i) points.(i+1) points.(i+2) > area1);
  
  (* cond 4 *)
  let q_pts = (to_int (member "Q_PTS" parameters))
  and quads = (to_int (member "QUADS" parameters))
  in
  cmv.(4) <- find 0 (numpoints-q_pts) (fun i -> number_quadrant i q_pts points quads);
  
  (* cond 5 *)

  cmv.(5) <- find 0 (numpoints-2) (fun i -> let (x,y) = points.(i) and (x',y') = points.(i+1) in x' -. x < 0.);
  
  (* cond 6 *)
  let n_pts = to_int (member "N_PTS" parameters)
  and dist = to_number (member "DIST" parameters)
  in
  cmv.(6) <- numpoints >= 3 && find 0 (numpoints - n_pts) (fun i -> let pi = points.(i)
								    and pf = points.(i + n_pts - 1)
								    in find i (i+n_pts-1) (fun j -> if pi = pf
												    then d pi points.(j) > dist
												    else ( abs_float( (d pi points.(j)) *. sin (angle pf points.(j) pi)) > dist)));
  
  (* cond 7 *)
  let k_pts = to_int (member "K_PTS" parameters)
  and length1 = to_number (member "LENGTH1" parameters)
  in
  cmv.(7) <- numpoints >= 3 && find 0 (numpoints-2-k_pts) (fun i -> distance points.(i) points.(i+1+k_pts) > length1);

  (* cond 8 *)
  let a_pts = to_int (member "A_PTS" parameters)
  and b_pts = to_int (member "B_PTS" parameters)
  and radius1 = to_number (member "RADIUS1" parameters)
  in
  cmv.(8) <- numpoints >= 5 && find 0 (numpoints-3-a_pts-b_pts) (fun i -> rayonCercleCirconscrit points.(i) points.(i+1+a_pts) points.(i+2+a_pts+b_pts) > radius1);
  
  (* cond 9 *)
  let c_pts = to_int (member "C_PTS" parameters)
  and d_pts = to_int (member "D_PTS" parameters)
  and epsilon = to_number (member "EPSILON" parameters)
  in
  cmv.(9) <- numpoints >= 5 && find 0 (numpoints-3-c_pts-d_pts) (fun i -> let p1,p2,p3 = points.(i), points.(i+1+c_pts), points.(i+2+c_pts+d_pts)
									  in p1 <> p2 && p2 <> p3 && let alpha = angle p1 p2 p3
												     in alpha < const_pi -. epsilon
													|| alpha > const_pi +. epsilon);
  
  (* cond 10 *)
  let e_pts = to_int (member "E_PTS" parameters)
  and f_pts = to_int (member "F_PTS" parameters)
  and area1 = to_number (member "AREA1" parameters)
  in
  cmv.(10) <- numpoints >= 5 && find 0 (numpoints-3-e_pts-f_pts) (fun i -> aire points.(i) points.(i+1+e_pts) points.(i+2+e_pts+f_pts) > area1);

  
  (* cond 11 *)
  let g_pts = to_int (member "G_PTS" parameters)
  in
  cmv.(11) <- numpoints >= 3 && find 0 (numpoints-2-g_pts) (fun i -> let (x,y) = points.(i) and (x',y') = points.(i+1+g_pts) in x' -. x < 0.);
  
  (* cond 12 *)
  let k_pts = to_int (member "K_PTS" parameters)
  and length1 = to_number (member "LENGTH1" parameters)
  and length2 = to_number (member "LENGTH2" parameters)
  in
  cmv.(12) <- numpoints >= 3
	      && find 0 (numpoints - k_pts-1) (fun i -> d points.(i) points.(i+k_pts) > length1)
	      && find 0 (numpoints - k_pts-1) (fun i -> d points.(i) points.(i+k_pts) < length2);
  
  (* cond 13 *)
  let a_pts = to_int (member "A_PTS" parameters)
  and b_pts = to_int (member "B_PTS" parameters)
  and radius1 = to_number (member "RADIUS1" parameters)
  and radius2 = to_number (member "RADIUS2" parameters)
  in
  cmv.(13) <- numpoints >= 5
	      && find 0 (numpoints-3-a_pts-b_pts) (fun i -> rayonCercleCirconscrit points.(i) points.(i+1+a_pts) points.(i+2+a_pts+b_pts) > radius1)
	      && find 0 (numpoints-3-a_pts-b_pts) (fun i -> rayonCercleCirconscrit points.(i) points.(i+1+a_pts) points.(i+2+a_pts+b_pts) < radius2);

  
  (* cond 14 *)
  let e_pts = to_int (member "E_PTS" parameters)
  and f_pts = to_int (member "F_PTS" parameters)
  and area1 = to_number (member "AREA1" parameters)
  and area2 = to_number (member "AREA2" parameters)
  in
  cmv.(14) <- numpoints >= 5
	      && find 0 (numpoints-3-e_pts-f_pts) (fun i -> aire points.(i) points.(i+1+e_pts) points.(i+2+e_pts+f_pts) > area1)
	      && find 0 (numpoints-3-e_pts-f_pts) (fun i -> aire points.(i) points.(i+1+e_pts) points.(i+2+e_pts+f_pts) < area2);

  (*** calculate PUM ***)
  let pum = Array.init 15 (fun i ->
			   Array.init 15 (fun j ->
					  match lcm.(i).(j) with
					  |"NOTUSED" -> true
					  |"ANDD" -> cmv.(i) && cmv.(j)
					  |"ORR" -> cmv.(i) || cmv.(j)
					  |connector -> failwith ("found invalid connector : "^connector^" in argument LCM")
					 )
			  )
  in
  
  (*** calculate FUV ***)
  let allTrue t =
    let rec allTrueRec i =
      i = Array.length t || (t.(i) && allTrueRec (i+1))
    in
    allTrueRec 0
  in
  
  let fuv = Array.init 15 (fun i ->
			   not(puv.(i)) || allTrue pum.(i)
			  )
  in
  
  (*** calculate launch ***)
  let launch = allTrue fuv
  in

  launch, cmv, pum, fuv



let launch, cmv, pum, fuv = decide ()

let () = print_string (if launch then "YES" else "NO")	       
	      


