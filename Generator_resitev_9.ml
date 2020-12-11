(*--------------------------------------------------- Gadgets ------------------------------------------------------------*)
  #load "str.cma";;
  

  let preberi_datoteko ime_datoteke =
    let chan = open_in ime_datoteke in
    let vsebina = really_input_string chan (in_channel_length chan) in
    close_in chan;
    vsebina

  let  izpisi_datoteko ime_datoteke vsebina =
          let chan = open_out ime_datoteke in
          output_string chan vsebina;
          close_out chan

  let str_to_list s =        (* source: https://stackoverflow.com/questions/9863036/ocaml-function-parameter-pattern-matching-for-strings  *)
    let rec expl i l =
      if i < 0 then l else
      expl (i - 1) (s.[i] :: l) in
    expl (String.length s - 1) [];;close_in

  let rec everyEven a =   (* source: https://stackoverflow.com/questions/28360588/returning-every-other-element-of-a-list-in-ocaml *)
    match a with
    |[]-> []
    |[_] -> []
    |x::y::t -> y::everyEven t 

  let razbitje str znak = String.split_on_char znak str 

  let kolikokrat_se_pojavi str ch = 
    match str_to_list str with
    | [] -> 0
    | x::xs -> List.length (List.find_all (fun y -> y = ch) (str_to_list str))

  let contains_substring str sub_str = (* source: https://stackoverflow.com/questions/8373460/substring-check-in-ocaml *)
    let re = Str.regexp_string sub_str
    in
        try ignore (Str.search_forward re str 0); true
        with Not_found -> false

  let rec maximum list =  (* source of inspiration : https://www.allegro.cc/forums/thread/598795 *)
    match list with
    | [] -> 0
    | x :: [] -> x
    | x :: xs -> 
          let v = maximum xs in
          if x > v then x else v

  let rec minimum list =  (* source : https://www.allegro.cc/forums/thread/598795 *)
    match list with
    | [] -> 0
    | x :: [] -> x
    | x :: xs -> 
          let v = minimum xs in
          if x < v then x else v
  
  let vsebuje sez n = List.mem (string_of_int n) sez 
  let vsebuje_2 sez n = List.mem n sez 

  let buildList i n =   (* source: https://stackoverflow.com/questions/5653739/building-a-list-of-ints-in-ocaml *)
    let rec aux acc i =
      if i <= n then
        aux (i::acc) (i+1)
      else (List.rev acc)
    in
    aux [] i

  let rec makeList i = (if i = 0 then [] else [0] @ makeList (i-1)) (* source: https://stackoverflow.com/questions/36568189/ocaml-how-to-create-a-list-of-n-1s-in/36568502*)
  let rec replaceelem ls x elem =  (* source: https://stackoverflow.com/questions/37091784/ocaml-function-replace-a-element-in-a-list *)
    match ls with
    | [] -> ls
    | h::t -> if (x=0) then
                elem::(replaceelem t (x-1) elem)
              else
                h::(replaceelem t (x-1) elem) 


  let list_diff l1 l2 = List.filter (fun x -> not (List.mem x l2)) l1 (* source: https://stackoverflow.com/questions/22132458/library-function-to-find-difference-between-two-lists-ocaml *)
  let list_equal l1 l2 = List.filter (fun x -> (List.mem x l2)) l1
  let list_equal_2 l1 l2 = 
    if l1 = [] 
      then l2
      else if l2 = []
        then l1
        else List.filter (fun x -> (List.mem x l2)) l1

  let replace l pos a  = List.mapi (fun i x -> if i = pos then a else x) l (* source: https://stackoverflow.com/questions/37091784/ocaml-function-replace-a-element-in-a-list *)

  let rec find x lst =
    match lst with
    | [] -> -1 
    | h :: t -> if x = h then 0 else 1 + find x t


(*--------------------------------------------------- DAY 9 ------------------------------------------------------------*)
  let datoteka_9_in = "day_9.in"
  let datoteka_9_1_out = "day_9_1.out" 
  let datoteka_9_2_out = "day_9_2.out" 

  let je_vsota_dveh_stevil_izmed_dveh_od_prej seznam =
    match seznam with
    | [] -> false
    | [a; b; c; d; e; f; g; h; i; j; k; l; m; n; o; p; r; s; t; u; v; z; x; y; w; q] -> 
      (vsebuje_2 [b; c; d; e; f; g; h; i; j; k; l; m; n; o; p; r; s; t; u; v; z; x; y; w] (q - a ) ||
      vsebuje_2 [a; c; d; e; f; g; h; i; j; k; l; m; n; o; p; r; s; t; u; v; z; x; y; w] (q - b  ) ||
      vsebuje_2 [a; b; d; e; f; g; h; i; j; k; l; m; n; o; p; r; s; t; u; v; z; x; y; w] (q - c ) ||
      vsebuje_2 [a; b; c; e; f; g; h; i; j; k; l; m; n; o; p; r; s; t; u; v; z; x; y; w] (q - d ) ||
      vsebuje_2 [a; b; c; d; f; g; h; i; j; k; l; m; n; o; p; r; s; t; u; v; z; x; y; w] (q - e ) ||
      vsebuje_2 [a; b; c; d; e; g; h; i; j; k; l; m; n; o; p; r; s; t; u; v; z; x; y; w] (q - f ) ||
      vsebuje_2 [a; b; c; d; e; f; h; i; j; k; l; m; n; o; p; r; s; t; u; v; z; x; y; w] (q - g ) ||
      vsebuje_2 [a; b; c; d; e; f; g; i; j; k; l; m; n; o; p; r; s; t; u; v; z; x; y; w] (q - h ) ||
      vsebuje_2 [a; b; c; d; e; f; g; h; j; k; l; m; n; o; p; r; s; t; u; v; z; x; y; w] (q - i ) ||
      vsebuje_2 [a; b; c; d; e; f; g; h; i; k; l; m; n; o; p; r; s; t; u; v; z; x; y; w] (q - j ) ||
      vsebuje_2 [a; b; c; d; e; f; g; h; i; j; l; m; n; o; p; r; s; t; u; v; z; x; y; w] (q - k ) ||
      vsebuje_2 [a; b; c; d; e; f; g; h; i; j; k; m; n; o; p; r; s; t; u; v; z; x; y; w] (q - l ) ||
      vsebuje_2 [a; b; c; d; e; f; g; h; i; j; k; l; n; o; p; r; s; t; u; v; z; x; y; w] (q - m ) ||
      vsebuje_2 [a; b; c; d; e; f; g; h; i; j; k; l; m; o; p; r; s; t; u; v; z; x; y; w] (q - n ) ||
      vsebuje_2 [a; b; c; d; e; f; g; h; i; j; k; l; m; n; p; r; s; t; u; v; z; x; y; w] (q - o ) ||
      vsebuje_2 [a; b; c; d; e; f; g; h; i; j; k; l; m; n; o; r; s; t; u; v; z; x; y; w] (q - p ) ||
      vsebuje_2 [a; b; c; d; e; f; g; h; i; j; k; l; m; n; o; p; s; t; u; v; z; x; y; w] (q - r ) ||
      vsebuje_2 [a; b; c; d; e; f; g; h; i; j; k; l; m; n; o; p; r; t; u; v; z; x; y; w] (q - s ) ||
      vsebuje_2 [a; b; c; d; e; f; g; h; i; j; k; l; m; n; o; p; r; s; u; v; z; x; y; w] (q - t ) ||
      vsebuje_2 [a; b; c; d; e; f; g; h; i; j; k; l; m; n; o; p; r; s; t; v; z; x; y; w] (q - u ) ||
      vsebuje_2 [a; b; c; d; e; f; g; h; i; j; k; l; m; n; o; p; r; s; t; u; z; x; y; w] (q - v ) ||
      vsebuje_2 [a; b; c; d; e; f; g; h; i; j; k; l; m; n; o; p; r; s; t; u; v; x; y; w] (q - z ) ||
      vsebuje_2 [a; b; c; d; e; f; g; h; i; j; k; l; m; n; o; p; r; s; t; u; v; z; y; w] (q - x ) ||
      vsebuje_2 [a; b; c; d; e; f; g; h; i; j; k; l; m; n; o; p; r; s; t; u; v; z; x; w] (q - y ) ||
      vsebuje_2 [a; b; c; d; e; f; g; h; i; j; k; l; m; n; o; p; r; s; t; u; v; z; x; y; ] (q - w ) )
      
                              
    | _ -> false
                              
  let preveri_na_mestu seznam x =
    let array = Array.of_list seznam in
    je_vsota_dveh_stevil_izmed_dveh_od_prej [
      array.(x - 26);
      array.(x - 25);
      array.(x - 24);
      array.(x - 23);
      array.(x - 22);
      array.(x - 21);
      array.(x - 20);
      array.(x - 19);
      array.(x - 18);
      array.(x - 17);
      array.(x - 16);
      array.(x - 15);
      array.(x - 14);
      array.(x - 13);
      array.(x - 12);
      array.(x - 11);
      array.(x - 10);
      array.(x - 9);
      array.(x - 8);
      array.(x - 7); 
      array.(x - 6); 
      array.(x - 5); 
      array.(x - 4); 
      array.(x - 3); 
      array.(x - 2); 
      array.(x - 1)]

  let t9 =  [11;1;33;5;20;31]

  let s9 =List.map (int_of_string) (razbitje (preberi_datoteko datoteka_9_in) '\n')
  
  let preveri_za_vsote seznam = List.map (preveri_na_mestu seznam) (buildList 26 (List.length seznam ))
  
  let index = find false (preveri_za_vsote s9) + 25
  let array_sez = Array.of_list s9
    

  let odgovor_9_1 = array_sez.(index)
  let odgovor_9_1_str = string_of_int odgovor_9_1
(*--------------------------------------------------- DAY 10 ------------------------------------------------------------*)
  let datoteka_10_in = "day_10.in"
  let datoteka_10_1_out = "day_10_1.out" 
  let datoteka_10_2_out = "day_10_2.out" 

  let s10 = List.sort compare (List.map int_of_string (razbitje (preberi_datoteko datoteka_10_in) '\n'))

  let jolt_jump_1  seznam  st = vsebuje_2 seznam (st + 1)
  let jolt_jump_2  seznam  st = vsebuje_2 seznam (st + 2)
  let jolt_jump_3  seznam  st = vsebuje_2 seznam (st + 3)
  
  let rec sestej_seznama_po_kompponentah sez1 sez2 = 
    match sez1, sez2 with
    | [], [] -> []
    | x::xs, y::ys -> [x + y] @ sestej_seznama_po_kompponentah xs ys
    | [], y::ys -> y::ys
    | x::xs, [] -> x::xs  

  let rec preveri_jolt_jump seznam trenutni_count = 
    match  seznam with 
    | [] -> trenutni_count
    | x::xs  -> if (jolt_jump_1 xs x) 
                  then preveri_jolt_jump xs (sestej_seznama_po_kompponentah trenutni_count [1; 0])
                  else preveri_jolt_jump xs (sestej_seznama_po_kompponentah trenutni_count [0; 1])   
  let test1 = List.sort compare [28; 33; 18; 42; 31; 14; 46; 20; 48; 47; 24; 23; 49; 45; 19; 38; 39; 11; 1; 32; 25; 35; 8; 17; 7; 9; 4; 2; 34; 10; 3]
  let rec zmnozi_elemente_seznama seznam = 
    match seznam with
    | [] -> 1
    | x::xs -> x * zmnozi_elemente_seznama xs
  
  let odgovor_10_1 = zmnozi_elemente_seznama (preveri_jolt_jump s10 [1;0])
  let odgovor_10_1_str = string_of_int odgovor_10_1



(*--------------------------------------------------- Generator ------------------------------------------------------------*)
  
  let _ = 
      izpisi_datoteko datoteka_9_1_out odgovor_9_1_str;
      izpisi_datoteko datoteka_10_1_out odgovor_10_1_str