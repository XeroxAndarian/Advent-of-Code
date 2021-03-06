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
  let razbitje_po_presledku str = String.split_on_char ' ' str 

  let kolikokrat_se_pojavi str ch = 
    match str_to_list str with
    | [] -> 0
    | x::xs -> List.length (List.find_all (fun y -> y = ch) (str_to_list str))

    let kolikokrat_se_pojavi_sez sez ch = 
      match sez with
      | [] -> 0
      | x::xs -> List.length (List.find_all (fun y -> y = ch) sez)

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
  let vsebuje_a array n = Array.mem n array 

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

  let reverse_a array = let len=Array.length array in (* source: https://stackoverflow.com/questions/50256693/how-to-reverse-array-in-ocaml *)
    for i=0 to (len/2) do 
        let temp = array.(i) in
        array.(i) <- array.(len-i-1);
        array.(len-i-1) <- temp         
    done;
    array

  let rec drop n h =
      if n == 0 then h else (drop (n-1) (match h with a::b -> b))
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

(*--------------------------------------------------- DAY 11 ------------------------------------------------------------*)
  let datoteka_11_in = "day_11.in"
  let datoteka_11_1_out = "day_11_1.out" 
  let datoteka_11_2_out = "day_11_2.out"

  let matr11 = razbitje (preberi_datoteko datoteka_11_in) '\n'
  let m11 = Array.of_list (List.map Array.of_list (List.map str_to_list matr11))
  
  let pozicija x y = (m11.(y)).(x) 
  let max_x = Array.length (m11.(0)) - 1
  let max_y = Array.length m11 - 1
  (* 
  (0,0) (1,0) (2,0) (3,0) (4,0) ...  (max_x, 0)
  (0,1) (1,1)                           .
  (0,2)       (2,2)                     .
  (0,3)             (3,3)               .
  (0,4)                   (4,4) 
  .
  .
  .                                  
  (0, max_y)  ....                    (max_x, max_y)

  *)

  let prazen x y = (pozicija x y = 'L' || pozicija x y = '.')
  let zaseden x y = (pozicija x y = '#')

  
  let sz x y = prazen (x-1) (y -1)
  let s x y = prazen (x) (y-1)
  let sv x y = prazen (x+1) (y-1)
  let z x y = prazen (x-1) (y)
  let v x y = prazen (x+1) (y)
  let jz x y = prazen (x-1) (y+1)
  let j x y = prazen (x) (y+1)
  let jv x y = prazen (x+1) (y+1)


  let surr x y = 
    if (x=0 && y=0) 
      then [v x y ; j x y ; jv x y]
      else if (y=max_y && x= max_x)
          then [sz x y; s x y; z x y]
          else if (x=max_x && y=0)
            then [z x y; jz x y ; j x y] 
            else if (x=0 && y=max_y)
              then [sz x y; s x y; sv x y]
              else if (x = 0)
                then [s x y; sv x y ; v x y ;  j x y ; jv x y]
                else if x=max_x
                  then  [sz x y; s x y; z x y;jz x y ; j x y]
                  else if y=0
                    then [z x y; v x y ; jz x y ; j x y ; jv x y]
                    else if y=max_y
                      then  [sz x y; s x y; sv x y ; z x y; v x y]
                      else [sz x y; s x y; sv x y ; z x y; v x y ; jz x y ; j x y ; jv x y]
 

  let okoli_prosti x y =
    (kolikokrat_se_pojavi_sez (surr x y) true )

  let postane_prazen x y = 
                        if m11.(x).(y) = '.'
                          then m11.(x).(y) <- '.'
                          else 
                            if okoli_prosti x y < 4
                              then m11.(x).(y) <- 'L'
                              else m11.(x).(y) <- '#' 

  let se_zasede x y = if m11.(x).(y) = '.'
                        then m11.(x).(y) <- '.'
                        else 
                          if okoli_prosti x y != 0  
                            then m11.(x).(y) <- '#'
                            else m11.(x).(y) <- 'L' 
  
  let runda x y = if prazen x y 
                    then se_zasede x y
                    else postane_prazen x y 
  
  let list_x = buildList 0 max_x 
  let list_y = buildList 0 max_y
                  
 

(*--------------------------------------------------- DAY 15 ------------------------------------------------------------*)
  let datoteka_15_in = "day_15.in"
  let datoteka_15_1_out = "day_15_1.out" 
  let datoteka_15_2_out = "day_15_2.out"

  let a15 =Array.of_list (List.map int_of_string (razbitje (preberi_datoteko datoteka_15_in) ','))
  let s15 =List.map int_of_string (razbitje (preberi_datoteko datoteka_15_in) ',')

  let preveri_mesto array n = "neki"

  let rec find_in_array a x n = (* vrne index pozicije prvega elementa *)
    if a.(n) = x 
      then n
      else find_in_array a x (n-1)
    

  let rec next array = 
    let last = (Array.length array - 1) in
    let zadnji = array.(last) in
    let anti_rep = Array.sub array 0 last in
    if last = 2021 then array
      else
        if vsebuje_a  (anti_rep) zadnji
          then next (Array.append array [|last  - (find_in_array anti_rep zadnji (last - 1))|])
          else next  (Array.append array [|0|])

  let t15 = [|0; 3; 6|]
  let tt15 = [|1;2;3;4;5;3;2;3;4;5|]

  let no_2020 array = (next array).(2019)
  let no_30000000 array = (next array).(30000000 - 1)

  let odgovor_15_1 = string_of_int (no_2020 a15)


(*--------------------------------------------------- DAY 16 ------------------------------------------------------------*)
  let datoteka_16_in = "day_16.in"
  let datoteka_16_1_out = "day_16_1.out" 
  let datoteka_16_2_out = "day_16_2.out"

  let raw_s16 = razbitje (preberi_datoteko datoteka_16_in) '\n'
  let r_s16 = List.map razbitje_po_presledku raw_s16
  let as16 = Array.of_list (List.map  (String.split_on_char ',') (drop 25 raw_s16))

  let ad16 = Array.map (List.map int_of_string) as16 
  let a16 = Array.map (Array.of_list) ad16

  let dl n = if ((n >= 33 && n <= 430) || (n >= 456 && n <= 967)) then 0 else n 
  let ds n = if ((n >= 42 && n <= 864) || (n >= 875 && n <= 957)) then 0 else n 
  let dp n = if ((n >= 42 && n <= 805) || (n >= 821 && n <= 968)) then 0 else n 
  let dt n = if ((n >= 34 && n <= 74) || (n >= 93 && n <= 967)) then 0 else n 
  let dd n = if ((n >= 40 && n <= 399) || (n >= 417 && n <= 955)) then 0 else n 
  let dti n = if ((n >= 30 && n <= 774) || (n >= 797 && n <= 950)) then 0 else n
  let al n = if ((n >= 50 && n <= 487) || (n >= 507 && n <= 954)) then 0 else n
  let asn n = if ((n >= 34 && n <= 693) || (n >= 718 && n <= 956)) then 0 else n
  let ap n = if ((n >= 42 && n <= 729) || (n >= 751 && n <= 959)) then 0 else n
  let at n = if ((n >= 28 && n <= 340) || (n >= 349 && n <= 968)) then 0 else n
  let cl n = if ((n >= 49 && n <= 524) || (n >= 543 && n <= 951)) then 0 else n
  let du n = if ((n >= 40 && n <= 372) || (n >= 397 && n <= 951)) then 0 else n
  let pr n = if ((n >= 48 && n <= 922) || (n >= 939 && n <= 951)) then 0 else n
  let ro n = if ((n >= 33 && n <= 642) || (n >= 666 && n <= 960)) then 0 else n
  let rw n = if ((n >= 39 && n <= 238) || (n >= 255 && n <= 973)) then 0 else n
  let st n = if ((n >= 48 && n <= 148) || (n >= 161 && n <= 973)) then 0 else n
  let tr n = if ((n >= 50 && n <= 604) || (n >= 630 && n <= 971)) then 0 else n
  let ty n = if ((n >= 29 && n <= 299) || (n >= 316 && n <= 952)) then 0 else n
  let wg n = if ((n >= 45 && n <= 898) || (n >= 921 && n <= 966)) then 0 else n
  let zn n = if ((n >= 34 && n <= 188) || (n >= 212 && n <= 959)) then 0 else n

  let all n = if ((n >= 29 && n <= 922) || (n >= 93 && n <= 968)) then 0 else n
  
  let preveri array = 
    if Array.length array = 20 
      then [| all array.(0);
     all array.(1); 
     all array.(2); 
     all array.(3); 
     all array.(4);  
     all array.(5); 
     all array.(6); 
     all array.(7); 
     all array.(8); 
     all array.(9); 
     all array.(10); 
     all array.(11);
     all array.(12);
     all array.(13);
     all array.(14);
     all array.(15);
     all array.(16);
     all array.(17);
     all array.(18);
     all array.(19) |]
    else [| 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0|]

  let preveri_za_vse array = Array.map preveri array

  let rec vsota sez = 
    match sez with
    | [] -> 0
    | x::xs -> x + vsota xs 

  let vsota_a array = vsota (Array.to_list array)
  
  let preveri_a16 = Array.map preveri a16
  let sum_a16 = Array.map vsota_a preveri_a16
  let sum_sum_a16 = vsota_a sum_a16 

  let odgovor_16_1 = string_of_int sum_sum_a16
(*--------------------------------------------------- Generator ------------------------------------------------------------*)
  
  let _ = 
      izpisi_datoteko datoteka_9_1_out odgovor_9_1_str;
      izpisi_datoteko datoteka_10_1_out odgovor_10_1_str;
      izpisi_datoteko datoteka_15_1_out odgovor_15_1;
      izpisi_datoteko datoteka_16_1_out odgovor_16_1;
      