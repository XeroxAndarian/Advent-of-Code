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
  let vsebuje_2 sez n = List.mem n sez (* n in str form*)

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

(*--------------------------------------------------- DAY 1 ------------------------------------------------------------*)

  let datoteka_1_in = "day_1.in"
  let datoteka_1_1_out = "day_1_1.out" 
  let datoteka_1_2_out = "day_1_2.out" 

  let test_list = ["1721"; "979"; "366"; "299"; "675"; "1456"]

  let list = String.split_on_char '\n' (preberi_datoteko datoteka_1_in)

  

  let rec zmnozek sez = 
    match sez with
    | [] -> 1
    | x::xs -> x * zmnozek xs

  let rec zdskvj vsota seznam = (* zdskvj = zmnozek dveh stevil, katerih vsota je *)
    match seznam with
    | [] -> -1
    | x::rest -> 
      let nadomestek = vsota - int_of_string x in
      if vsebuje rest nadomestek then zmnozek  [nadomestek; int_of_string x] else zdskvj vsota rest 

  let rec v3sis vsota seznam  = (* vsota_treh_stevil_iz_seznama_je_enaka *)     
    match seznam with 
    | [] -> -1
    | x::xs -> 
      let nadomestek = vsota - int_of_string x in
      if zdskvj nadomestek xs = -1 then v3sis vsota xs else int_of_string x * zdskvj nadomestek xs



  let odgovor_1_1 = string_of_int (zdskvj 2020 list)
  let odgovor_1_2 = string_of_int (v3sis 2020 list)




(*--------------------------------------------------- DAY 2 ------------------------------------------------------------*)
  let datoteka_2_in = "day_2.in"
  let datoteka_2_1_out = "day_2_1.out" 
  let datoteka_2_2_out = "day_2_2.out" 

  type password = {pojavitev_min: int; pojavitev_max: int; znak: char; geslo: string}

  let razbitje_po_odstavkih str = String.split_on_char '\n' str 
  let razbitje_po_presledkih str = String.split_on_char ' ' str 
  let razbitje_po_pomisljaju str = String.split_on_char '-' str



  let razvrstitev seznam =
    match seznam with
    | [x; y; z] -> {
      pojavitev_min= int_of_string (List.hd (razbitje_po_pomisljaju x)); 
      pojavitev_max= int_of_string (List.nth (razbitje_po_pomisljaju x) 1 ); 
      znak=y.[0];
      geslo=z}
    | _::_ -> {pojavitev_min=0; pojavitev_max=0; znak='0'; geslo="Prazno"}
    | [] -> {pojavitev_min=0; pojavitev_max=0; znak='0'; geslo="Prazno"}



  let kolikokrat_se_pojavi_list seznam vrednost = 
    match seznam with
    | [] -> 0
    | x::xs -> List.length (List.find_all (fun y -> y = vrednost) seznam)

  let koda_ima_vec_pojavitev str ch n = kolikokrat_se_pojavi str ch >= n 
  let koda_ima_manj_pojavitev str ch n = kolikokrat_se_pojavi str ch <= n 

  let test1 = {pojavitev_min= 4; pojavitev_max= 5; znak= 't'; geslo= "ftttttrvts"}
  let test2 = {pojavitev_min= 4; pojavitev_max= 5; znak= 't'; geslo= "ftaaaaarvs"}

  let ali_je_geslo_primerno pass = (koda_ima_manj_pojavitev pass.geslo pass.znak pass.pojavitev_max && koda_ima_vec_pojavitev pass.geslo pass.znak pass.pojavitev_min) 


  let koliko_gesel_je_primernih seznam = kolikokrat_se_pojavi_list (List.map ali_je_geslo_primerno seznam) true

  let seznam_pass = List.map razvrstitev (List.map razbitje_po_presledkih (razbitje_po_odstavkih (preberi_datoteko datoteka_2_in)))  


  let pojavi_se_na_mestih pass = 
    match str_to_list pass.geslo with
    | [] -> false
    | _ -> (List.nth (str_to_list pass.geslo) (pass.pojavitev_min - 1) = pass.znak) <> (List.nth (str_to_list pass.geslo) (pass.pojavitev_max - 1) = pass.znak) 



  let odgovor_2_1 = string_of_int (koliko_gesel_je_primernih (seznam_pass))
  let odgovor_2_2 = string_of_int (kolikokrat_se_pojavi_list (List.map pojavi_se_na_mestih seznam_pass) true )


(*--------------------------------------------------- DAY 3 ------------------------------------------------------------*)
  let datoteka_3_in = "day_3.in"
  let datoteka_3_1_out = "day_3_1.out" 
  let datoteka_3_2_out = "day_3_2.out" 

  let test1 = ['.'; '.'; '.'; '#'; '#'; '.'; '.'; '#';'.'; '.']


  let seznam_dreves_po_vrsticah = razbitje_po_odstavkih (preberi_datoteko datoteka_3_in)

  let perioda = List.length (str_to_list (List.hd seznam_dreves_po_vrsticah))

  let je_drevo mesto seznam = (if mesto = 0 then (List.nth seznam ((List.length seznam) - 1) = "#".[0]) else (List.nth seznam (mesto - 1) = "#".[0]))

  let mesto_v_vrstici mesto = mesto mod perioda 

  let vrstica_v_mesto vrstica = 3 * (vrstica - 1) + 1 

  let zadanem_drevo seznam_dreves = 
    List.map2 
    je_drevo
    (List.map mesto_v_vrstici (List.map  vrstica_v_mesto(buildList 1 (List.length seznam_dreves))))
    (List.map str_to_list seznam_dreves)

  
  let vrstica_v_mesto_1 vrstica =  (vrstica - 1) + 1
  let vrstica_v_mesto_5 vrstica =  5 * (vrstica - 1) + 1
  let vrstica_v_mesto_7 vrstica =  7 * (vrstica - 1) + 1

  let zadanem_drevo_1  seznam_dreves = 
    List.map2 
    je_drevo
    (List.map mesto_v_vrstici (List.map  vrstica_v_mesto_1 (buildList 1 (List.length seznam_dreves))))
    (List.map str_to_list seznam_dreves)

    let zadanem_drevo_5  seznam_dreves = 
    List.map2 
    je_drevo
    (List.map mesto_v_vrstici (List.map  vrstica_v_mesto_5 (buildList 1 (List.length seznam_dreves))))
    (List.map str_to_list seznam_dreves)

    let zadanem_drevo_7  seznam_dreves = 
    List.map2 
    je_drevo
    (List.map mesto_v_vrstici (List.map  vrstica_v_mesto_7 (buildList 1 (List.length seznam_dreves))))
    (List.map str_to_list seznam_dreves)

    let zadanem_drevo_preskoci  seznam_dreves = 
    (List.map2 
    je_drevo
    (List.map mesto_v_vrstici (List.map  vrstica_v_mesto_1 (buildList 1 ((List.length seznam_dreves)/2 + 1) )))
    (everyEven ([['a']] @ (List.map str_to_list seznam_dreves))))

  let odgovor_3_2_1 =  kolikokrat_se_pojavi_list (zadanem_drevo_1 seznam_dreves_po_vrsticah) true
  let odgovor_3_2_5 =  kolikokrat_se_pojavi_list (zadanem_drevo_5 seznam_dreves_po_vrsticah) true
  let odgovor_3_2_7 =  kolikokrat_se_pojavi_list (zadanem_drevo_7 seznam_dreves_po_vrsticah) true
  let odgovor_3_2_preskoci =  kolikokrat_se_pojavi_list (zadanem_drevo_preskoci seznam_dreves_po_vrsticah) true

  let odgovor_3_1 = string_of_int (kolikokrat_se_pojavi_list (zadanem_drevo seznam_dreves_po_vrsticah) true)
  let odgovor_3_2 = string_of_int (odgovor_3_2_1 * odgovor_3_2_5 * odgovor_3_2_7 * odgovor_3_2_preskoci *  (int_of_string odgovor_3_1))


(*--------------------------------------------------- DAY 4 ------------------------------------------------------------*)
  let datoteka_4_in = "day_4.in"
  let datoteka_4_1_out = "day_4_1.out" 
  let datoteka_4_2_out = "day_4_2.out" 

  let umetni_passport = "
  ecl:gry pid:860033327 eyr:2020 hcl:#fffffd
  byr:1937 iyr:2017 cid:147 hgt:183cm
  
  iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884
  hcl:#cfa07d byr:1929
  
  hcl:#ae17e1 iyr:2013
  eyr:2024
  ecl:brn pid:760753108 byr:1931
  hgt:179cm
  
  hcl:#cfa07d eyr:2025 pid:166559648
  iyr:2011 ecl:brn hgt:59in "

  
  let spremeni_input4_v_seznam niz = Str.split (Str.regexp "\n\n") niz

  let preveri_validnost_passporta niz = (
      contains_substring niz "byr" &&
      contains_substring niz "iyr" && 
      contains_substring niz "eyr" && 
      contains_substring niz "hcl" && 
      contains_substring niz "hgt" && 
      contains_substring niz "ecl" &&
      contains_substring niz "pid")

  let validnost_skupek_passportov seznam = List.map preveri_validnost_passporta seznam

  let odgovor_4_1 = string_of_int (kolikokrat_se_pojavi_list (validnost_skupek_passportov (spremeni_input4_v_seznam (preberi_datoteko datoteka_4_in))) true)

  
  let pripisi_vrednost niz regexp = try Str.search_forward (Str.regexp regexp) niz 0 with Not_found -> -1
  
  let vsebuje_regexp niz regexp = if (pripisi_vrednost niz regexp = -1) then false else true
  
  let preveri_validnost_passporta_natancno niz = (
      vsebuje_regexp niz "byr:\\(19[2-9][0-9]\\|200[0-2]\\)"  &&
      vsebuje_regexp niz "iyr:\\(201[0-9]\\|2020\\)" && 
      vsebuje_regexp niz "eyr:\\(202[0-9]\\|2030\\)" && 
      vsebuje_regexp niz "hcl:#[0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f]" && 
      vsebuje_regexp niz "hgt:\\(\\(1[5-8][0-9]\\|19[0-3]\\)cm\\)\\|hgt:\\(7[0-6]\\|59\\|6[0-9]\\)in" && 
      vsebuje_regexp niz "ecl:\\(amb\\|blu\\|brn\\|gry\\|grn\\|hzl\\|oth\\)" &&
      vsebuje_regexp niz "pid:[0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9]\\b")

  let validnost_skupek_passportov_natancno seznam = List.map preveri_validnost_passporta_natancno seznam

  let odgovor_4_2 = string_of_int (kolikokrat_se_pojavi_list (validnost_skupek_passportov_natancno (spremeni_input4_v_seznam (preberi_datoteko datoteka_4_in))) true)


(*--------------------------------------------------- DAY 5 ------------------------------------------------------------*)
  let datoteka_5_in = "day_5.in"
  let datoteka_5_1_out = "day_5_1.out" 
  let datoteka_5_2_out = "day_5_2.out" 

  let spremeni_input5_v_seznam niz = Str.split (Str.regexp "\n") niz

  let spremeni_input5_v_seznam_seznamov  seznam = List.map str_to_list seznam

  let input5 = spremeni_input5_v_seznam_seznamov ( spremeni_input5_v_seznam ( preberi_datoteko datoteka_5_in ) )

  let test_ticket = str_to_list "FBFBBFFRLR" 

  let find_middle n1 n2 = (n1 + n2)/2


  let rec vrstica seznam min max = 
    match seznam with
    | [] -> -1
    | ['B'; a; b; c] -> max - 1
    | ['F'; a; b; c] -> min
    | 'F'::xs -> vrstica xs min (find_middle min max)
    | 'B'::xs -> vrstica xs (find_middle min max) max
    | _ -> -1 

  let rec sedez seznam min max = 
    match seznam with
    | ['R'] -> max - 1
    | ['L'] -> min
    | 'R'::xs -> sedez xs (find_middle min max) max
    | 'L'::xs -> sedez xs min (find_middle min max)
    | x::xs -> sedez xs min max
    | _ -> -1

  let ticket_ID seznam = (vrstica seznam 0 127 + 1) * 8 + (sedez seznam 0 7 + 1) (* Sedezi in vrstice se zacnejo stevilciti z 0*) 
  let ticket_ID_2 seznam = (vrstica seznam 0 128 ) * 8 + (sedez seznam 0 8 )  (* Sedezi in vrstice se zacnejo stevilciti z 1*)

  let odgovor_5_1 = maximum (List.map ticket_ID input5)
  let odgovor_5_1_str = string_of_int odgovor_5_1
  
  let ticket_ID_to_seat id = id mod 8
  let ticket_ID_to_row id = Float.to_int (Float.floor (Float.of_int ( id / 8)))
  
  let odstevanje el1 el2 = el1 - el2

  let sedezi1 = buildList (minimum (List.map ticket_ID input5)) (maximum (List.map ticket_ID input5)) 
  let sedezi2 = (List.sort compare (List.map ticket_ID input5)) 

  let rec pregled_vozovnic seznam1 seznam2 =
    match seznam1 with
    | [] -> 0
    | x::xs -> if (vsebuje_2 seznam2 x)  then pregled_vozovnic xs seznam2 else (if (x mod 8 = 0) then pregled_vozovnic xs seznam2 else x)

  let odgovor_5_2 = pregled_vozovnic sedezi1 sedezi2
  let odgovor_5_2_str = string_of_int odgovor_5_2

(*--------------------------------------------------- DAY 6 ------------------------------------------------------------*)
  let datoteka_6_in = "day_6.in"
  let datoteka_6_1_out = "day_6_1.out" 
  let datoteka_6_2_out = "day_6_2.out" 

  let sez_skup_in_odg = List.map razbitje_po_odstavkih (spremeni_input4_v_seznam (preberi_datoteko datoteka_6_in)) 

  let rec preverjalnik_anyone seznam =
    match seznam with
    | [] -> []
    | x::xs -> (str_to_list  x) @ list_diff (preverjalnik_anyone xs) (str_to_list  x)

  let count_elements seznam = List.length seznam

  let test_6 = List.map preverjalnik_anyone sez_skup_in_odg

  let test62 = List.map count_elements test_6 

  let rec vsota_seznama sez =
    match sez with
    | [] -> 0
    | x::xs -> x + vsota_seznama xs

  let odgovor_6_1 = vsota_seznama test62
  let odgovor_6_1_str = string_of_int odgovor_6_1

  let rec preverjalnik_everyone seznam =
    match seznam with
    | [] -> ['a'; 'b'; 'c'; 'd'; 'e'; 'f'; 'g'; 'h'; 'i'; 'j'; 'k'; 'l'; 'm'; 'n'; 'o'; 'p'; 'q'; 'r'; 's'; 't'; 'u'; 'v'; 'x'; 'y'; 'w'; 'z'] 
    | x::xs -> list_equal (str_to_list x) (preverjalnik_everyone xs)

  let odgovor_6_2 = vsota_seznama (List.map count_elements (List.map preverjalnik_everyone sez_skup_in_odg))
  let odgovor_6_2_str = string_of_int odgovor_6_2


(*--------------------------------------------------- DAY 7 (Unsolved)--------------------------------------------------*)
  let datoteka_7_in = "day_7.in"
  let datoteka_7_1_out = "day_7_1.out" 
  let datoteka_7_2_out = "day_7_2.out"

  let seznam_vseh_torb = razbitje_po_odstavkih(preberi_datoteko datoteka_7_in)
  let svt_contains  = List.map (Str.split (Str.regexp " contain")) seznam_vseh_torb
  
  let mother_bag torba_x seznam =  if contains_substring (List.nth seznam 1) torba_x then List.hd seznam else "0" 

  let g = Str.split (Str.regexp " contain") "clear crimson bags contain 2 dim blue bags, 4 bright indigo bags" (* ["clear crimson bags" ; "2 dim blue bags, 4 bright indigo bags"]*)
  let h = Str.split (Str.regexp " contain") "dotted lavender bags contain 5 shiny olive bags, 3 plaid blue bags, 1 shiny gold bag."

  let rec pajvkdt seznam torba = (* Preveri Ali Je V Kaksni Drugi Torbi -> vrne seznam torb v katerih lahko nosimo torbo*)
    match seznam with
    | [] -> []
    | x::xs -> if mother_bag torba x = "0" 
                  then pajvkdt xs torba 
                  else [mother_bag torba x ] @ pajvkdt xs torba  
  
  let rec naredi_seznam_vseh_torb seznam =
    match seznam with
    | [] -> []
    | x::xs -> [List.hd x] @ naredi_seznam_vseh_torb xs
  
  let svt = naredi_seznam_vseh_torb svt_contains



  let rec presek_listov seznam = 
    match seznam with
    | [] -> []
    | x::xs -> list_equal_2 x (presek_listov xs) 
  
  let presek_listov_test = presek_listov [[1;2];[2;3];[2;4]] 

  let rec unija_listov seznam = 
    match seznam with
    | [] -> []
    | x::xs -> x @ (list_diff (unija_listov xs) x )

    let unija_listov_test = unija_listov [[1;2];[2;3];[2;4]; [1;5]; [2;3]] 

  let shiny_gold_se_pojavi_v = unija_listov (List.map (pajvkdt svt_contains) (pajvkdt svt_contains "shiny gold"))
  let shiny_2 = unija_listov (List.map (pajvkdt svt_contains) shiny_gold_se_pojavi_v)
  let shiny_3 = unija_listov (List.map (pajvkdt svt_contains) shiny_2)
  let shiny_4 = unija_listov (List.map (pajvkdt svt_contains) shiny_3)
  let shiny_5 = unija_listov (List.map (pajvkdt svt_contains) shiny_4)
  let shiny_6 = unija_listov (List.map (pajvkdt svt_contains) shiny_5)
  let shiny_7 = unija_listov (List.map (pajvkdt svt_contains) shiny_6)

  let lvmtzs = shiny_gold_se_pojavi_v @ shiny_2 @ shiny_3 @ shiny_4 @ shiny_5 @ shiny_6 @ shiny_7

  let odgovor_7_1 = List.length shiny_gold_se_pojavi_v * List.length shiny_2 * List.length shiny_3 * List.length shiny_4 * List.length shiny_5 * List.length shiny_6 

  (*----------------------------------------------------------------------------------------------------------------------- *)


(*--------------------------------------------------- DAY 8 ------------------------------------------------------------*)
  let datoteka_8_in = "day_8.in"
  let datoteka_8_1_out = "day_8_1.out" 
  let datoteka_8_2_out = "day_8_2.out" 

  let sez_8 = razbitje_po_odstavkih (preberi_datoteko datoteka_8_in)
  let sez8  = List.map razbitje_po_presledkih sez_8 
  
  let rec racunaj_acc seznam = 
    let acc = 0 in
    match seznam with
    | [] ->  acc + 0 
    | x::xs -> acc + (x + racunaj_acc xs)

  let p = [] 

  let dogajanje [accumulator; mesto] seznam  =
    let acc = accumulator in
    match seznam with
    | [] -> [acc; mesto]
    | x::xs -> if List.hd (List.nth seznam mesto) = "acc" 
            then [(acc + (int_of_string (List.nth (List.nth seznam mesto) 1))) ; (mesto + 1)] 
            else  if List.hd (List.nth seznam mesto) = "jmp" 
              then [acc ; (mesto + int_of_string (List.nth (List.nth seznam mesto) 1))]
              else [acc ; mesto + 1] 

  let dodaj_mesto [accumulator; mesto] seznam_mest = seznam_mest @ [mesto]


  let rec n8 [accumulator; mesto] seznam seznam_mest = 
    if vsebuje_2 seznam_mest mesto 
      then accumulator
    else n8 (dogajanje [accumulator; mesto] seznam) seznam (dodaj_mesto [accumulator; mesto] seznam_mest) 
    
  let odgovor_8_1 = n8 [0; 0] sez8 p
  let odgovor_8_1_str = string_of_int odgovor_8_1
  
  let rec najdi_napako [accumulator; mesto] seznam seznam_mest = 
    if vsebuje_2 seznam_mest mesto 
      then List.nth seznam_mest (List.length (seznam_mest) - 1)
    else najdi_napako (dogajanje [accumulator; mesto] seznam) seznam (dodaj_mesto [accumulator; mesto] seznam_mest)

  let t8 = najdi_napako [0; 0] sez8 p
  let zamenjaj_232 = replace sez8 233 ["nop"; "+1"]
  let t8_1 = najdi_napako [0; 0] zamenjaj_232 p


  let odgovor_8_2 = n8 [0; 0] zamenjaj_232 p
(*--------------------------------------------------- DAY 9 ------------------------------------------------------------*)

    

(*--------------------------------------------------- Generator ------------------------------------------------------------*)
  
  let _ = 
      izpisi_datoteko datoteka_1_1_out odgovor_1_1;
      izpisi_datoteko datoteka_1_2_out odgovor_1_2;
      izpisi_datoteko datoteka_2_1_out odgovor_2_1;
      izpisi_datoteko datoteka_2_2_out odgovor_2_2;
      izpisi_datoteko datoteka_3_1_out odgovor_3_1;
      izpisi_datoteko datoteka_3_2_out odgovor_3_2;
      izpisi_datoteko datoteka_4_1_out odgovor_4_1;
      izpisi_datoteko datoteka_4_2_out odgovor_4_2;
      izpisi_datoteko datoteka_5_1_out odgovor_5_1_str;
      izpisi_datoteko datoteka_5_2_out odgovor_5_2_str;
      izpisi_datoteko datoteka_6_1_out odgovor_6_1_str;
      izpisi_datoteko datoteka_6_2_out odgovor_6_2_str;
      izpisi_datoteko datoteka_8_1_out odgovor_8_1_str;