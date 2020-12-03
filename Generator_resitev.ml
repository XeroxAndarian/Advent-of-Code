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
  expl (String.length s - 1) [];;




(*--------------------------------------------------- DAY 1 ------------------------------------------------------------*)

let datoteka_1_in = "day_1.in"
let datoteka_1_1_out = "day_1_1.out" 
let datoteka_1_2_out = "day_1_2.out" 

let test_list = ["1721"; "979"; "366"; "299"; "675"; "1456"]

let list = String.split_on_char '\n' (preberi_datoteko datoteka_1_in)

let vsebuje sez a = List.mem (string_of_int a) sez

let rec zmnozek sez = 
  match sez with
  | [] -> 1
  | x::xs -> x * zmnozek xs

let rec zdskvj vsota seznam = (* zdskvj = zmnozek dveh stevil, katerih vsota je *)
  match seznam with
  | [] -> "Seznam ne vsebuje dveh stevil, katerih vsota je zahtevana vrednost"
  | x::rest -> 
    let nadomestek = vsota - int_of_string x in
    if vsebuje rest nadomestek then string_of_int (zmnozek [nadomestek; int_of_string x]) else zdskvj vsota rest 


          
  
    

let odgovor_1_1 = zdskvj 2020 list
let odgovor_1_2 = "Še Ni rešeno"




(*--------------------------------------------------- DAY 2 ------------------------------------------------------------*)
let datoteka_2_in = "day_2.in"
let datoteka_2_1_out = "day_2_1.out" 
let datoteka_2_2_out = "day_2_2.out" 

type password = {pojavitev_min: int; pojavitev_max: int; znak: char; geslo: string}

let razbitje_po_odstavkih str = String.split_on_char '\n' str 
let razbitje_po_presledkih str = String.split_on_char ' ' str 
let razbitje_po_pomisljaju str = String.split_on_char '-' str

let razbitje str znak = String.split_on_char znak str 

let razvrstitev seznam =
  match seznam with
  | [x; y; z] -> {
    pojavitev_min= int_of_string (List.hd (razbitje_po_pomisljaju x)); 
    pojavitev_max= int_of_string (List.nth (razbitje_po_pomisljaju x) 1 ); 
    znak=y.[0];
    geslo=z}
  | _::_ -> {pojavitev_min=0; pojavitev_max=0; znak='0'; geslo="Prazno"}
  | [] -> {pojavitev_min=0; pojavitev_max=0; znak='0'; geslo="Prazno"}


let kolikokrat_se_pojavi str ch = 
  match str_to_list str with
  | [] -> 0
  | x::xs -> List.length (List.find_all (fun y -> y = ch) (str_to_list str))

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

let odgovor_2_1 = string_of_int (koliko_gesel_je_primernih (seznam_pass))

let pojavi_se_na_mestih pass = 
  match str_to_list pass.geslo with
  | [] -> false
  | _ -> (List.nth (str_to_list pass.geslo) (pass.pojavitev_min - 1) = pass.znak) <> (List.nth (str_to_list pass.geslo) (pass.pojavitev_max - 1) = pass.znak) 



let odgovor_2_2 = string_of_int (kolikokrat_se_pojavi_list (List.map pojavi_se_na_mestih seznam_pass) true )


(*--------------------------------------------------- Generator ------------------------------------------------------------*)
  
let _ = 
    izpisi_datoteko datoteka_1_1_out odgovor_1_1;
    izpisi_datoteko datoteka_1_2_out odgovor_1_2;
    izpisi_datoteko datoteka_2_1_out odgovor_2_1;
    izpisi_datoteko datoteka_2_2_out odgovor_2_2;