let preberi_datoteko ime_datoteke =
  let chan = open_in ime_datoteke in
  let vsebina = really_input_string chan (in_channel_length chan) in
  close_in chan;
  vsebina

let  izpisi_datoteko ime_datoteke vsebina =
        let chan = open_out ime_datoteke in
        output_string chan vsebina;
        close_out chan

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







let odgovor_2_1 = "Še Ni rešeno"
let odgovor_2_2 = "Še Ni rešeno"


(*--------------------------------------------------- Generator ------------------------------------------------------------*)
  
let _ = 
    izpisi_datoteko datoteka_1_1_out odgovor_1_1;
    izpisi_datoteko datoteka_1_2_out odgovor_1_2;
    izpisi_datoteko datoteka_2_1_out odgovor_2_1;
    izpisi_datoteko datoteka_2_2_out odgovor_2_2;