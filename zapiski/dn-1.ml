(* # 1. domača naloga *)

(* ## Ogrevanje *)

(** Števke *)

let rec stevke b n = 
  if n < b then [n]
  else (stevke b (n / b)) @ [n mod b]

(** Začetek seznama *)

let rec take n list =
   match n, list with
   | 0, _ -> []
   | _, [] -> []
   | _, x :: xs -> x :: take (n - 1) xs

(** Odstranjevanje ujemajočih *)

let rec drop_while p list =
   match list with
   | [] -> []
   | x :: xs -> if p x then drop_while p xs else list

(** Funkcija `filter_mapi` *)

let filter_mapi _ _ = failwith __LOC__

(* ## Izomorfizmi množic *)

type ('a, 'b) sum = In1 of 'a | In2 of 'b

(** $A \times B \cong B \times A$ *)

let phi1 (a, b) = (b, a)
let psi1 (b, a) = (a, b)

(** $A + B \cong B + A$ *)

let phi2 x =
   match x with
   | In1 a -> In2 a
   | In2 b -> In1 b
let psi2 x =
   match x with
   | In1 b -> In2 b
   | In2 a -> In1 a

(** $A \times (B \times C) \cong (A \times B) \times C$ *)

let phi3 (a, (b, c)) = ((a, b), c)
let psi3 ((a, b), c) = (a, (b, c))

(** $A + (B + C) \cong (A + B) + C$ *)

let phi4 x = 
   match x with
   | In1 a -> In1 (In1 a)
   | In2 (In1 b) -> In1 (In2 b)
   | In2 (In2 c) -> In2 c
let psi4 x = 
   match x with
   | In1 (In1 a) -> In1 a
   | In1 (In2 b) -> In2 (In1 b)
   | In2 c -> In2 (In2 c)

(** $A \times (B + C) \cong (A \times B) + (A \times C)$ *)

let phi5 (a, x) =
   match x with
   | In1 b -> In1 (a, b)
   | In2 c -> In2 (a, c)
let psi5 x =
   match x with
   | In1 (a, b) -> (a, In1 b)
   | In2 (a, c) -> (a, In2 c)

(** $A^{B + C} \cong A^B \times A^C$ *)

let phi6 f =
   let fb = (fun b -> f (In1 b)) in
   let fc = (fun c -> f (In2 c)) in
   (fb, fc)
let psi6 (fb, fc) =
   fun x ->
      match x with
      | In1 b -> fb b
      | In2 c -> fc c

(** $(A \times B)^C \cong A^C \times B^C$ *)

let phi7 _ = failwith __LOC__
let psi7 _ = failwith __LOC__

(* ## Polinomi *)

type polinom = int list

(** Odstranjevanje odvečnih ničel *)
let skrajsaj seznam = 
   match List.rev seznam with
   | [] -> []
   | _ :: tl -> List.rev tl

let zadnji lst = 
   match List.rev lst with
   | [] -> None
   | x :: _ -> Some x

let rec pocisti (seznam : polinom) : polinom =
   match zadnji seznam with
   | Some 0 -> pocisti (skrajsaj seznam)
   | _ -> if seznam = [] then [0] else seznam

(** Seštevanje *)

let rec sestej (sez1 : polinom) (sez2 : polinom) : polinom =
   match (sez1, sez2) with
   | ([], []) -> []
   | ([], _) -> sez2
   | (_, []) -> sez1
   | (x :: xs, y :: ys) ->
       let vsota = x + y in
       vsota :: sestej xs ys

let ( +++ ) sez1 sez2 =
   let sesteto = sestej sez1 sez2 in
   pocisti sesteto

(** Množenje *)

let rec dodaj_nicle polinom n =
   if n <= 0 then polinom else 0 :: dodaj_nicle polinom (n - 1)

let pomnozi_koeficient koeficient polinom2 stopnja =
   dodaj_nicle (List.map (fun koeficient2 -> koeficient * koeficient2) polinom2) stopnja

let ( *** ) (pol1 : polinom) (pol2 : polinom) : polinom =
   let rec pomnozi pol1 stopnja =
      match pol1 with
      | [] -> []
      | koeficient :: ostalo ->
         let delni_rezultat = pomnozi_koeficient koeficient pol2 stopnja in
         delni_rezultat +++ pomnozi ostalo (stopnja + 1)
      in  pomnozi pol1 0

(** Izračun vrednosti v točki *)

let rec potenciranje_int osnova stopnja =
   if stopnja = 0 then 1 else osnova * potenciranje_int osnova (stopnja - 1)

let vrednost (polinom : polinom) n =
   let rec izracunaj polinom stopnja =
      match polinom with
      | [] -> 0
      | koeficient :: ostalo -> 
         (koeficient * (potenciranje_int n stopnja) + izracunaj ostalo (stopnja + 1))
   in izracunaj polinom 0


(** Odvajanje *)

let odvod (polinom : polinom) : polinom =
   let rec izracunaj_odvod polinom stopnja =
      match polinom with
      | [] -> []
      | koeficient :: ostalo ->
         if stopnja = 0 then izracunaj_odvod ostalo (stopnja + 1)
         else (stopnja * koeficient) :: izracunaj_odvod ostalo (stopnja + 1)
      in izracunaj_odvod polinom 0

(** Lep izpis *)

let izpis _ = failwith __LOC__
(* let primer_3_8 = izpis [ 1; 2; 1 ] *)
(* let primer_3_9 = izpis [ 1; 0; -1; 0; 1; 0; -1; 0; 1; 0; -1; 0; 1 ] *)
(* let primer_3_10 = izpis [ 0; -3; 3; -1 ] *)

(* ## Samodejno odvajanje *)

let priblizek_odvoda f x0 h = (f (x0 +. h) -. f x0) /. h

(* let primer_3_11 =
   let f x = sin x +. cos x +. exp x in
   List.map (priblizek_odvoda f 1.) [ 0.1; 0.01; 0.001; 0.0001; 0.00001 ] *)

type odvedljiva = (float -> float) * (float -> float)

let sinus : odvedljiva = (sin, cos)
let kosinus : odvedljiva = (cos, fun x -> -.sin x)
let eksp : odvedljiva = (exp, exp)

let ( ++. ) : odvedljiva -> odvedljiva -> odvedljiva =
 (* pozorni bodite, da anonimni funkciji v paru date med oklepaje *)
 fun (f, f') (g, g') -> ((fun x -> f x +. g x), fun x -> f' x +. g' x)

(** Vrednost odvoda *)

let vrednost2 _ _ = failwith __LOC__
let odvod _ _ = failwith __LOC__

(** Osnovne funkcije *)

let konstanta _ = failwith __LOC__
let identiteta = ((fun _ -> failwith __LOC__), fun _ -> failwith __LOC__)

(** Produkt in kvocient *)

let ( **. ) _ _ = failwith __LOC__
let ( //. ) _ _ = failwith __LOC__
(* let kvadrat = identiteta **. identiteta *)

(** Kompozitum *)

let ( @@. ) _ _ = failwith __LOC__
(* let vedno_ena = (kvadrat @@. sinus) ++. (kvadrat @@. kosinus) *)
(* let primer_4_1 = vrednost vedno_ena 12345. *)
(* let primer_4_2 = odvod vedno_ena 12345. *)

(* ## Substitucijska šifra *)

let quick_brown_fox = "THEQUICKBRWNFXJMPSOVLAZYDG"
let rot13 = "NOPQRSTUVWXYZABCDEFGHIJKLM"
let indeks c = Char.code c - Char.code 'A'
let crka i = Char.chr (i + Char.code 'A')

(** Šifriranje *)

let sifriraj _ _ = failwith __LOC__
(* let primer_5_1 = sifriraj quick_brown_fox "HELLO, WORLD!" *)
(* let primer_5_2 = "VENI, VIDI, VICI" |> sifriraj rot13 *)
(* let primer_5_3 = "VENI, VIDI, VICI" |> sifriraj rot13 |> sifriraj rot13 *)

(** Inverzni ključ *)

let inverz _ = failwith __LOC__
(* let primer_5_4 = inverz quick_brown_fox *)
(* let primer_5_5 = inverz rot13 = rot13 *)
(* let primer_5_6 = inverz "BCDEA" *)

(** Ugibanje ključa *)

let besede =
  "the of to and a in is it you that he was for on are with as i his they be \
   at one have this from or had by word but what some we can out other were \
   all there when up use your how said an each she which do their time if will \
   way about many then them write would like so these her long make thing see \
   him two has look more day could go come did number sound no most people my \
   over know water than call first who may down side been now find any new \
   work part take get place made live where after back little only round man \
   year came show every good me give our under name very through just form \
   sentence great think say help low line differ turn cause much mean before \
   move right boy old too same tell does set three want air well also play \
   small end put home read hand port large spell add even land here must big \
   high such follow act why ask men change went light kind off need house \
   picture try us again animal point mother world near build self earth father \
   head stand own page should country found answer school grow study still \
   learn plant cover food sun four between state keep eye never last let \
   thought city tree cross farm hard start might story saw far sea draw left \
   late run don't while press close night real life few north open seem \
   together next white children begin got walk example ease paper group always \
   music those both mark often letter until mile river car feet care second \
   book carry took science eat room friend began idea fish mountain stop once \
   base hear horse cut sure watch color face wood main enough plain girl usual \
   young ready above ever red list though feel talk bird soon body dog family \
   direct pose leave song measure door product black short numeral class wind \
   question happen complete ship area half rock order fire south problem piece \
   told knew pass since top whole king space heard best hour better true . \
   during hundred five remember step early hold west ground interest reach \
   fast verb sing listen six table travel less morning ten simple several \
   vowel toward war lay against pattern slow center love person money serve \
   appear road map rain rule govern pull cold notice voice unit power town \
   fine certain fly fall lead cry dark machine note wait plan figure star box \
   noun field rest correct able pound done beauty drive stoDo contain front \
   teach week final gave green oh quick develop ocean warm free minute strong \
   special mind behind clear tail produce fact street inch multiply nothing \
   course stay wheel full force blue object decide surface deep moon island \
   foot system busy test record boat common gold possible plane stead dry \
   wonder laugh thousand ago ran check game shape equate hot miss brought heat \
   snow tire bring yes distant fill east paint language among grand ball yet \
   wave drop heart am present heavy dance engine position arm wide sail \
   material size vary settle speak weight general ice matter circle pair \
   include divide syllable felt perhaps pick sudden count square reason length \
   represent art subject region energy hunt probable bed brother egg ride cell \
   believe fraction forest sit race window store summer train sleep prove lone \
   leg exercise wall catch mount wish sky board joy winter sat written wild \
   instrument kept glass grass cow job edge sign visit past soft fun bright \
   gas weather month million bear finish happy hope flower clothe strange gone \
   jump baby eight village meet root buy raise solve metal whether push seven \
   paragraph third shall held hair describe cook floor either result burn hill \
   safe cat century consider type law bit coast copy phrase silent tall sand \
   soil roll temperature finger industry value fight lie beat excite natural \
   view sense ear else quite broke case middle kill son lake moment scale loud \
   spring observe child straight consonant nation dictionary milk speed method \
   organ pay age section dress cloud surprise quiet stone tiny climb cool \
   design poor lot experiment bottom key iron single stick flat twenty skin \
   smile crease hole trade melody trip office receive row mouth exact symbol \
   die least trouble shout except wrote seed tone join suggest clean break \
   lady yard rise bad blow oil blood touch grew cent mix team wire cost lost \
   brown wear garden equal sent choose fell fit flow fair bank collect save \
   control decimal gentle woman captain practice separate difficult doctor \
   please protect noon whose locate ring character insect caught period \
   indicate radio spoke atom human history effect electric expect crop modern \
   element hit student corner party supply bone rail imagine provide agree \
   thus capital won't chair danger fruit rich thick soldier process operate \
   guess necessary sharp wing create neighbor wash bat rather crowd corn \
   compare poem string bell depend meat rub tube famous dollar stream fear \
   sight thin triangle planet hurry chief colony clock mine tie enter major \
   fresh search send yellow gun allow print dead spot desert suit current lift \
   rose continue block chart hat sell success company subtract event \
   particular deal swim term opposite wife shoe shoulder spread arrange camp \
   invent cotton born determine quart nine truck noise level chance gather \
   shop stretch throw shine property column molecule select wrong gray repeat \
   require broad prepare salt nose plural anger claim continent oxygen sugar \
   death pretty skill women season solution magnet silver thank branch match \
   suffix especially fig afraid huge sister steel discuss forward similar \
   guide experience score apple bought led pitch coat mass card band rope slip \
   win dream evening condition feed tool total basic smell valley nor double \
   seat arrive master track parent shore division sheet substance favor \
   connect post spend chord fat glad original share station dad bread charge \
   proper bar offer segment slave duck instant market degree populate chick \
   dear enemy reply drink occur support speech nature range steam motion path \
   liquid log meant quotient teeth shell neck"

let slovar = [ (* TODO *) ]
(* let primer_5_7 = take 42 slovar *)
(* let primer_5_8 = List.nth slovar 321 *)

(* Med ugibanjem seveda ne bomo poznali celotnega ključa. V tem primeru bomo za neznane črke uporabili znak `_`. Na primer, če bi vedeli, da je črka `A` v besedilu šifrirana kot `X`, črka `C` pa kot `Y`, bi ključ zapisali kot `"X_Y_______________________"`. *)
(*  *)
(* Napišite funkcijo `dodaj_zamenjavo : string -> char * char -> string option`, ki sprejme ključ ter ga poskusi razširiti z zamenjavo dane črke. Funkcija naj vrne `None`, če razširitev vodi v ključ, ki ni bijektiven (torej če ima črka že dodeljeno drugo zamenjavo ali če smo isto zamenjavo dodelili dvema različnima črkama). *)

(** Razširjanje ključa s črko *)
let dodaj_zamenjavo _ _ = failwith __LOC__

(* let primer_5_9 = dodaj_zamenjavo "AB__E" ('C', 'X') *)
(* let primer_5_10 = dodaj_zamenjavo "ABX_E" ('C', 'X') *)
(* let primer_5_11 = dodaj_zamenjavo "ABY_E" ('C', 'E') *)

(** Razširjanje ključa z besedo *)

(* S pomočjo funkcije `dodaj_zamenjavo` sestavite še funkcijo `dodaj_zamenjave : string -> string * string -> string option`, ki ključ razširi z zamenjavami, ki prvo besedo preslikajo v drugo. *)

let dodaj_zamenjave _ _ = failwith __LOC__
(* let primer_5_12 = dodaj_zamenjave "__________________________" ("HELLO", "KUNNJ") *)
(* let primer_5_13 = dodaj_zamenjave "ABCDU_____________________" ("HELLO", "KUNNJ") *)
(* let primer_5_14 = dodaj_zamenjave "ABCDE_____________________" ("HELLO", "KUNNJ") *)

(** Vse možne razširitve *)

(* Sestavite funkcijo `mozne_razsiritve : string -> string -> string list -> string list`, ki vzame ključ, šifrirano besedo ter slovar vseh možnih besed, vrne pa seznam vseh možnih razširitev ključa, ki šifrirano besedo slikajo v eno od besed v slovarju. *)

let mozne_razsiritve _ _ _ = failwith __LOC__

(* let primer_5_15 =
   slovar
   |> mozne_razsiritve (String.make 26 '_') "KUNNJ"
   |> List.map (fun kljuc -> (kljuc, sifriraj kljuc "KUNNJ")) *)

(** Odšifriranje *)

(* Napišite funkcijo `odsifriraj : string -> string option`, ki sprejme šifrirano besedilo in s pomočjo slovarja besed ugane odšifrirano besedilo. Funkcija naj vrne `None`, če ni mogoče najti nobenega ustreznega ključa. *)
let odsifriraj _ = failwith __LOC__
(* let primer_5_16 = sifriraj quick_brown_fox "THIS IS A VERY HARD PROBLEM" *)
(* let primer_5_17 = odsifriraj "VKBO BO T AUSD KTSQ MSJHNUF" *)