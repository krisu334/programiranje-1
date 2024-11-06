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

let filter_mapi f list =
   let rec pomozna i sez =
      match sez with
      | [] -> []
      | x :: xs ->
         (match f i x with
         | Some y -> y :: pomozna (i + 1) xs
         | None -> pomozna (i + 1) xs)
   in pomozna 0 list

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

let phi7 (f : 'c -> 'a * 'b) : ('c -> 'a) * ('c -> 'b) =
   let fa = (fun c -> fst (f c)) in
   let fb = (fun c -> snd (f c)) in
   (fa, fb)
let psi7 ((fa, fb) : ('c -> 'a) * ('c -> 'b)) : 'c -> ('a * 'b) =
   fun c -> (fa c, fb c)

(* ## Polinomi *)

type polinom = int list

(** Odstranjevanje odvečnih ničel *)
let skrajsaj seznam = 
   match List.rev seznam with
   | [] -> []
   | _ :: xs -> List.rev xs

let zadnji seznam = 
   match List.rev seznam with
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
   pocisti (sestej sez1 sez2)

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
      in pomnozi pol1 0

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

let mnozi_indeks (polinom : polinom) : polinom =
   let rec pomozna sez indeks =
      match sez with
      | [] -> []
      | x :: xs -> (x * indeks) :: pomozna xs (indeks + 1)
   in pomozna polinom 0

let odvod (polinom : polinom) : polinom =
   List.tl (mnozi_indeks polinom)

(** Lep izpis *)

let rec nadnapis n =
   let eksponent = ["⁰"; "¹"; "²"; "³"; "⁴"; "⁵"; "⁶"; "⁷"; "⁸"; "⁹"] in
   if n < 10 then List.nth eksponent n
   else nadnapis (n / 10) ^ List.nth eksponent (n mod 10)

let izpis (polinom : polinom) : string =
   let obrnjeno = List.rev polinom in
   let rec aux polinom stopnja =
     match polinom with
     | [] -> ""
     | 0 :: xs -> aux xs (stopnja - 1)
     | koef :: xs ->
         let clen =
            if stopnja = 0 then " + " ^ (string_of_int koef)
            else if koef = 1 && stopnja = 1 then "+ x"
            else if koef = -1 && stopnja = 1 then "- x"
            else if koef = 1 then " + x" ^ nadnapis stopnja
            else if koef = -1 then "- x" ^ nadnapis stopnja
            else if stopnja = 1 then (string_of_int koef) ^ " x"
            else (string_of_int koef) ^ " x" ^ nadnapis stopnja
         in let ostalo = aux xs (stopnja - 1) in clen ^ " " ^ ostalo
   in String.trim (aux obrnjeno (List.length polinom - 1))

(* ## Samodejno odvajanje *)

let priblizek_odvoda f x0 h = (f (x0 +. h) -. f x0) /. h

type odvedljiva = (float -> float) * (float -> float)

let sinus : odvedljiva = (sin, cos)
let kosinus : odvedljiva = (cos, fun x -> -.sin x)
let eksp : odvedljiva = (exp, exp)

let ( ++. ) : odvedljiva -> odvedljiva -> odvedljiva =
 (* pozorni bodite, da anonimni funkciji v paru date med oklepaje *)
 fun (f, f') (g, g') -> ((fun x -> f x +. g x), fun x -> f' x +. g' x)

(** Vrednost odvoda *)

let vrednost2 ((f, _) : odvedljiva) x = f x
let odvod2 ((_, g) : odvedljiva) x = g x

(** Osnovne funkcije *)

let konstanta c : odvedljiva = ((fun _ -> c), (fun _ -> 0.))
let identiteta : odvedljiva = ((fun x -> x), fun _ -> 1.)

(** Produkt in kvocient *)

let ( **. ) ((f, f') : odvedljiva) ((g, g') : odvedljiva) : odvedljiva =
   ((fun x -> f x *. g x), (fun x -> f' x *. g x +. f x *. g' x))
let ( //. ) ((f, f') : odvedljiva) ((g, g') : odvedljiva) : odvedljiva =
   ((fun x -> f x /. g x), (fun x -> (f' x *. g x -. f x *. g' x) /. (g x *. g x)))

(** Kompozitum *)

let ( @@. ) ((f, f') : odvedljiva) ((g, g') : odvedljiva) : odvedljiva =
   ((fun x -> f (g x)), (fun x -> f' (g x) *. g' x))

(* ## Substitucijska šifra *)

let quick_brown_fox = "THEQUICKBRWNFXJMPSOVLAZYDG"
let rot13 = "NOPQRSTUVWXYZABCDEFGHIJKLM"
let indeks c = Char.code c - Char.code 'A'
let crka i = Char.chr (i + Char.code 'A')

(** Šifriranje *)

let sifriraj pravilo stavek =
   String.map (fun c ->
      if 'A' <= c && c <= 'Z' then 
      let index = indeks c in pravilo.[index]
      else c) stavek

(** Inverzni ključ *)

let inverz pravilo =
   let dolzina = String.length pravilo in
   let rec inv i acc =
      if i >= dolzina then acc else
      let c = pravilo.[i] in
      let poz = indeks c in
      let nov_acc = List.mapi (fun j ch -> if j = poz then crka i else ch) acc in
      inv (i + 1) nov_acc
   in let acc_zacetni = List.init dolzina (fun _ -> 'A') in
   String.concat "" (List.map (String.make 1) (inv 0 acc_zacetni))

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

let slovar = 
   besede
   |> String.split_on_char ' '
   |> List.map String.uppercase_ascii

(** Razširjanje ključa s črko *)

let dodaj_zamenjavo (izraz : string) ((crka1, crka2) : char * char) : string option =
   let index = indeks crka1 in
   if izraz.[index] <> '_' && izraz.[index] <> crka2 then None
   else if String.contains izraz crka2 && izraz.[index] <> crka2 then None
   else let nov_string = 
      String.init (String.length izraz) (fun i ->
         if i = index then crka2 else izraz.[i]) in Some nov_string

(** Razširjanje ključa z besedo *)

let dodaj_zamenjave izraz (beseda1, beseda2) =
   if String.length beseda1 <> String.length beseda2 then None
   else

(** Vse možne razširitve *)

let mozne_razsiritve _ _ _ = failwith __LOC__

(** Odšifriranje *)

let odsifriraj _ = failwith __LOC__