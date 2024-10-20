(* ========== Vaja 2: Uvod v funkcijsko programiranje  ========== *)

(*----------------------------------------------------------------------------*]
Vektorje predstavimo kot seznam števil s plavajočo vejico.
[*----------------------------------------------------------------------------*)

type vector = float list

(*----------------------------------------------------------------------------*]
Definirajte enotske vektorje `i`, `j` in `k` v treh dimenzijah.
[*----------------------------------------------------------------------------*)

let i = [1.; 0.; 0.]

(*----------------------------------------------------------------------------*]
Napišite funkcijo `razteg : float -> vector -> vector`, ki vektor, 
predstavljen s seznamom števil s plavajočo vejico, pomnoži z danim skalarjem.
[*----------------------------------------------------------------------------*)

let razteg k v = List.map (( *. ) k) v (*pika zraven zvezdice ker so floati; pisali smo kot funkcijo na k-ju*)
(*tudi brez v lahko, ker idrk*)

(*----------------------------------------------------------------------------*]
Napišite funkcijo `sestej : vector -> vector -> vector`, ki vrne vsoto dveh 
vektorjev.
[*----------------------------------------------------------------------------*)

let sestej v1 v2 = List.map2 (+.) v1 v2
(*map2 uporabljamo za razcep dveh vektorjev oz listov
lahko tudi brez v1 in v2, ker je že definirano*)

(*----------------------------------------------------------------------------*]
Napišite funkcijo `skalarni_produkt : vector -> vector -> float`, ki izračuna 
skalarni produkt dveh vektorjev
[*----------------------------------------------------------------------------*)

let vsota_seznama = List.fold_left ( +. ) 0.

let skalarni_produkt v1 v2 = 
  vsota_seznama @@ List.map2 ( *. ) v1 v2

(*----------------------------------------------------------------------------*]
Napišite funkcijo `norma : vector -> float`, ki vrne evklidsko normo vektorja.
[*----------------------------------------------------------------------------*)

let norma v = sqrt (skalarni_produkt v v)

(*----------------------------------------------------------------------------*]
Napišite funkcijo `projeciraj : vector -> vector -> vector`, ki izračuna 
projekcijo prvega vektorja na drugega.
[*----------------------------------------------------------------------------*)

let projeciraj v1 v2 = razteg ((1. /. (norma v2 ** 2.)) *. skalarni_produkt v1 v2) v2

(*----------------------------------------------------------------------------*]
Napišite funkcijo `ovij : string -> string -> string`, ki sprejme ime HTML 
oznake in vsebino ter vrne niz, ki predstavlja ustrezno HTML oznako.

Primer:
`ovij "h1" "Hello, world!"`

[*----------------------------------------------------------------------------*)

let ovij oznaka vsebina = 
  "<" ^ oznaka ^ ">" ^ vsebina ^ "</" ^ oznaka ^ ">"

(*----------------------------------------------------------------------------*]
Napišite funkcijo `zamakni : int -> string -> string`, ki sprejme število 
presledkov in niz ter vrne niz, v katerem je vsaka vrstica zamaknjena za ustrezno število presledkov.

Primer:
`zamakni 4 "Hello, world!"`

[*----------------------------------------------------------------------------*)

let zamakni n vsebina =
  let presledki = String.make n ' ' in (* naredi string ki vkljucuje presledke*)
  vsebina
  |> String.split_on_char '\n' (*naredi list iz stringov, kjer smo ločili glede na \n*)
  |> List.map (String.cat presledki) (*list.map pomeni da vsakemu elementu niza naredimo nekaj: doda presledke - string.cat*)
  |> String.concat "\n" (*zdruzi list v nov string z \n vmes*)

(*----------------------------------------------------------------------------*]
Napišite funkcijo `ul : string list -> string`, ki sprejme seznam nizov in vrne 
niz, ki predstavlja ustrezno zamaknjen neurejeni seznam v HTML-ju:

Primer:
`ul ["ananas"; "banana"; "čokolada"]`

[*----------------------------------------------------------------------------*)

let ul seznam = 
  seznam
  |> List.map (ovij "li")
  |> String.concat "\n"
  |> zamakni 2
  |> (fun content -> "\n" ^ content ^ "\n")
  |> ovij "ul"

(*----------------------------------------------------------------------------*]
Napišite funkcijo `razdeli_vrstico : string -> string * string`, ki sprejme niz, 
ki vsebuje vejico, loči na del pred in del za njo.

Primer:
`razdeli_vrstico "mleko, 2"`

[*----------------------------------------------------------------------------*)

let rec razdeli_vrstico = ()

(*----------------------------------------------------------------------------*]
Napišite funkcijo `pretvori_v_seznam_parov : string -> (string * string) list`, 
ki sprejme večvrstični niz, kjer je vsaka vrstica niz oblike 
"izdelek, vrednost", in vrne seznam ustreznih parov.

Primer:
`pretvori_v_seznam_parov "mleko, 2\nkruh, 1\njabolko, 5"`

[*----------------------------------------------------------------------------*)

let rec pretvori_v_seznam_parov = ()

(*----------------------------------------------------------------------------*]
Napišite funkcijo `pretvori_druge_komponente : ('a -> 'b) -> (string * 'a) list -> (string * 'b) list`,
ki dano funkcijo uporabi na vseh drugih komponentah elementov seznama.

Primer:
```ml
let seznam = [("ata", "mama"); ("teta", "stric")] in 
pretvori_druge_komponente String.length seznam
```

[*----------------------------------------------------------------------------*)

let rec pretvori_druge_komponente = ()

(*----------------------------------------------------------------------------*]
Napišite funkcijo `izracunaj_skupni_znesek : string -> string -> float`, ki 
sprejme večvrstična niza nakupovalnega seznama in cenika in izračuna skupni 
znesek nakupa.

Primer:
```ml
let nakupovalni_seznam = "mleko, 2\njabolka, 5"
and cenik = "jabolka, 0.5\nkruh, 2\nmleko, 1.5" in
izracunaj_skupni_znesek cenik nakupovalni_seznam
```

[*----------------------------------------------------------------------------*)

let rec izracunaj_skupni_znesek = ()
