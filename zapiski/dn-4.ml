(*----------------------------------------------------------------------------*
 # 4. domača naloga
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Pri tej nalogi boste napisali svoj simulator Turingovih strojev. Zaradi
 preprostosti bomo za abecedo vzeli kar znake tipa `char`, za prazni znak bomo
 izbrali presledek `' '`, stanja pa bomo predstavili z nizi. Za možne premike
 zafiksiramo tip `direction`:
[*----------------------------------------------------------------------------*)

type direction = Left | Right
type state = string

(*----------------------------------------------------------------------------*
 ## Implementacija trakov
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Napišite modul `Tape`, ki implementira spodnjo signaturo, kjer je:

 - `t` tip v obe smeri neomejenih trakov in glavo na danem mestu;
 - `make`, ki naredi nov trak z znaki iz niza ter glavo na prvem znaku;
 - `read`, ki vrne znak pod glavo;
 - `write`, ki pod glavo zapiše dani znak;
 - `move`, ki glavo premakne v dano smer;
 - `print`, ki izpiše vsebino traku (brez presledkov na začetku in koncu) ter
 pod njim z `^` označi mesto glave.

 Zadnji dve funkciji naj vrneta nov trak, obstoječega pa naj pustita
 nespremenjenega.

 Ker je tip `t` abstrakten, si lahko privoščite poljubno implementacijo, zato
 poskrbite tako za učinkovitost kot za preglednost kode.
[*----------------------------------------------------------------------------*)

module type TAPE = sig
  type t

  val make : string -> t
  val move : direction -> t -> t
  val read : t -> char
  val write : char -> t -> t
  val print : t -> unit
end

module Tape : TAPE = struct
  type t = {left: char list; head: char; right: char list}

  let make niz =
    let rec string_to_chars str indeks zbirno_mesto =
      if indeks < 0 then zbirno_mesto
      else string_to_chars str (indeks - 1) (str.[indeks] :: zbirno_mesto) in
    let char_list = string_to_chars niz (String.length niz - 1) [] in
    match char_list with
    | [] -> {left = []; head = ' '; right = []}
    | h :: t -> {left = []; head = h; right = t}
    
  let move dir tape =
    match dir with
    | Left -> (
        match tape.left with
        | [] -> {left = []; head = ' '; right = tape.head :: tape.right}
        | h :: t -> {left = t; head = h; right = tape.head :: tape.right})
    | Right -> (
        match tape.right with
        | [] -> { left = tape.head :: tape.left; head = ' '; right = [] }
        | h :: t -> { left = tape.head :: tape.left; head = h; right = t })

  let read tape =
    tape.head
    
  let write nov tape =
    {tape with head = nov}

  let print tape =
    let rec list_to_string list =
      match list with
      | [] -> ""
      | c :: ostali -> String.make 1 c ^ list_to_string ostali in
    let str_levo = list_to_string (List.rev tape.left) in
    let str_desno = list_to_string tape.right in
    let str_cel = str_levo ^ String.make 1 tape.head ^ str_desno in
    let head_mesto = String.length str_levo in
    Printf.printf "%s\n" str_cel;
    Printf.printf "%s^\n" (String.make head_mesto ' ')
end

(*----------------------------------------------------------------------------*
 ## Implementacija Turingovih strojev
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Napišite modul `Machine`, ki implementira spodnjo signaturo, kjer je:

 - `t` tip Turingovih strojev;
 - `make`, ki naredi nov stroj z danim začetnim stanjem in seznamom preostalih
 stanj ter prazno prehodno funkcijo;
 - `initial`, ki vrne začetno stanje stroja;
 - `add_transition`, ki prehodno funkcijo razširi s prehodom $(q, a) \mapsto
 (q', a', d)$;
 - `step`, ki za dano stanje in trak izvede en korak stroja, če je to mogoče.

 Zadnji dve funkciji naj vrneta spremenjene vrednosti, obstoječe argumente pa
 naj pustita nespremenjene. Prav tako pri zadnjih dveh funkcijah lahko
 predpostavite, da ju bomo klicali le na poprej podanih stanjih.

 Tudi tu je tip `t` abstrakten, zato poskrbite za učinkovitost in preglednost
 kode.
[*----------------------------------------------------------------------------*)

module type MACHINE = sig
  type t
  val make : state -> state list -> t
  val initial : t -> state
  val add_transition : state -> char -> state -> char -> direction -> t -> t
  val step : t -> state -> Tape.t -> (state * Tape.t) option
end

module Machine : MACHINE = struct
  type t = {
    initial_state : state;
    states : state list;
    transitions : (state * char * state * char * direction) list;
  } (*prvi state je trenutni, prvi char je začetni v head, drugi state je nov in drugi char tisti, ki ga napiše*)
  
  let make initial_state states =
    {initial_state; states; transitions = []}

  let initial machine =
    machine.initial_state

  let add_transition trenutno beri_c next pisi_c dir machine =
    {machine with transitions = (trenutno, beri_c, next, pisi_c, dir) :: machine.transitions}

  let step machine trenutno_st tape =
    let char_head = Tape.read tape in
    let najdi_transition (state, read, _, _, _) =
      state = trenutno_st && read = char_head in
    match List.find_opt najdi_transition machine.transitions with
    | None -> None
    | Some (_, _, next, pisi_c, direction) ->
        let nov_tape = Tape.write pisi_c tape in
        let move_tape = Tape.move direction nov_tape in
        Some (next, move_tape)
end

(*----------------------------------------------------------------------------*
 Primer stroja "Binary Increment" na <http://turingmachine.io> lahko
 implementiramo kot:
[*----------------------------------------------------------------------------*)

let binary_increment =
  Machine.(
    make "right" [ "carry"; "done" ]
    |> add_transition "right" '1' "right" '1' Right
    |> add_transition "right" '0' "right" '0' Right
    |> add_transition "right" ' ' "carry" ' ' Left
    |> add_transition "carry" '1' "carry" '0' Left
    |> add_transition "carry" '0' "done" '1' Left
    |> add_transition "carry" ' ' "done" '1' Left
  )

(* val binary_increment : Machine.t = <abstr> *)

(*----------------------------------------------------------------------------*
 Zapišite funkciji `slow_run` in `speed_run` tipa `Machine.t -> str -> unit`, ki
 simulirata Turingov stroj na traku, na katerem je na začetku zapisan dani niz.
 Prva naj izpiše trakove in stanja pri vseh vmesnih korakih, druga pa naj izpiše
 le končni trak. Slednjo bomo uporabljali tudi pri meritvi učinkovitosti
 izvajanja.
[*----------------------------------------------------------------------------*)

let slow_run machine input =
  let rec run trenutno_st tape =
    Tape.print tape;
    Printf.printf "%s\n" trenutno_st;
    match Machine.step machine trenutno_st tape with
    | None -> ()
    | Some (next, nov_tape) -> run next nov_tape
  in run (Machine.initial machine) (Tape.make input)

let speed_run machine input =
  let rec run trenutno_st tape =
    match Machine.step machine trenutno_st tape with
    | None -> tape
    | Some (next, nov_tape) -> run next nov_tape
  in let konec = run (Machine.initial machine) (Tape.make input)
  in Tape.print konec

(*----------------------------------------------------------------------------*
 ## Krajši zapis
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Ko definiramo Turingov stroj, prehode običajno združujemo najprej po stanjih,
 nato pa še po znakih. Prav tako pri dosti prehodih samo premikamo glavo, trak
 in stanje pa pustimo pri miru. Zapišite funkcije:

 - `for_state`
 - `for_character`
 - `for_characters`
 - `move`
 - `switch_and_move`
 - `write_and_move`
 - `write_switch_and_move`

 s katerimi bi lahko zgornji primer na krajše zapisali kot spodaj.
 Implementacijo in tipe ugotovite sami.
[*----------------------------------------------------------------------------*)

let for_state state transitions machine =
  List.

let for_character c (next, pisi_c, dir) =
  [(c, next, (match pisi_c with None -> c | Some nov_znak -> nov_znak), dir)]

let for_characters chars (next, pisi_c, dir) =
  let najdi_c char =
    let pisi = match pisi_c with
      | None -> char 
      | Some znak -> znak 
    in (char, next, pisi, dir)
  in List.init (String.length chars) (fun i -> najdi_c chars.[i])

let move dir = (None, None, dir)

let switch_and_move next dir = (Some next, None, dir)

let write_and_move pisi_c dir = (None, Some pisi_c, dir)

let write_switch_and_move pisi_c next dir = (Some next, Some pisi_c, dir)

(*----------------------------------------------------------------------------*
 ## Primeri Turingovih strojev
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Pri tej nalogi boste sestavljali stroje, ki bodo iz začetnega niza na traku na
 različne načine izračunali nov niz. Pri tem lahko predpostavite, da je začetni
 niz sestavljen iz ničel in enic, preostanek traku pa je prazen. Na koncu
 izvajanja naj bo glava na začetku novega niza, z izjemo tega niza pa naj bo
 trak prazen. Ni pa treba, da se izračunani niz začne na istem mestu na traku,
 kot se je začel prvotni niz.
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 ### Obračanje niza
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Sestavite Turingov stroj, ki začetni niz obrne na glavo.
[*----------------------------------------------------------------------------*)

let reverse =
  Machine.make ""

(*let primer_reverse = speed_run reverse "0000111001"*)
(* 
1001110000          
^
*)
(* val primer_reverse : unit = () *)

(*----------------------------------------------------------------------------*
 ### Podvajanje niza
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Sestavite Turingov stroj, ki podvoji začetni niz.
[*----------------------------------------------------------------------------*)

let duplicate =
  Machine.make "right" ["zacetek"; "beri"; "pisi_0x2"; "nesi_eno_0";
   "pisi_1x2"; "nesi_eno_1"; "done"]
  |> for_state "right" [
    for_characters "01" @@ move Right;
    for_character ' ' @@ write_switch_and_move 'C' "zacetek" Left
  ]
  |> for_state "zacetek" [
    for_characters "01C" @@ move Left;
    for_character ' ' @@ switch_and_move "beri" Right
  ]
  |> for_state "beri" [
    for_character '0' @@ write_switch_and_move ' ' "pisi_0x2" Right;
    for_character '1' @@ write_switch_and_move ' ' "pisi_1x2" Right;
    for_character 'C' @@ write_switch_and_move ' ' "done" Right;
  ]
  |> for_state "pisi_0x2" [
    for_characters "01C" @@ move Right;
    for_character ' ' @@ write_switch_and_move '0' "pisi_0x1" Right
  ]
  |> for_state "pisi_1x2" [
    for_characters "01C" @@ move Right;
    for_character ' ' @@ write_switch_and_move '1' "pisi_1x1" Right
  ]
  |> for_state "pisi_0x1" [
    for_character ' ' @@ write_switch_and_move '0' "zacetek" Left
  ]
  |> for_state "pisi_1x1" [
    for_character ' ' @@ write_switch_and_move '1' "zacetek" Left
  ]

(*----------------------------------------------------------------------------*
 ### Eniški zapis
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Sestavite Turingov stroj, ki na začetku na traku sprejme število $n$, zapisano
 v dvojiškem zapisu, na koncu pa naj bo na traku zapisanih natanko $n$ enic.
[*----------------------------------------------------------------------------*)

let to_unary = 
  Machine.make 

(*let primer_to_unary = speed_run to_unary "1010"*)
(* 
1111111111
^
*)
(* val primer_to_unary : unit = () *)

(*----------------------------------------------------------------------------*
 ### Dvojiški zapis
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Sestavite ravno obratni Turingov stroj, torej tak, ki na začetku na traku
 sprejme število $n$ enic, na koncu pa naj bo na traku zapisano število $n$ v
 dvojiškem zapisu.
[*----------------------------------------------------------------------------*)

let to_binary =
  Machine.make "zapisi" []

(*let primer_to_binary = speed_run to_binary (String.make 42 '1')*)
(* 
101010                                           
^
*)
(* val primer_to_binary : unit = () *)
