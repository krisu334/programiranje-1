(*----------------------------------------------------------------------------*
 # 2. domača naloga
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Pri tej nalogi boste napisali svoj simulator računalnika, ki se bo malenkostno
 razlikoval od [tistega, ki smo ga spoznali na
 predavanjih](https://schweigi.github.io/assembler-simulator/):
 - Simulator bo uporabljal Harvardsko arhitekturo, kar pomeni, da bo ločil med
 pomnilnikoma za program in podatke.
 - Namesto pomnilnika z omejeno velikostjo bomo imeli samo sklad, ki ga bomo
 predstavili s poljubno velikim seznamom.
 - Prav tako ne bomo vsega predstavili z 8-bitnimi števili. Za ukaze bomo
 definirali svoj naštevni tip, števila v pomnilniku pa bodo taka, kot jih
 podpira OCaml.
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 ## Podatkovni tipi
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Pri vsakem večjem funkcijskem programu je prvi korak definicija ustreznih
 tipov. V simulatorju bomo imeli dva glavna tipa: `instruction`, s katerim bomo
 predstavili posamezne ukaze v programu, in `state`, s katerim bomo predstavili
 trenutno stanje računalnika. Seveda pa si bomo morali pred njima definirati še
 nekaj pomožnih tipov.
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 ### Registri
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Nekateri ukazi za argument sprejmejo register, ki ga spreminjajo, na primer:
 `INC A` ali `POP B`.

 Definirajte naštevni tip `register`, ki bo predstavljal štiri možne registre
 procesorja **A**, **B**, **C** in **D**.
[*----------------------------------------------------------------------------*)

type register = A | B | C | D

(*----------------------------------------------------------------------------*
 ### Izrazi
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Nekateri ukazi poleg registra sprejmejo še dodaten argument, ki je lahko bodisi
 register, bodisi celoštevilska konstanta, na primer `MOV A, B` ali `MOV A, 42`.
 Definirajte naštevni tip `expression`, ki predstavlja izraze, ki so lahko
 registri ali števila.
[*----------------------------------------------------------------------------*)

type expression =
  | Register of register
  | Const of int

(*----------------------------------------------------------------------------*
 ### Naslovi
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Ukazi za skoke za argument sprejmejo naslov ukaza v pomnilniku. Naslove bomo
 predstavili s celimi števili, da pa jih ne bi ponesreči zamešali s
 celoštevilskimi konstantami, definirajte še tip `address`, ki naj bo naštevni
 tip z eno samo varianto `Address` s celoštevilskim argumentom.
[*----------------------------------------------------------------------------*)

type address = Address of int

(*----------------------------------------------------------------------------*
 ### Ukazi
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Naš simulator bo podpiral naslednje ukaze, pri čemer je _R_ vedno poljuben
 register, _A_ naslov v ukaznem pomnilniku, _E_ pa izraz, torej bodisi register
 bodisi celoštevilska konstanta.

 ukaz                      | opis
 ------------------------: | ---------------------------------------------------
 --------------------------------------------------
 `MOV` _R_, _E_            | premakni vrednost izraza _E_ v register _R_
 `ADD`/`SUB` _R_, _E_      | register _R_ povečaj/zmanjšaj za _E_
 `INC`/`DEC` _R_           | register _R_ povečaj/zmanjšaj za 1
 `MUL`/`DIV` _E_           | register **A** pomnoži/deli z _E_
 `AND`/`OR`/`XOR` _R_, _E_ | v register _R_ shrani rezultat logične operacije _R
 op E_
 `NOT` _R_                 | negiraj register _R_
 `CMP` _R_, _E_            | primerjaj register _R_ z vrednostjo _E_ ter
 rezultat primerjave shrani v zastavici **Zero** in **Carry**
 `JMP` _A_                 | skoči na naslov _A_
 `JA`/`JAE` _A_            | skoči na naslov _A_, če je v zadnji primerjavi
 veljalo _x > y_ / _x ≥ y_
 `JB`/`JBE` _A_            | skoči na naslov _A_, če je v zadnji primerjavi
 veljalo _x < y_ / _x ≤ y_
 `JE`/`JNE` _A_            | skoči na naslov _A_, če je v zadnji primerjavi
 veljalo _x = y_ / _x ≠ y_
 `CALL` _A_                | skoči na naslov _A_ in shrani naslov naslednjega
 ukaza na vrh sklada
 `RET`                     | iz funkcije se vrni na naslov na vrhu sklada
 `PUSH` _E_                | vrednost izraza _E_ shrani na vrh sklada
 `POP` _R_                 | snemi vrednost s sklada in jo shrani v register _R_
 `HLT`                     | ustavi izvajanje programa
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Dopolnite naslednjo definicijo tipa `instruction`, da bo imel eno varianto za
 vsakega od zgoraj navedenih ukazov:
[*----------------------------------------------------------------------------*)

type instruction =
  | MOV of register * expression
  | ADD of register * expression
  | SUB of register * expression
  | INC of register
  | DEC of register
  | MUL of expression
  | DIV of expression
  | AND of register * expression
  | OR of register * expression
  | XOR of register * expression
  | NOT of register
  | CMP of register * expression
  | JMP of address
  | JA of address
  | JAE of address
  | JB of address
  | JBE of address
  | JE of address
  | JNE of address
  | CALL of address
  | RET
  | PUSH of expression
  | POP of register
  | HLT

(*----------------------------------------------------------------------------*
 Za primer večjega programa se spomnimo programa za izračun Fibonaccijevih
 števil. S seznamom ukazov, bi ga napisali kot spodaj. Pri tem opazite, da so
 naslovi ukazov v programu zapisani kot celoštevilski indeksi. Pretvorbo iz
 berljivih oznak kot so `main`, `fib` in `.fib_end` bomo obravnavali kasneje.
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 ### Pomnilnik
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Morda v nasprotju s pričakovanji ukazov ne bomo shranjevali v sezname tipa
 `instruction list`, ampak v tabele tipa `instruction array`. O tabelah se bomo
 še pogovarjali, njihova bistvena prednost pa je ta, da do elementa na danem
 mestu lahko dostopamo takoj, ne da bi se morali sprehoditi po predhodnih
 elementih. Tabele pišemo tako kot sezname, le da oklepaje pišemo kot `[| ...
 |]` namesto kot `[ ... ]`, do posameznega elementa tabele pa dostopamo prek
 `tabela.(indeks)`, na primer `[| 314; 42; 2718 |].(1)` vrne `42`.
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Nazadnje bomo celotno stanje računalnika predstavili z zapisnim. Definirajte
 tip `state` s sledečimi polji:
 - `instructions`: tabela ukazov v ukaznem pomnilniku,
 - `a`, `b`, `c`, `d`: štiri celoštevilske vrednosti v registrih,
 - `ip`: naslov trenutnega ukaza, tipa `address`,
 - `zero`, `carry`: vrednosti zastavic **Zero** in **Carry**,
 - `stack`: seznam celoštevilskih vrednosti na skladu.
[*----------------------------------------------------------------------------*)

type state = {
  instructions : instruction array;
  a : int;
  b : int;
  c : int;
  d : int;
  ip : address;
  zero : bool;
  carry : bool;
  stack : int list;
}

(*----------------------------------------------------------------------------*
 ### Začetno stanje
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Prazno stanje pomnilnika lahko predstavimo z zapisom:
[*----------------------------------------------------------------------------*)

let empty = {
  instructions = [||];
  a = 0;
  b = 0;
  c = 0;
  d = 0;
  ip = Address 0;
  zero = false;
  carry = false;
  stack = [];
} 
(* val empty : state =
  {instructions = [||]; a = 0; b = 0; c = 0; d = 0; ip = Address 0;
   zero = false; carry = false; stack = []} *)

(*----------------------------------------------------------------------------*
 Kljub temu, da so tabele učinkovitejše, so seznami za delo bolj praktični. Zato
 definirajte funkcijo `init : instruction list -> state`, ki sprejme seznam
 ukazov in vrne začetno stanje računalnika, v katerem so vsi registri in
 zastavice nastavljeni na nič, sklad pa je prazen. Pri tem si lahko za pretvorbo
 seznama v tabelo pomagate z uporabo funkcije `Array.of_list`.
[*----------------------------------------------------------------------------*)

let init (navodila : instruction list) : state = {
  instructions = Array.of_list navodila;
  a = 0;
  b = 0;
  c = 0;
  d = 0;
  ip = Address 0;
  zero = false;
  carry = false;
  stack = [];
  }

(*----------------------------------------------------------------------------*
 ## Izvajanje ukazov
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 S pripravljenima tipoma ukazov in stanja se lahko lotimo pisanja funkcij za
 izvrševanje ukazov.
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 ### Branje stanja
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Napišite funkcijo `read_instruction : state -> instruction option`, ki v danem
 stanju vrne trenuten ukaz. Če ukaz sega izven območja ukaznega pomnilnika, naj
 funkcija vrne `None`.
[*----------------------------------------------------------------------------*)

let read_instruction (st : state) : instruction option =
  let ip_index = match st.ip with Address naslov -> naslov in
  if ip_index >= 0 && ip_index < Array.length st.instructions then
    Some (st.instructions.(ip_index))
  else
    None

(*----------------------------------------------------------------------------*
 Napišite funkcijo `read_register : state -> register -> int`, ki vrne vrednost
 registra v danem stanju.
[*----------------------------------------------------------------------------*)

let read_register (st : state) (reg : register) : int =
  match reg with
  | A -> st.a
  | B -> st.b
  | C -> st.c
  | D -> st.d

(*----------------------------------------------------------------------------*
 Napišite funkcijo `read_expression : state -> expression -> int`, ki vrne
 celoštevilsko vrednost izraza v danem stanju.
[*----------------------------------------------------------------------------*)

let read_expression (st : state) (expr : expression) : int =
  match expr with
  | Register reg -> read_register st reg
  | Const n -> n 

(*----------------------------------------------------------------------------*
 ### Spreminjanje registrov
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Napišite funkcijo `write_register : state -> register -> int -> state`, ki
 vrednost registra v danem stanju nastavi na dano število. Funkcija naj vrne
 novo stanje.
[*----------------------------------------------------------------------------*)

let write_register (st : state) (reg : register) (vrednost : int) : state =
  match reg with
  | A -> { st with a = vrednost }
  | B -> { st with b = vrednost }
  | C -> { st with c = vrednost }
  | D -> { st with d = vrednost }

(*----------------------------------------------------------------------------*
 Napišite funkcijo `perform_unop : (int -> int) -> state -> register -> state`,
 ki izvede eniško operacijo na vrednosti registra. Funkcija naj vrne novo stanje
 s spremenjenim registrom.
[*----------------------------------------------------------------------------*)

let perform_unop (f : int -> int) (st : state) (reg : register) : state =
  let trenutno = read_register st reg in
  let nova = f trenutno in
  write_register st reg nova

(*----------------------------------------------------------------------------*
 Napišite funkcijo `perform_binop : (int -> int -> int) -> state -> register ->
 expression -> state`, ki izvede dvojiško operacijo na danem registru in izrazu.
 Funkcija naj vrne novo stanje s spremenjenim registrom.
[*----------------------------------------------------------------------------*)

let perform_binop (f : int -> int -> int) (st : state) (reg : register) (expr : expression) : state =
  let reg_vrednost = read_register st reg in
  let expr_vrednost = read_expression st expr in
  let new_vrednost = f reg_vrednost expr_vrednost in
  write_register st reg new_vrednost

(*----------------------------------------------------------------------------*
 ### Skoki
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Napišite funkcijo `next : address -> address`, ki vrne naslednji naslov (torej
 povečan za 1, saj v našem primeru vsi ukazi zasedejo enako prostora).
[*----------------------------------------------------------------------------*)

let next (addr : address) : address =
  match addr with
  | Address n -> Address (n + 1)

(*----------------------------------------------------------------------------*
 Napišite funkciji `jump : state -> address -> state` in `proceed : state ->
 state`. Prva naj v danem stanju skoči na dani naslov, druga pa naj skoči na
 naslednji ukaz.
[*----------------------------------------------------------------------------*)

let jump (st : state) (addr : address) : state =
  {st with ip = addr}
let proceed (st : state) : state =
  let next_ip = next st.ip in
  {st with ip = next_ip}

(*----------------------------------------------------------------------------*
 Napišite funkciji `push_stack : state -> int -> state` in `pop_stack : state ->
 int * state`, ki dodata vrednost na sklad oziroma jo odstranita z njega.
 Funkcija `pop_stack` poleg spremenjenega stanja vrne tudi odstranjeno vrednost.
 Če je sklad prazen, naj funkcija `pop_stack` sproži izjemo.
[*----------------------------------------------------------------------------*)

let push_stack (st : state) (vrednost : int) : state =
  {st with stack = vrednost :: st.stack}
let pop_stack (st : state) : int * state =
  match st.stack with
  | [] -> failwith "Ni elementa za odstraniti."
  | prvi :: ostali -> (prvi, {st with stack = ostali})

(*----------------------------------------------------------------------------*
 ### Pogojni skoki
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Napišite funkcijo `compare : state -> int -> int -> state`, ki primerja
 vrednosti dveh števil in ustrezno nastavi zastavici **Zero** in **Carry**. Prvo
 naj nastavi na `true` natanko tedaj, kadar sta števili enaki, drugo pa takrat,
 kadar je prvo število manjše.Funkcija naj vrne novo stanje.
[*----------------------------------------------------------------------------*)

let compare state x y =
  let zero_zastava = (x = y) in
  let carry_zastava = (x < y) in
  {state with zero = zero_zastava; carry = carry_zastava}

(*----------------------------------------------------------------------------*
 Napišite funkcijo `conditional_jump : state -> bool -> address -> state`, ki
 skoči na dani naslov, če je podan pogoj izpolnjen. V nasprotnem primeru naj
 funkcija skoči na naslednji ukaz.
[*----------------------------------------------------------------------------*)

let conditional_jump state dani_naslov pogoj =
  if pogoj then
    {state with ip = dani_naslov}
  else
    {state with ip = next state.ip}

(*----------------------------------------------------------------------------*
 ### Klici funkcij
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Napišite funkcijo `call : state -> address -> state`, ki v danem stanju skoči
 na dani naslov in na sklad doda naslednji naslov.
[*----------------------------------------------------------------------------*)

let call state target_address =
  let Address n = state.ip in
  let dodamo = n + 1 in
  let nov_stack = dodamo :: state.stack in
  { state with ip = target_address; stack = nov_stack }

(*----------------------------------------------------------------------------*
 Napišite funkcijo `return : state -> state`, ki v danem stanju skoči na naslov,
 ki je na vrhu sklada, in odstrani ta naslov s sklada.
[*----------------------------------------------------------------------------*)

let return state =
  let return_address, nov_state = pop_stack state in
  { nov_state with ip = Address return_address }

(*----------------------------------------------------------------------------*
 ### Izvajanje programov
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 S pomočjo zgoraj definiranih funkcij dopolnite funkcijo `run_instruction :
 state -> instruction -> state`, ki izvede podani ukaz v danem stanju in vrne
 novo stanje. Za interpretacije pogojnih skokov si lahko pomagate z [navodili
 simulatorja](https://schweigi.github.io/assembler-simulator/instruction-
 set.html), ki smo ga pogledali na predavanjih.
[*----------------------------------------------------------------------------*)

let run_instruction st = function
  | MOV (reg, exp) -> 
      let premik = read_expression st exp in 
      write_register st reg premik |> proceed
  | ADD (reg, exp) -> perform_binop ( + ) st reg exp |> proceed
  | SUB (reg, exp) -> perform_binop ( - ) st reg exp |> proceed
  | INC reg -> perform_unop succ st reg |> proceed
  | DEC reg -> perform_unop pred st reg |> proceed
  | MUL exp -> perform_binop ( * ) st A exp |> proceed
  | DIV exp -> 
      let divisor = read_expression st exp in 
      if divisor = 0 then failwith "Deljenje z 0."
      else perform_binop ( / ) st A exp |> proceed
  | AND (reg, exp) -> perform_binop ( land ) st reg exp |> proceed
  | OR (reg, exp) -> perform_binop ( lor ) st reg exp |> proceed
  | XOR (reg, exp) -> perform_binop ( lxor ) st reg exp |> proceed
  | NOT reg -> perform_unop lnot st reg |> proceed
  | CMP (reg, exp) -> 
      let reg_value = read_register st reg in 
      let exp_value = read_expression st exp in 
      compare st reg_value exp_value |> proceed
  | JMP add -> jump st add
  | JA add -> conditional_jump st add (not st.carry && not st.zero)
  | JAE add -> conditional_jump st add (not st.carry)
  | JB add -> conditional_jump st add st.carry
  | JBE add -> conditional_jump st add (st.carry || st.zero)
  | JE add -> conditional_jump st add st.zero
  | JNE add -> conditional_jump st add (not st.zero)
  | CALL add -> call st add
  | RET -> return st
  | PUSH exp -> push_stack st (read_expression st exp) |> proceed
  | POP reg ->
      let n, st' = pop_stack st in
      write_register st' reg n |> proceed
  | HLT -> failwith "Cannot execute instruction" 

(*----------------------------------------------------------------------------*
 Napišite funkcijo `run_program : state -> state`, ki izvaja ukaze v danem
 stanju, dokler ne naleti na ukaz `HLT` ali pa ukazni kazalec skoči ven iz
 ukaznega pomnilnika. Funkcija naj vrne končno stanje.
[*----------------------------------------------------------------------------*)

let rec run_program state =
  let ip_val =
    match state.ip with Address n -> n
  in if ip_val < 0 || ip_val >= Array.length state.instructions then state
  else
    let instr = state.instructions.(ip_val) in
    match instr with
    | HLT -> state
    | _ -> run_program (run_instruction state instr)

(*----------------------------------------------------------------------------*
 ## Branje zbirnika
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Da bomo programe lahko pisali v zbirniku, napišimo še funkcije za branje nizov.
 Predpostavljate lahko, da bodo vsi nizi pravilno oblikovani, zato v primeru
 napake s `failwith ...` javite ustrezno sporočilo o napaki.
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 ### Registri in izrazi
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Napišite funkcijo `parse_register : string -> register`, ki iz niza prebere
 register.
[*----------------------------------------------------------------------------*)

let parse_register str =
  match String.uppercase_ascii str with
  | "A" -> A
  | "B" -> B
  | "C" -> C
  | "D" -> D
  | _ -> failwith ("Neveljaven register.")

(*----------------------------------------------------------------------------*
 Napišite funkcijo `parse_expression : string -> expression`, ki iz niza prebere
 izraz.
[*----------------------------------------------------------------------------*)

let parse_expression str =
  try
    Const (int_of_string str)
  with Failure _ ->
    Register (parse_register str)

(*----------------------------------------------------------------------------*
 ### Čiščenje vrstic
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Napišite funkcijo `clean_line : string -> string`, ki iz niza odstrani vse
 presledke in komentarje (ki se začnejo z znakom `;`). Pri iskanju in
 odstranjevanju komentarjev si pomagajte z uporabo funkcij `String.index_opt` in
 `String.sub`.
[*----------------------------------------------------------------------------*)

let clean_line _ = ()

let primer_branje_4 = clean_line "   MOV A, 42    ; To je komentar   "
(* val primer_branje_4 : string = "MOV A, 42" *)

(*----------------------------------------------------------------------------*
 Napišite funkcijo `clean_lines : string list -> string list`, ki iz seznama
 nizov najprej odstrani vse komentarje in presledke, nato pa odstrani vse prazne
 vrstice.
[*----------------------------------------------------------------------------*)

let clean_lines _ = ()

(*----------------------------------------------------------------------------*
 ### Oznake
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Kot smo navajeni iz zbirnika, skokov ne podajamo z indeksi, ampak raje v dele
 kode napišemo oznake kot so `main:` ali `.loop:`, nato pa se nanje sklicujemo
 kot `JA .loop`, `JMP main`, `CALL fib` in tako naprej. Oznake bomo hranili v
 seznamu, ki bo vsaki oznaki priredil ustrezen naslov v ukaznem pomnilniku.
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Napišite funkcijo `parse_address : (string * address) list -> string ->
 address`, ki pri danem seznamu oznak iz niza prebere naslov. Naslov je lahko
 podan direktno s številom ali pa z eno izmed oznak v seznamu.
[*----------------------------------------------------------------------------*)

let parse_address _ _ = ()

(* let primer_branje_5 = parse_address [("main", Address 42)] "main" *)
(* val primer_branje_5 : address = Address 42 *)

(* let primer_branje_6 = parse_address [("main", Address 42)] "123" *)
(* val primer_branje_6 : address = Address 123 *)

(*----------------------------------------------------------------------------*
 Napišite funkcijo `parse_label : string -> string option`, ki vrne oznako, če
 se niz konča z dvopičjem, sicer pa vrne `None`.
[*----------------------------------------------------------------------------*)

let parse_label _ = ()

let primer_branje_7 = parse_label "main:"
(* val primer_branje_7 : string option = Some "main" *)

let primer_branje_8 = parse_label "MOV A, 42"
(* val primer_branje_8 : string option = None *)

(*----------------------------------------------------------------------------*
 Da bomo iz kode določili oznake, napišite funkcijo `parse_labels : string list
 -> (string * address) list * string list`, ki iz seznama nizov, ki so bodisi
 oznake bodisi ukazi, izloči oznake in jim priredi naslove, ostale vrstice pa
 pusti nespremenjene.
[*----------------------------------------------------------------------------*)

let parse_labels _ = ()

let primer_branje_9 =
  parse_labels ["JMP main"; "main:"; "MOV A, 0"; "loop:"; "INC A"; "JMP loop"]
(* val primer_branje_9 : (string * address) list * string list =
  ([("loop", Address 2); ("main", Address 1)],
   ["JMP main"; "MOV A, 0"; "INC A"; "JMP loop"]) *)

(*----------------------------------------------------------------------------*
 Dopolnite spodnjo funkcijo `parse_instruction : (string * address) list ->
 string -> instruction`, ki iz niza prebere ukaz.
[*----------------------------------------------------------------------------*)

(* let parse_instruction labels line =
  let tokens =
    line
    |> String.split_on_char ' '
    |> List.concat_map (String.split_on_char ',')
    |> List.map String.trim
    |> List.filter (fun token -> token <> "")
  in
  match tokens with
  | ["MOV"; reg; exp] -> MOV (parse_register reg, parse_expression exp)
  (* | ["ADD"; reg; exp] -> TODO *)
  (* | ["SUB"; reg; exp] -> TODO *)
  | ["INC"; reg] -> INC (parse_register reg)
  (* | ["DEC"; reg] -> TODO *)
  (* | ["MUL"; exp] -> TODO *)
  (* | ["DIV"; exp] -> TODO *)
  (* | ["AND"; reg; exp] -> TODO *)
  (* | ["OR"; reg; exp] -> TODO *)
  (* | ["XOR"; reg; exp] -> TODO *)
  (* | ["NOT"; reg] -> TODO *)
  (* | ["CMP"; reg; exp] -> TODO *)
  | ["JMP"; add] -> JMP (parse_address labels add)
  (* | ["JA"; add] -> TODO *)
  (* | ["JAE"; add] -> TODO *)
  (* | ["JB"; add] -> TODO *)
  (* | ["JBE"; add] -> TODO *)
  (* | ["JE"; add] -> TODO *)
  (* | ["JNE"; add] -> TODO *)
  (* | ["CALL"; add] -> TODO *)
  | ["RET"] -> RET
  | ["PUSH"; exp] -> PUSH (parse_expression exp)
  | ["POP"; reg] -> POP (parse_register reg)
  | ["HLT"] -> HLT
  | _ -> failwith ("Invalid instruction: " ^ line) *)

(* let primer_branje_10 =
  List.map (parse_instruction [("main", Address 42)]) ["MOV A, 42"; "CALL main"; "HLT"] *)
(* val primer_branje_10 : instruction list =
  [MOV (A, Const 42); CALL (Address 42); HLT] *)

(*----------------------------------------------------------------------------*
 S pomočjo zgoraj napisanih funkcij sestavite funkcijo `run : string -> state`,
 ki niz razbije na vrstice, prebere ukaze in oznake ter pripravi začetno stanje,
 nato pa program izvaja vse dokler ne naleti na ukaz `HLT`. Po klicu naj
 funkcija vrne končno stanje.
[*----------------------------------------------------------------------------*)

let run _ = ()

let fibonacci = {|
  JMP main
  ; Funkcija, ki izračuna fib(A) in vrednost shrani v register A
  fib:
      ; Shranimo vrednosti registrov
      PUSH C
      PUSH B
  
      ; V C shranimo začetno vrednost A
      MOV C, A
  
      ; Če je A = 0, je to tudi rezultat
      CMP A, 0
      JE .fib_end
  
      ; Če je A = 1, je to tudi rezultat
      CMP A, 1
      JE .fib_end
  
      ; V nasprotnem primeru najprej izračunamo fib(A - 1) in ga shranimo v B
      DEC C
      MOV A, C
      CALL fib
      MOV B, A
  
      ; Nato izračunamo še fib(A - 2) in ga shranimo v A
      DEC C
      MOV A, C
      CALL fib
      
      ; Nazadnje k A prištejemo še B, s čimer dobimo končni rezultat
      ADD A, B
      JMP .fib_end
  
  .fib_end:
      ; Povrnemo vrednosti registrov in vrnemo rezultat
      POP B
      POP C
      RET
  
  main:
      MOV A, 7
      CALL fib
  HLT
|}
(* val fibonacci : string =
  "\n  JMP main\n  ; Funkcija, ki izračuna fib(A) in vrednost shrani v register A\n  fib:\n      ; Shranimo vrednosti registrov\n      PUSH C\n      PUSH B\n  \n      ; V C shranimo začetno vrednost A\n      MOV C, A\n  \n      ; Če je A = 0, je to tudi rezultat\n      CMP A, 0\n      JE .fib_end\n  \n      ; Če"... (* string length 872; truncated *) *)

let primer_branje_11 =
  run fibonacci
(* val primer_branje_11 : state =
  {instructions =
    [|JMP (Address 20); PUSH (Register C); PUSH (Register B);
      MOV (C, Register A); CMP (A, Const 0); JE (Address 17);
      CMP (A, Const 1); JE (Address 17); DEC C; MOV (A, Register C);
      CALL (Address 1); MOV (B, Register A); DEC C; MOV (A, Register C);
      CALL (Address 1); ADD (A, Register B); JMP (Address 17); POP B; 
      POP C; RET; MOV (A, Const 7); CALL (Address 1); HLT|];
   a = 13; b = 0; c = 0; d = 0; ip = Address 22; zero = true; carry = false;
   stack = []} *)
