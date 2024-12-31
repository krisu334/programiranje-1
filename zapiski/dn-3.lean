open Classical
set_option autoImplicit false

/------------------------------------------------------------------------------
 ## Naravna števila

 Definirajte funkcijo, ki _rekurzivno_ (torej naivno in ne direktno s formulo,
 ki jo boste morali dokazati) sešteje prvih `n` naravnih števil, ter
 dokažite, da zanjo velja znana enakost (najprej v obliki, ki ne zahteva
 deljenja, nato pa še v običajni obliki).
------------------------------------------------------------------------------/

def vsota_prvih : Nat → Nat :=
  fun n => match n with
  | 0 => 0
  | n + 1 => n + 1 + vsota_prvih n

theorem gauss : (n : Nat) → 2 * vsota_prvih n = n * (n + 1) := by
  intro n
  induction n with
  | zero =>
    simp [vsota_prvih]
  | succ n ih =>
    calc
    2 * vsota_prvih (n + 1) = 2 * ((n + 1) + vsota_prvih n) := by simp [vsota_prvih]
    _  = 2 * (n + 1) + 2 * vsota_prvih n := by rw [Nat.mul_add]
    _  = 2 * (n + 1) + n * (n + 1) := by rw [ih]
    _  = 2 * (n + 1) + (n + 1) * n := by rw [Nat.mul_comm n (n + 1)]
    _  = (n + 1) * 2 + (n + 1) * n := by rw [Nat.mul_comm (n + 1) 2]
    _  = (n + 1) * (2 + n) := by rw [Nat.left_distrib]
    _  = (n + 1) * (n + 2) := by simp [Nat.add_comm]

theorem cisto_pravi_gauss : (n : Nat) → vsota_prvih n = (n * (n + 1)) / 2 := by
  intro n
  have gauss' := gauss n
  calc

/------------------------------------------------------------------------------
 ## Vektorji

 Definirajmo vektorje podobno kot na predavanjih, le da namesto svojih naravnih
 števil uporabimo vgrajena. Da se tipi ujamejo, funkcijo stikanja napišemo s
 pomočjo taktik.

 Napišite funkcijo `obrni`, ki vrne na glavo obrnjen vektor, ter funkciji
 `glava` in `rep`, ki varno vrneta glavo in rep _nepraznega_ seznama.
------------------------------------------------------------------------------/

inductive Vektor : Type → Nat → Type where
  | prazen : {A : Type} → Vektor A 0
  | sestavljen : {A : Type} → {n : Nat} → A → Vektor A n → Vektor A (n + 1)
deriving Repr

def stakni : {A : Type} → {m n : Nat} → Vektor A m → Vektor A n → Vektor A (m + n) :=
  fun xs ys => match xs with
  | .prazen => by rw [Nat.add_comm]; exact ys
  | .sestavljen x xs' => by rw [Nat.add_right_comm]; exact Vektor.sestavljen x (stakni xs' ys)

def obrni : {A : Type} → {n : Nat} → Vektor A n → Vektor A n :=
  fun v => match v with
  | .prazen => Vektor.prazen
  | .sestavljen x xs => stakni (obrni xs) (Vektor.sestavljen x Vektor.prazen)

def glava : {A : Type} → {n : Nat} → Vektor A (n + 1) → A :=
  fun v => match v with
  | Vektor.sestavljen x _ => x

def rep : {A : Type} → {n : Nat} → Vektor A (n + 1) → Vektor A n :=
  fun v => match v with
  | Vektor.sestavljen _ xs => xs

/------------------------------------------------------------------------------
 ## Predikatni račun

 Dokažite spodnje tri trditve. Zadnja je _paradoks pivca_, ki pravi:
   "V vsaki neprazni gostilni obstaja gost, za katerega velja,
   da če pije on, pijejo vsi v gostilni."
 Za dokaz potrebujete klasično logiko, torej nekaj iz modula `Classical`.
------------------------------------------------------------------------------/

theorem forall_implies : {A : Type} → {P Q : A → Prop} →
  (∀ x, (P x → Q x)) → (∀ x, P x) → (∀ x, Q x) := by
  intros A P Q h1 h2
  intro x
  exact h1 x (h2 x)

theorem forall_implies' : {A : Type} → {P : Prop} → {Q : A → Prop} →
  (∀ x, (P → Q x)) ↔ (P → ∀ x, Q x) := by
  intros A P Q
  apply Iff.intro
  . intros h hp x
    apply h x
    exact hp
  . intros h x hp
    apply h
    exact hp

theorem paradoks_pivca :
  {G : Type} → {P : G → Prop} →
  (g : G) →  -- (g : G) pove, da je v gostilni vsaj en gost
  ∃ (p : G), (P p → ∀ (x : G), P x) := by
  intros G P g


/------------------------------------------------------------------------------
 ## Dvojiška drevesa

 Podan naj bo tip dvojiških dreves skupaj s funkcijama za zrcaljenje in izračun
 višine ter dvema funkcijama, ki obe od leve proti desni naštejeta elemente
 drevesa. Pri tem prva deluje naivno in ima časovno zahtevnost O(n log n), druga
 pa je malo bolj zapletena in deluje v času O(n). Dokažite spodnje enakosti, pri
 čemer lahko do pomožne funkcije `aux` dostopate kot `elementi'.aux`
-------------------------------------------------------------------------------/

inductive Drevo : Type → Type where
  | prazno : {A : Type} → Drevo A
  | sestavljeno : {A : Type} → Drevo A → A → Drevo A → Drevo A

def zrcali : {A : Type} → Drevo A → Drevo A :=
  fun t => match t with
  | .prazno => .prazno
  | .sestavljeno l x d => .sestavljeno (zrcali d) x (zrcali l)

def visina : {A : Type} → Drevo A → Nat :=
  fun t => match t with
  | .prazno => 0
  | .sestavljeno l _ d => 1 + max (visina l) (visina d)

def elementi : {A : Type} → Drevo A → List A :=
  fun t => match t with
  | .prazno => []
  | .sestavljeno l x d => elementi l ++ x :: elementi d

def elementi' : {A : Type} → Drevo A → List A :=
  let rec aux : {A : Type} → Drevo A → List A → List A :=
    fun t acc => match t with
    | .prazno => acc
    | .sestavljeno l x d => aux l (x :: aux d acc)
  fun t => aux t []

theorem zrcali_zrcali :
  {A : Type} → (t : Drevo A) →
  zrcali (zrcali t) = t := by
  intro A t
  induction t with
  | prazno =>
    simp [zrcali]
  | sestavljeno l x d ih_l ih_d =>
    calc
    zrcali (zrcali (.sestavljeno l x d))
        = zrcali (.sestavljeno (zrcali d) x (zrcali l)) := by simp [zrcali]
    _   = .sestavljeno (zrcali (zrcali l)) x (zrcali (zrcali d)) := by simp [zrcali]
    _   = .sestavljeno l x d := by rw [ih_l, ih_d]

theorem visina_zrcali :
  {A : Type} → (t : Drevo A) →
  visina (zrcali t) = visina t := by
  intro A t
  induction t with
  | prazno =>
    simp [zrcali, visina]
  | sestavljeno l x d ih_l ih_d =>
    calc
    visina (zrcali (.sestavljeno l x d))
        = visina (.sestavljeno (zrcali d) x (zrcali l)) := by simp [zrcali]
    _   = 1 + max (visina (zrcali d)) (visina (zrcali l)) := by simp [visina]
    _   = 1 + max (visina d) (visina l) := by rw [ih_d, ih_l]
    _   = 1 + max (visina l) (visina d) := by rw [Nat.max_comm]
    _   = visina (.sestavljeno l x d) := by simp [visina]

theorem elementi_elementi' :
  {A : Type} → (t : Drevo A) →
  elementi t = elementi' t := by
  intro A t
  induction t with
  | prazno =>
    simp [elementi, elementi']
