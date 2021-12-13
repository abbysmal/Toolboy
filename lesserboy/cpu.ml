open Machine
 
type flag = Z | N | HC | CA
type register = A | B | C | D | E | F | H | L
type paired_register = AF | BC | DE | HL

let get_register s r =
  match r with
  | A -> s.cpu.a 
  | B -> s.cpu.b
  | C -> s.cpu.c
  | D -> s.cpu.d
  | E -> s.cpu.e
  | H -> s.cpu.h
  | L -> s.cpu.l
  | F ->
    ((if s.cpu.z then 0b10000000 else 0) lor 
     (if s.cpu.n then 0b01000000 else 0) lor
     (if s.cpu.hc then 0b00100000 else 0) lor 
     (if s.cpu.ca then 0b00010000 else 0))
  |> Uint8.inj [@@inline]

let get_register_pair s r =
  let aux a b =
    let a = get_register s a in
    let b = get_register s b in
    (Uint8.proj a lsl 8) lor Uint8.proj b
  in 
  match r with
  | AF -> aux A F
  | BC -> aux B C
  | DE -> aux D E
  | HL -> aux H L [@@inline]

let put_register m r v =
  match r with
  | A -> m.cpu.a <- v
  | B -> m.cpu.b <- v
  | C -> m.cpu.c <- v
  | D -> m.cpu.d <- v
  | E -> m.cpu.e <- v
  | H -> m.cpu.h <- v
  | L -> m.cpu.l <- v
  | F -> assert false [@@inline]

let put_register_pair s r v =
  match r with
  | AF ->
    let a = Uint8.inj (v lsr 8) in
    let f = Uint8.inj (v land 0b1111_1111) in
    s.cpu.a <- a;
    s.cpu.z <- Uint8.is_bit_set f 7;  
    s.cpu.n <- Uint8.is_bit_set f 6;  
    s.cpu.hc <- Uint8.is_bit_set f 5;  
    s.cpu.ca <- Uint8.is_bit_set f 4
  | BC ->
    let b = Uint8.inj (v lsr 8) in
    let c = Uint8.inj (v land 0b1111_1111) in
    s.cpu.b <- b;
    s.cpu.c <- c
  | DE ->
    let d = Uint8.inj (v lsr 8) in
    let e = Uint8.inj (v land 0b1111_1111) in
    s.cpu.d <- d;
    s.cpu.e <- e
  | HL ->
    let h = Uint8.inj (v lsr 8) in
    let l = Uint8.inj (v land 0b1111_1111) in
    s.cpu.h <- h;
    s.cpu.l <- l [@@inline]

let put_flags ?z ?n ?hc ?ca s =
  (match z with None -> ()  | Some z -> s.cpu.z <- z);
  (match n with None -> () | Some n -> s.cpu.n <- n);
  (match hc with None -> () | Some hc -> s.cpu.hc <- hc);
  (match ca with None -> () | Some ca -> s.cpu.ca <- ca)

let is_flag_set f m =
  match f with
  | Z -> m.cpu.z
  | N -> m.cpu.n
  | HC -> m.cpu.hc
  | CA -> m.cpu.ca [@@inline]
            
let tick s n =
  let t = s.cpu.t + n in
  let m = s.cpu.m + (n / 4) in
  s.cpu.t <- t;
  s.cpu.m <- m
  
let pp_flag ppf f =
  let s =
    match f with
    | Z -> "z"
    | N -> "n"
    | HC -> "hc"
    | CA -> "ca"
  in
  Format.fprintf ppf "%s" s
    
let pp_register ppf r =
  let s =
    match r with
    | A -> "a"
    | F -> "f"
    | B -> "b"
    | C -> "c"
    | D -> "d"
    | E -> "e"
    | H -> "h"
    | L -> "l"
  in
  Format.fprintf ppf "%s" s

let pp_paired_register ppf p =
  let s =
    match p with
    | AF -> "af"
    | BC -> "bc"
    | DE -> "de"
    | HL -> "hl"
  in
  Format.fprintf ppf "%s" s
