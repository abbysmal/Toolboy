module R = Rresult.R

type t =
  Write of (int * int)
| Wait of int
| Osef

open Angstrom

(* https://vgmrips.net/wiki/VGM_Specification *)

let magic = LE.int32 0x206d6756l

let ww =
  any_char >>= fun n ->
  if (Char.code n) >= 0x70 && (Char.code n) < 0x80 then
    return (Wait ( ( (Char.code n) mod 0x70) + 1 ))
  else return Osef

let w =
  int8 0x61 >>= fun _ -> LE.any_int16 >>= fun n -> return (Wait n)

let b =
  let wr = int8 0xB3 in
  wr >>= fun _ ->
  any_char >>= fun _reg ->
  any_char >>= fun _v ->
  return (Write (Char.code _reg,Char.code _v))

let parse_header =
  magic >>= fun () ->
  advance (0x34 - 4) >>= fun () ->
  LE.any_int32 >>= fun offset ->
  advance (0x80 - 0x38) >>= fun () ->
  LE.any_int32 >>= fun clock ->
  Printf.printf "clock: %ld\n" clock;
  flush_all ();
  advance ((Int32.to_int offset) - 0x40) >>= fun () ->
  many (b <|> w <|> ww)
