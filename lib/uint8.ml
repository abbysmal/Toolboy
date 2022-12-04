type t = char

let to_signed c =
  let c = Char.code c in
  if (c > 127) then
    -((lnot c+1) land 255)
  else
    c

let zero = Char.unsafe_chr 0
let one = Char.unsafe_chr 1

let max_int = 255

let proj = Char.code
let inj = Char.unsafe_chr

let add x y = (proj x + proj y) land max_int |> inj
let sub x y = (proj x - proj y) land max_int |> inj
let mul x y = (proj x * proj y) land max_int |> inj
let div x y = proj x / proj y |> inj
let rem x y = proj x mod proj y |> inj
let pred x = sub x one
let succ x = add x one

module Infix = struct

  let ( * ) = mul
  let ( + ) = add
  let ( - ) = sub
  let ( / ) = div
  let ( mod ) = rem

end

let ( lsl ) x y = (proj x lsl proj y) land max_int |> inj
let ( lsr ) x y = proj x lsr proj y |> inj
let ( land ) x y = proj x land proj y |> inj
let ( lor ) x y = proj x lor proj y |> inj
let ( lxor ) x y = proj x lxor proj y |> inj
let lnot x = x lxor (inj max_int)

let swap_nibbles x = ((x land (inj 0x0f)) lsl inj 4) lor ((x land (inj 0xF0)) lsr (inj 4))

let is_bit_set x bit = x land (one lsl (inj bit)) != zero
let is_bit_set' bit x = is_bit_set x bit
let set_bit x bit = x lor (one lsl (inj bit))
let set_bit' bit x = x lor (one lsl (inj bit))
let unset_bit x bit = x land (lnot (one lsl (inj bit)))
let unset_bit' bit x = x land (lnot (one lsl (inj bit)))
let toggle_bit x bit = x lxor (one lsl (inj bit))
let get_n_bits x n = (x lsl n) lsr n

let show_bin d =
  if d = 0x0 then "0" else
  let rec aux acc d =
    if d = 0x0 then acc else
    aux (string_of_int Int.(logand d 0x1) :: acc) Int.(shift_right_logical d 0x1)
  in
  String.concat "" (aux [] d)

let show_bin_u8 d =
  let d = proj d in
  let res = show_bin d in
  res

let show_bin_u16 d =
  let res = show_bin d in
  (String.make (16 - String.length res) '0') ^ res

let pp_hex ppf x = Format.fprintf ppf "0x%02X" (proj x)
let pp_hex' ppf x = Format.fprintf ppf "%02X" (proj x)
let pp_bin ppf x = Format.fprintf ppf "%s" (show_bin_u8 x)
let pp_bin' ppf x = Format.fprintf ppf "%s" (show_bin_u16 x)