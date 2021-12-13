open Uint8
open Machine

type key = [ `Down | `Up | `Right | `Left | `A | `B | `Select | `Start ]
type key_event = [ `Button_down of key | `Button_up of key ]
           
let address = 0xFF00

let p15 = 5
let p14 = 4

let a = 0
let b = 1
let select = 2
let start = 3
  
let right = 0
let left = 1
let up = 2
let down = 3

let write t b =
  t.joypad.control <- b lor inj 0b11000000

let button_index = function
  | `A -> a
  | `Right -> right
  | `B -> b
  | `Left -> left
  | `Select -> select
  | `Up -> up
  | `Start -> start
  | `Down -> down

let button_up t button =
  match button with
  | `A
  | `B
  | `Start
  | `Select ->
    t.joypad.buttons <- set_bit t.joypad.buttons (button_index button)
  | _ ->
    t.joypad.arrows <- set_bit t.joypad.arrows (button_index button)

let button_down t button =
  match button with
  | `A
  | `B
  | `Start
  | `Select ->
    t.joypad.buttons <- unset_bit t.joypad.buttons (button_index button)
  | _ ->
    t.joypad.arrows <- unset_bit t.joypad.arrows (button_index button)

let get t =
  let open Uint8 in
  let high = t.joypad.control in
  let low =
    let input = ref zero in
    if is_bit_set t.joypad.control p14 then
      input := !input lor t.joypad.buttons;
    if is_bit_set t.joypad.control p15 then
      input := !input lor t.joypad.arrows;
    !input
  in
  high lor low
