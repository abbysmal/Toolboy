open Machine
open Uint8

let put_pixel m ~x ~y ~color =
  try
    (if y >= 144 || x >= 160 then
       ()
     else
       m.gpu.framebuffer.(x).(y) <- color)
  with
  | _ -> failwith "mdr"

module Control = struct

  let lcd_on = 7
  let win_tile_map_display = 6
  let window_display_on = 5
  let bg_window_tile_data_select = 4
  let bg_tile_map_display_select = 3
  let obj_size = 2
  let obj_on = 1
  let display_on = 0

  let is_flag_set f m = is_bit_set m.gpu.control_register f

  let set_flag f m =
    m.gpu.control_register <- set_bit m.gpu.control_register f

  let get_window_tile_map_display m =
    match is_bit_set m.gpu.control_register win_tile_map_display with
    | false -> 0x9800, 0x9BFF
    | true -> 0x9C00, 0x9FFF

  let bg_tile_data_select m =
    match is_bit_set m.gpu.control_register bg_window_tile_data_select with
    | false -> 0x9000
    | true -> 0x8000

  let bg_layout_select m =
    match  is_bit_set m.gpu.control_register bg_tile_map_display_select with
    | false -> 0x9800
    | true -> 0x9C00

  let get_obj_size m =
    match is_bit_set m.gpu.control_register obj_size with
    | false -> `Eight
    | true -> `Sixteen

end

module Lcd = struct

  let lyc_ly_irq = 6
  let oam_irq  = 5
  let vblank_irq = 4
  let hblank_irq = 3
  let lyc_ly_flag = 2

  let is_flag_set m f = is_bit_set m.gpu.lcd_register f
  let set_flag m f = m.gpu.lcd_register <- set_bit m.gpu.lcd_register f
  let unset_flag m f = m.gpu.lcd_register <- unset_bit m.gpu.lcd_register f

  let get_mode_flag m =
    let mode = Uint8.(m.gpu.lcd_register land (inj 0b11)) in
    match Uint8.proj mode with
    | 0b00 -> `Hblank
    | 0b01 -> `Vblank
    | 0b10 -> `OAM_ram
    | 0b11 -> `Lcd_transfer
    | _ -> assert false

  let set_mode_flag m mode =
    let open Uint8 in
    let lcd_register =
      match mode with
      | `Hblank -> unset_bit' 0 m.gpu.lcd_register |> unset_bit' 1
      | `Vblank -> unset_bit' 1 m.gpu.lcd_register |> set_bit' 0
      | `OAM_ram -> set_bit' 1 m.gpu.lcd_register |> unset_bit' 0
      | `Lcd_transfer -> set_bit' 0 m.gpu.lcd_register |> set_bit' 1
    in
    m.gpu.lcd_register <- lcd_register

end

let map_color c =
  match Char.code c with
  | 0 -> `White
  | 1 -> `Light_gray
  | 2 -> `Dark_gray
  | 3 -> `Black
  | _ -> assert false

let get_palette palette =
  let color_4 = get_n_bits palette (Uint8.inj 2) |> map_color in
  let color_3 = get_n_bits (palette lsr Uint8.inj 2) (Uint8.inj 2) |> map_color in
  let color_2 = get_n_bits (palette lsr Uint8.inj 4) (Uint8.inj 2) |> map_color in
  let color_1 = get_n_bits (palette lsr Uint8.inj 6) (Uint8.inj 2) |> map_color in
  color_1, color_2, color_3, color_4
