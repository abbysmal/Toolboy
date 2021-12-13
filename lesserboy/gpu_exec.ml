let clear_bg (m : Machine.t) =
  let gpu = m.gpu in
  for pixel = 0 to 159 do
    Gpu.put_pixel ~x:pixel ~y:(Uint8.proj gpu.ly) ~color:`White m
  done

let render_line_bg (m : Machine.t) =
  let open Gpu in
  let gpu = m.gpu in
  match Gpu.Control.(is_flag_set display_on m) with
  | false -> clear_bg m
  | true  ->

  let c1, c2, c3, c4 =
    let open Uint8 in
      (gpu.bg_pal) land (inj 0b11),
      (gpu.bg_pal lsr Uint8.inj 2) land (inj 0b11),
      (gpu.bg_pal lsr Uint8.inj 4) land (inj 0b11),
      (gpu.bg_pal lsr Uint8.inj 6) land (inj 0b11)
  in
  let tile_data = Control.bg_tile_data_select m in
  let tile_number_map = Control.bg_layout_select m in
  let ly = Uint8.proj gpu.ly in
  let scr_y = Uint8.proj gpu.scr_y in
  let scr_x = Uint8.proj gpu.scr_x in

  let tile_y = ((scr_y + ly) / 8) mod 32 in
  let tile_y_off = (scr_y + ly) mod 8 in

  for pixel=0 to 159 do

    let tile_x = (((scr_x + pixel) / 8) mod 32) land Uint16.max_int in
    let tile_num = Mmu.get m ((tile_number_map + (tile_y * 32) + tile_x) land Uint16.max_int) in

    let tile_data_ptr =
      if tile_data != 0x9000 then
        (tile_data + (Uint8.proj tile_num) * 0x10) land Uint16.max_int
      else
        (tile_data + (Uint8.to_signed tile_num) * 0x10) land Uint16.max_int
    in
    let tile_data_ptr = tile_data_ptr + (tile_y_off * 2) land Uint16.max_int in
    let b1 = Mmu.get m (tile_data_ptr) in
    let b2 = Mmu.get m (tile_data_ptr + 1) in
    let col_bit = (pixel + scr_x) mod 8 in
    let col_bit = col_bit - 7 in
    let col_bit = col_bit * (-1) in
    let col_num = if Uint8.is_bit_set b2 col_bit then Uint8.one else Uint8.zero in
    let col_num = Uint8.(col_num lsl one) in
    let col_num = Uint8.(col_num lor (if Uint8.is_bit_set b1 col_bit then one else zero)) in
    let color = match Uint8.proj col_num with
      | 0x0 -> c1
      | 0x1 -> c2
      | 0x2 -> c3
      | 0x3 -> c4
      | _ -> assert false
    in
    let color = Gpu.map_color color in
    Gpu.put_pixel ~x:pixel ~y:(Uint8.proj gpu.ly) ~color m

  done

let render_line_windows (m : Machine.t) =
  let open Uint8 in
  let open Gpu in

  let gpu = m.gpu in
  match Control.(is_flag_set window_display_on m) with
  | false -> ()
  | true  ->

    let win_y = proj gpu.ly - proj gpu.win_y in
  if win_y < 0 then
    ()
  else begin
    let c1, c2, c3, c4 =
      let open Uint8 in
      (gpu.bg_pal) land (inj 0b11),
      (gpu.bg_pal lsr Uint8.inj 2) land (inj 0b11),
      (gpu.bg_pal lsr Uint8.inj 4) land (inj 0b11),
      (gpu.bg_pal lsr Uint8.inj 6) land (inj 0b11)
    in
    let tile_number_map, _ = Control.get_window_tile_map_display m in
    let tile_data = Control.bg_tile_data_select m in
    let tile_y = div (inj win_y) (inj 8) in
    let tile_y_offset = win_y mod 8 |> inj in
    let win_x = (proj gpu.win_x) - 7 in
    for x = 0 to 159 do
      if x < win_x then begin
        ()
      end
      else begin
        let tile_x = Infix.((inj x - inj win_x) / inj 8) in
        let tile_number =
          Mmu.get m (tile_number_map + (proj tile_y * 32) + proj tile_x)
        in
        let tile_data_ptr =
          if tile_data != 0x9000 then
            (tile_data + (Uint8.proj tile_number) * 0x10)
          else
            (tile_data + (Uint8.to_signed tile_number) * 0x10)
        in
        let tile_data_ptr = tile_data_ptr + (proj tile_y_offset * 2) in
        let b1 = Mmu.get m (tile_data_ptr) in
        let b2 = Mmu.get m (tile_data_ptr + 1) in
        let bit = 7 - x mod 8 in
        let pLo = if is_bit_set b1 bit then 0x01 else 0x00 in
        let pHo = if is_bit_set b2 bit then 0x02 else 0x00 in
        let col_num = pLo + pHo in
        let color = match col_num with
          | 0x0 -> c1
          | 0x1 -> c2
          | 0x2 -> c3
          | 0x3 -> c4
          | _ -> assert false
        in
        let color = Gpu.map_color color in
        Gpu.put_pixel ~x ~y:(Uint8.proj gpu.ly) ~color m
      end
    done
  end

let render_line_obj (m : Machine.t) =
  let open Gpu in
  let open Uint8 in
  let gpu = m.gpu in
  let get_n_oam t i = try Mmu.get t (0xFE00 + i) with _ -> failwith "get_n_oam" in

  match Control.(is_flag_set obj_on m) with
  | false -> ()
  | true  ->
    let rec loop i =
      if i >= 0 then begin
      let obj_y = get_n_oam m i in
      let obj_size = Control.get_obj_size m in
      let height = match obj_size with `Eight -> 8 | `Sixteen -> 16 in

      let y = sub obj_y (inj 16) in
      if not ((y <= gpu.ly) && (add y (inj height)) > gpu.ly) then
        if (proj gpu.ly = 0x0 && (proj obj_y = 0x0C)) then
          Printf.printf "i: %02X\n" (proj obj_y);
      (* sprite is on current scanline *)
      if (y <= gpu.ly) && (add y (inj height)) > gpu.ly then begin
        if (proj gpu.ly = 0x0 && (proj obj_y != 0xA0)) then
          Printf.printf "i: %02X\n" (proj obj_y);
        let obj_x = get_n_oam m (i + 1) in
        let sprite_tile_number = get_n_oam m (i + 2) in
        let sprite_flags = get_n_oam m (i + 3) in
        let sprite_tile_number =
          match obj_size with
          | `Sixteen -> sprite_tile_number land (inj 0xFE)
          | _ -> sprite_tile_number
        in
        let palette_number =
          if is_bit_set sprite_flags 4 then 0x01 else 0x00
        in
        let c1, c2, c3, c4 =
          let open Uint8 in
          match palette_number with
          | 0x00 ->
            inj 0x00,
            (gpu.obj_pal0 lsr Uint8.inj 2) land (inj 0b11),
            (gpu.obj_pal0 lsr Uint8.inj 4) land (inj 0b11),
            (gpu.obj_pal0 lsr Uint8.inj 6) land (inj 0b11)
          | _ ->
            inj 0x00,
            (gpu.obj_pal1 lsr Uint8.inj 2) land (inj 0b11),
            (gpu.obj_pal1 lsr Uint8.inj 4) land (inj 0b11),
            (gpu.obj_pal1 lsr Uint8.inj 6) land (inj 0b11)
        in
        let x = sub obj_x (inj 8) in

        let sprite_size_in_bytes = inj 16 in
        let tile_data = 0x0000 in
        let tile_pointer = (tile_data + (proj sprite_tile_number * proj sprite_size_in_bytes)) in
        let tile_y_offset =
          if is_bit_set sprite_flags 6 then
            Infix.(((inj height) - one) - (gpu.ly - y))
          else
            Infix.(gpu.ly - y)
        in
        let tile_pointer = (tile_pointer + (proj tile_y_offset * (2))) in
        let vram = 0x8000 in
        let low = Mmu.get m (vram + (tile_pointer)) in
        let high = Mmu.get m (vram + (tile_pointer) + 1) in

        for index_x = 0 to 7 do
          if (proj gpu.ly = 0x0 && (proj obj_y != 0xA0)) then
            Printf.printf "ii: %02X\n" (proj obj_y);
          let pixel_x = (proj x) + index_x in
          if (proj gpu.ly = 0x0 && (proj obj_y != 0xA0)) then
            Printf.printf "ii: pixel_x :%d index_x %d x %d\n" pixel_x index_x (proj x);
          if (pixel_x >= 0) && (pixel_x <= 160) then begin
            if (proj gpu.ly = 0x0 && (proj obj_y != 0xA0)) then
              Printf.printf "iii: %02X\n" (proj obj_y);
            let bit = if is_bit_set sprite_flags 5 then index_x else 7 - index_x in
            let pixel_val = if is_bit_set high bit then zero lor (inj 0x02) else zero in
            let pixel_val = if is_bit_set low bit then pixel_val lor (inj 0x01) else pixel_val in
            let color =
              match proj pixel_val with
              | 0x00 -> c1
              | 0x01 -> c2
              | 0x02 -> c3
              | 0x03 -> c4
              | _ -> assert false
            in
            let color' = Gpu.map_color color in
            let index = pixel_x in
            if (proj gpu.ly = 0x0 && (proj obj_y != 0xA0)) then
              Printf.printf "iii: %02X %d %d\n" (proj obj_y) (proj color) (proj pixel_val);
            if index < 160 && pixel_val != zero then begin
              if (proj gpu.ly = 0x0 && (proj obj_y != 0xA0)) then
                Printf.printf "iiii: %02X\n" (proj obj_y);
            if not (is_bit_set sprite_flags 7) || gpu.framebuffer.(index).(Uint8.proj gpu.ly) = `White then begin
              if (proj gpu.ly = 0x0 && (proj obj_y != 0xA0)) then
                Printf.printf "iiiii: %04X obj_y: %02X h: %d index: %d color: %d\n" (0xFE00 + i) (proj obj_y) height index (proj color);
              if (proj gpu.ly = 0x0 && (proj obj_y != 0xA0)) then (
                Printf.printf "iiiiii: %02X\n" (proj obj_y);
              print_endline "gotcha");
              try
                (
                  Gpu.put_pixel ~x:(index) ~y:(Uint8.proj gpu.ly) ~color:color' m
                )
              with
              | _ -> failwith (Printf.sprintf "dang %02X %02X" index (proj gpu.ly))
            end
            else
              ()
          end
          else ();
          end else ();
        done
      end;
      loop (i - 4)
    end
  in
  loop 156

let oam_ram_cycles = 80
let lcd_transfer_cycles = 172
let hblank_cycles = 204
let vblank_cycles = 456

let dma_transfer (t : Machine.t) addr =
  t.gpu.dma_transfer_request <- false;
  t.gpu.dma_clock <- 752;
  let src = (Uint8.proj addr) * 0x100 in
  for offset = 0x00 to 0x9F do
    let source = src lor offset in
    Mmu.put t (0xFE00 + offset) (Mmu.get t source)
  done

let trigger_interrupt t int =
  let isf = Mmu.get t 0xFF0F in
  Mmu.put t 0xFF0F (Uint8.set_bit isf int)

let step (t : Machine.t) cycles =
  let open Gpu in
  let gpu = t.gpu in
  let current_mode = Lcd.get_mode_flag t in
  if gpu.dma_transfer_request then
    dma_transfer t gpu.dma_transfer;
  if gpu.dma_clock > 0 then begin
    gpu.dma_clock <- gpu.dma_clock - cycles;
  end;
  let clock = gpu.clock + cycles in
  begin match current_mode with
  | `OAM_ram when clock >= oam_ram_cycles -> begin
      let clock = clock - oam_ram_cycles in
      t.gpu.clock <- clock;
      Lcd.set_mode_flag t `Lcd_transfer
    end
    | `Lcd_transfer when clock >= lcd_transfer_cycles ->
      let clock = clock - lcd_transfer_cycles in
      render_line_bg t;
      render_line_windows t;
      render_line_obj t;
      t.gpu.clock <- clock;
      if Lcd.is_flag_set t Lcd.hblank_irq then
        trigger_interrupt t Interrupts.lcd_stats;
      Lcd.set_mode_flag t `Hblank
    | `Hblank when clock >= hblank_cycles ->
      let clock = clock - hblank_cycles in
      let ly = Uint8.succ gpu.ly in
      if (Uint8.proj ly == 144) then begin
        gpu.redraw <- true;
        trigger_interrupt t Interrupts.vblank;
        if Lcd.is_flag_set t Lcd.vblank_irq then
          trigger_interrupt t Interrupts.lcd_stats;
        t.gpu.clock <- clock;
        t.gpu.ly <- ly;
        Lcd.set_mode_flag t `Vblank
      end
      else begin
        t.gpu.clock <- clock;
        t.gpu.ly <- ly;
        Lcd.set_mode_flag t `OAM_ram;
        if Lcd.is_flag_set t Lcd.oam_irq then
          trigger_interrupt t Interrupts.lcd_stats
      end
    | `Vblank when clock >= vblank_cycles ->
      let clock = clock - vblank_cycles in
      let ly = Uint8.succ gpu.ly in
      if Uint8.proj ly = 154 then begin
        let ly = Uint8.zero in
        if Lcd.is_flag_set t Lcd.oam_irq then
          trigger_interrupt t Interrupts.lcd_stats;
        t.gpu.clock <- clock;
        t.gpu.ly <- ly;
        Lcd.set_mode_flag t `OAM_ram
      end
      else begin
        t.gpu.clock <- clock;
        t.gpu.ly <- ly;
      end
    | _ -> t.gpu.clock <- clock
  end;
  if t.gpu.ly = t.gpu.lyc then begin
      Lcd.set_flag t Lcd.lyc_ly_flag;
      if Lcd.is_flag_set t Lcd.lyc_ly_irq then
        trigger_interrupt t Interrupts.lcd_stats
    end else
      Lcd.unset_flag t Lcd.lyc_ly_flag
