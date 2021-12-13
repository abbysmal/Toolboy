module J = Joypad

external ( <= ) : int -> int -> bool = "%lessequal"
external ( >= ) : int -> int -> bool = "%greaterequal"

let is_between' v (a, b) = v >= a && v <= b [@@inline]

exception Unimplemented of string

let echo1          = 0xE000, 0xEFFF
let echo2          = 0xF000, 0xFDFF
let unused         = 0xFEA0, 0xFEFF
let cartridge_ram  = 0xA000, 0xBFFF

let get (s : Machine.t) a =
  try
  match a with
  | 0xFF04 -> s.timers.div
  | 0xFF05 -> s.timers.tima
  | 0xFF06 -> s.timers.tma
  | 0xFF07 -> s.timers.tac
  | 0xFF00 -> Joypad.get s
  | 0xFF40 -> s.gpu.control_register
  | 0xFF41 -> s.gpu.lcd_register
  | 0xFF42 -> s.gpu.scr_y
  | 0xFF43 -> s.gpu.scr_x
  | 0xFF44 -> s.gpu.ly
  | 0xFF45 -> s.gpu.lyc
  | 0xFF46 -> s.gpu.dma_transfer
  | 0xFF47 -> s.gpu.bg_pal
  | 0xFF48 -> s.gpu.obj_pal0
  | 0xFF49 -> s.gpu.obj_pal1
  | 0xFF4B -> s.gpu.win_x
  | 0xFF4A -> s.gpu.win_y
  | a when is_between' a (0xFF10, 0xFF14) -> Square.get s.sc1 a
  | a when is_between' a (0xFF15, 0xFF19) -> Square.get s.sc2 a
  | a when is_between' a (0xFF1A, 0xFF1E) -> Wave.get s.sc3 a
  | a when is_between' a (0xff1F, 0xff23) -> Noise.get s.sc4 a
  | a when is_between' a (0xFF24, 0xFF26) -> Spu.get s a
  | a when is_between' a Machine.Audio.wave_table_range -> Wave.get s.sc3 a
  | a when (is_between' a Machine.Bios.range) &&
           (s.cpu.in_bios = true) -> s.bios.get a
  | a when is_between' a Machine.Cartridge.range || (is_between' a cartridge_ram)-> Cartridge.get s.cartridge a
  | a when is_between' a Machine.Vram.range -> s.vram.get a
  | a when is_between' a Machine.Wram0.range -> s.wram0.get a
  | a when is_between' a Machine.Wram1.range -> s.wram1.get a
  | a when is_between' a echo1 -> s.wram0.get (a - 0x2000)
  | a when is_between' a echo2 -> s.wram1.get (a - 0x2000)
  | a when is_between' a unused -> Uint8.inj 0xFF
  | a when is_between' a Machine.Hram.range -> s.hram.get a
  | a when is_between' a Machine.Oam.range -> s.oam.get a
  | a when is_between' a Machine.Io_ports.range -> s.io_ports.get a
  | _ -> raise (Unimplemented (Printf.sprintf "0x%04X" a))
  with
  | _ -> failwith (Printf.sprintf "omg at %04X\n" a)

let get_u16 s addr =
  let n = get s addr in
  let nn = get s (succ addr) in
  (Uint8.proj nn lsl 8)
  lor
  (Uint8.proj n)

let put (s : Machine.t) a v =
  try
  match a with
  | 0xFF40 -> s.gpu.control_register <- v
  | 0xFF41 -> s.gpu.lcd_register <- v
  | 0xFF42 -> s.gpu.scr_y <- v
  | 0xFF43 -> s.gpu.scr_x <- v
  | 0xFF44 -> s.gpu.ly <- v
  | 0xFF45 -> s.gpu.lyc <- v
  | 0xFF4B -> s.gpu.win_x <- v
  | 0xFF4A -> s.gpu.win_y <- v
  | 0xFF47 -> s.gpu.bg_pal <- v
  | 0xFF48 -> s.gpu.obj_pal0 <- v
  | 0xFF49 -> s.gpu.obj_pal1 <- v
  | 0xFF46 -> begin
      s.gpu.dma_transfer <- v;
      s.gpu.dma_transfer_request <- true
  end
  | 0xFF02 -> begin
    if v = Uint8.inj 0x81 then
      let serial = get s 0xFF01 in
      s.serial <- Some serial
    else
      ()
  end
  | 0xFF00 -> Joypad.write s v
  | 0xFF04 -> s.timers.div <- Uint8.zero
  | 0xFF05 -> s.timers.tima <- v
  | 0xFF06 -> s.timers.tma <- v
  | a when is_between' a (0xff10, 0xff14) -> Square.set s.sc1 v a
  | a when is_between' a (0xff15, 0xff19) -> Square.set s.sc2 v a
  | a when is_between' a (0xff1A, 0xff1E) -> Wave.set s.sc3 v a
  | a when is_between' a (0xff1F, 0xff23) -> Noise.set s.sc4 v a
  | a when is_between' a (0xFF24, 0xFF26) -> Spu.set s a v
  | a when (is_between' a Machine.Bios.range) &&
           (s.cpu.in_bios = true) -> assert false
  | a when is_between' a Machine.Audio.wave_table_range -> Wave.set s.sc3 v a
  | a when is_between' a Machine.Cartridge.range ||
           is_between' a cartridge_ram -> Cartridge.put s.cartridge a v
  | a when is_between' a Machine.Vram.range -> s.vram.set a v
  | a when is_between' a Machine.Wram0.range -> s.wram0.set a v
  | a when is_between' a Machine.Wram1.range -> s.wram1.set a v
  | a when is_between' a echo1 -> ()
  | a when is_between' a echo2 -> ()
  | a when is_between' a unused -> ()
  | a when is_between' a Machine.Hram.range -> s.hram.set a v
  | a when is_between' a Machine.Oam.range -> s.oam.set a v
  | a when is_between' a Machine.Io_ports.range -> s.io_ports.set a v
  | _ -> raise (Unimplemented (Printf.sprintf "0x%04X" a))
  with
  | _ -> failwith (Printf.sprintf "omg at %04X\n" a)

let put_u16 s addr v =
  put s (addr + 1) (v lsr 8 |> Uint8.inj);
  put s addr (v land 0x00FF |> Uint8.inj)
