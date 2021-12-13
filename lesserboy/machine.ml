module Cpu = struct

  type t = {
    mutable sp : int;
    mutable pc : int;
    mutable ime : bool;
    mutable t : int;
    mutable m : int;
    mutable a : Uint8.t;
    mutable b : Uint8.t;
    mutable c : Uint8.t;
    mutable d : Uint8.t;
    mutable e : Uint8.t;
    mutable h : Uint8.t;
    mutable l : Uint8.t;
    mutable z : bool;
    mutable n : bool;
    mutable hc : bool;
    mutable ca : bool;
    mutable halted : bool;
    mutable in_bios : bool;
  }

  let make () = {
    sp = 0;
    pc = 0;
    ime = false;
    t = 0;
    m = 0;
    a = Uint8.zero;
    b = Uint8.zero;
    c = Uint8.zero;
    d = Uint8.zero;
    e = Uint8.zero;
    h = Uint8.zero;
    l = Uint8.zero;
    z = false;
    n = false;
    hc = false;
    ca = false;
    halted = false;
    in_bios = true;
  }

end

module Vram = struct

  type t = Addressable.t
  let range = 0x8000, 0x9FFF
  let make () = Addressable.make ~name:"vram" range

end

module Wram0 = struct

  type t = Addressable.t
  let range = 0xC000, 0xCFFF
  let make () = Addressable.make ~name:"wram0" range

end

module Wram1 = struct

  type t = Addressable.t
  let range = 0xD000, 0xDFFF
  let make () = Addressable.make ~name:"wram1" range

end

module Oam = struct

  type t = Addressable.t
  let range = 0xFE00, 0xFE9F
  let make () = Addressable.make ~name:"oam" range

end

module Io_ports = struct

  type t = Addressable.t
  let range = 0xFF00, 0xFF7F
  let make () = Addressable.make ~name:"io_ports" range

end

module Hram = struct

  type t = Addressable.t
  let range = 0xFF80, 0xFFFF
  let make () = Addressable.make ~name:"hram" range

end

module Bios = struct

  type t = Addressable.t
  let range = 0x0000, 0x00FE
  let make ?bios () =
    match bios with
    | Some buffer ->
      Addressable.make ~buffer ~name:"bios" range
    | None ->
      Addressable.make ~name:"bios" range

end

module Cartridge = struct

  type t = Cartridge.t
  let range = 0x0000, 0x7FFF
  let make ?rom ?sav () = Cartridge.make ?rom ?sav ()

end

module Timers = struct

  type rtc = {
    mutable high : Uint8.t;
    mutable low : Uint8.t;
    mutable seconds : Uint8.t;
    mutable minutes : Uint8.t;
    mutable hours : Uint8.t;
    mutable days : Uint8.t;
  }

  type t = {
    mutable main : int;
    mutable sub : int;
    mutable div_c : int;
    mutable tima : Uint8.t;
    mutable div : Uint8.t;
    mutable tma : Uint8.t;
    mutable tac : Uint8.t;
    mutable last_rtc : int;
    mutable rtc_real : rtc option;
    mutable rtc_latched : rtc option;
  }

  let make_rtc () = {
    high = Uint8.zero;
    low = Uint8.zero;
    seconds = Uint8.zero;
    minutes = Uint8.zero;
    hours = Uint8.zero;
    days = Uint8.zero;
  }

  let make () = {
    main = 0;
    sub = 0;
    div_c = 0;
    div = Uint8.zero;
    tima = Uint8.zero;
    tma = Uint8.zero;
    tac = Uint8.zero;
    rtc_real = None;
    rtc_latched = None;
    last_rtc = 0;
  }

end

module Gpu = struct

  type color = [ `White | `Black | `Light_gray | `Dark_gray ]
  type tile = color list

  type t = {
    mutable clock : int;
    mutable lcd_register : Uint8.t;
    mutable control_register : Uint8.t;
    mutable scr_y : Uint8.t;
    mutable scr_x : Uint8.t;
    mutable ly : Uint8.t;
    mutable lyc : Uint8.t;
    mutable win_x : Uint8.t;
    mutable win_y : Uint8.t;
    mutable bg_pal  : Uint8.t;
    mutable obj_pal0 : Uint8.t;
    mutable obj_pal1 : Uint8.t;
    mutable dma_transfer : Uint8.t;
    mutable dma_transfer_request : bool;
    mutable retrace_ly : int;
    mutable redraw : bool;
    mutable dma_clock : int;
    framebuffer : color array array;
  }

  let make () = {
    clock = 0;
    dma_clock = 0;
    dma_transfer_request = false;
    redraw = false;
    lcd_register = Uint8.inj 0x80;
    control_register = Uint8.zero;
    ly = Uint8.zero;
    lyc = Uint8.zero;
    scr_y = Uint8.zero;
    scr_x = Uint8.zero;
    win_x = Uint8.zero;
    win_y = Uint8.zero;
    bg_pal = Uint8.zero;
    obj_pal0 = Uint8.zero;
    obj_pal1 = Uint8.zero;
    dma_transfer = Uint8.zero;
    framebuffer = Array.make_matrix 160 144 `Black;
    retrace_ly = 0;
  }

end

module Joypad = struct

type t = {
  mutable control : Uint8.t;
  mutable arrows : Uint8.t;
  mutable buttons : Uint8.t;
}

let make () = {
  control = Uint8.inj 0b11000000;
  arrows = Uint8.inj 0b1111;
  buttons =  Uint8.inj 0b1111;
}

end

module Square = struct

  type t = {
    channel : [ `SC1 | `SC2 ];
    mutable duty : Uint8.t;
    mutable length_load : Uint8.t;
    mutable env_add_mode : bool;
    mutable period : Uint8.t;
    mutable frequency_lsb : Uint8.t;
    mutable trigger : Uint8.t;
    mutable frequency_msb : Uint8.t;
    mutable enabled : bool;
    mutable frequency : int;
    mutable sweep_timer : Uint8.t;
    mutable sweep_enable : bool;
    mutable sweep_freq : int;
    mutable sweep_period : Uint8.t;
    mutable sweep_negate : bool;
    mutable sweep_shift : Uint8.t;
    mutable sweep_direction : bool;
    mutable starting_volume : Uint8.t;
    mutable volume : int;
    mutable envelope_timer : Uint8.t;
    mutable envelope_period : Uint8.t;
    mutable envelope_direction : bool;
    mutable length_counter : int;
    mutable length_enabled : bool;
    mutable timer : int;
    mutable step : int;
    mutable dac_enabled : bool;
  }

  let make mode : t = {
    channel = mode;
    sweep_period = Uint8.zero;
    sweep_negate = false;
    sweep_shift = Uint8.zero;
    duty = Uint8.zero;
    length_load = Uint8.zero;
    starting_volume = Uint8.zero;
    env_add_mode = false;
    period = Uint8.zero;
    frequency_lsb = Uint8.zero;
    trigger = Uint8.zero;
    length_enabled = false;
    frequency_msb = Uint8.zero;
    timer = 0;
    length_counter = 0;
    volume = 0;
    step = 0;
    enabled = false;
    dac_enabled = false;
    envelope_period = Uint8.zero;
    sweep_enable = false;
    sweep_timer = Uint8.zero;
    frequency = 0;
    sweep_freq = 0;
    sweep_direction = false;
    envelope_timer = Uint8.zero;
    envelope_direction = false;
  }

end

module Wave = struct

  type t = {
    mutable enabled : bool;
    mutable dac_enabled : bool;
    mutable length_load : Uint8.t;
    mutable volume_code : Uint8.t;
    mutable frequency_lsb : Uint8.t;
    mutable frequency_msb : Uint8.t;
    mutable frequency : int;
    mutable trigger : bool;
    mutable length_enable : bool;
    mutable volume : int;
    mutable length_counter : int;
    mutable timer : int;
    mutable pos : int;
    mutable sample_buffer : Uint8.t;
    wave_table : Bytes.t;
  }

  let make () = {
    enabled = false;
    dac_enabled = true;
    length_load = Uint8.zero;
    volume_code = Uint8.zero;
    frequency_lsb = Uint8.zero;
    frequency_msb = Uint8.zero;
    frequency = 0;
    trigger = false;
    length_enable = false;
    volume = 0;
    length_counter = 0;
    timer = 0;
    pos = 0;
    sample_buffer = Uint8.zero;
    wave_table = Bytes.create 32;
  }

end

module Noise = struct

  type t = {
    mutable trigger : bool;
    mutable length_load : Uint8.t;
    mutable starting_volume : Uint8.t;
    mutable env_add_mode : bool;
    mutable period : Uint8.t;
    mutable enabled : bool;
    mutable envelope_timer : Uint8.t;
    mutable envelope_period : Uint8.t;
    mutable envelope_direction : bool;
    mutable length_counter : int;
    mutable length_enabled : bool;
    mutable timer : int;
    mutable clock_shift : Uint8.t;
    mutable width_mode : bool;
    mutable volume : int;
    mutable divisor_code : Uint8.t;
    mutable lsfr : int;
  }

  let make () : t = {
    length_load = Uint8.zero;
    starting_volume = Uint8.zero;
    env_add_mode = false;
    period = Uint8.zero;
    trigger = false;
    length_enabled = false;
    timer = 0;
    length_counter = 0;
    volume = 0;
    enabled = false;
    envelope_period = Uint8.zero;
    envelope_timer = Uint8.zero;
    envelope_direction = false;
    clock_shift = Uint8.zero;
    divisor_code = Uint8.zero;
    width_mode = false;
    lsfr = 0x7FFF;
  }

end

module Audio = struct

  let wave_table_range = 0xFF30, 0xFF3F

  type t = {
    mutable enabled : bool;
    mutable frameseq : int;
    mutable frameseq_counter : int;
    mutable downsample_count : int;
    mutable buffer_fill : int;
    mutable buffer : (int32, Bigarray.int32_elt, Bigarray.c_layout) Bigarray.Array1.t;
    mutable need_queue : bool;
    mutable vin_l : bool;
    mutable vin_r : bool;
    mutable vol_l : Uint8.t;
    mutable vol_r : Uint8.t;
    mutable channelenable : Uint8.t;
  }

  let make size = {
    enabled = false;
    frameseq = 0;
    frameseq_counter = 0;
    downsample_count = 0;
    vin_l = false;
    vin_r = false;
    vol_l = Uint8.zero;
    vol_r = Uint8.zero;
    buffer_fill = 0;
    need_queue = false;
    buffer = Bigarray.Array1.create Bigarray.int32 Bigarray.c_layout size;
    channelenable = Uint8.zero;
  }

end

type config = {
  bios : Bytes.t option;
  rom : Bytes.t option;
  sav : Bytes.t option;
  sample_size : int;
  sample_rate : int;
}

type t = {
  cpu : Cpu.t;
  timers : Timers.t;
  gpu : Gpu.t;
  vram : Vram.t;
  wram0 : Wram0.t;
  wram1 : Wram1.t;
  bios : Bios.t;
  io_ports : Io_ports.t;
  hram : Hram.t;
  oam : Oam.t;
  cartridge : Cartridge.t;
  joypad : Joypad.t;
  sc1 : Square.t;
  sc2 : Square.t;
  sc3 : Wave.t;
  sc4 : Noise.t;
  apu : Audio.t;
  config : config;
  mutable serial : Uint8.t option;
}

let make (config : config) =
  {
    config;
    cpu = Cpu.make ();
    gpu = Gpu.make ();
    timers = Timers.make ();
    vram = Vram.make ();
    wram0 = Wram0.make ();
    wram1 = Wram1.make ();
    bios = Bios.make ?bios:config.bios ();
    cartridge = Cartridge.make ?rom:config.rom ?sav:config.sav ();
    io_ports = Io_ports.make ();
    hram = Hram.make ();
    oam = Oam.make ();
    joypad = Joypad.make ();
    serial = None;
    sc1 = Square.make `SC1;
    sc2 = Square.make `SC2;
    sc3 = Wave.make ();
    sc4 = Noise.make ();
    apu = Audio.make config.sample_size;
  }
