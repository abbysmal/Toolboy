module Cpu : sig

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

  val make : unit -> t

end

module Vram : sig
  type t = Addressable.t
  val range : (int * int)
end

module Wram0 : sig
  type t = Addressable.t
  val range : (int * int)
end

module Wram1 : sig
  type t = Addressable.t
  val range : (int * int)
end

module Oam : sig
  type t = Addressable.t
  val range : (int * int)
end

module Io_ports : sig
  type t = Addressable.t
  val range : (int * int)
end

module Hram : sig
  type t = Addressable.t
  val range : (int * int)
end

module Bios : sig
  type t = Addressable.t
  val range : (int * int)
end

module Cartridge : sig
  type t = Cartridge.t
  val range : (int * int)
end

module Timers : sig

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

  val make_rtc : unit -> rtc

end

module Gpu : sig

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

end

module Joypad : sig

type t = {
  mutable control : Uint8.t;
  mutable arrows : Uint8.t;
  mutable buttons : Uint8.t;
}

end

module Square : sig

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
    mutable current_midi : Uint8.t;
  }

  val make : [`SC1 | `SC2 ] -> t

end

module Wave : sig

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

end

module Noise : sig

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

end

module Audio : sig

  val wave_table_range : (int * int)

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

val make : config -> t
