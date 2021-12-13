(* res:
   https://emudev.de/gameboy-emulator/bleeding-ears-time-to-add-audio/
   https://www.reddit.com/r/EmuDev/comments/5gkwi5/gb_apu_sound_emulation/
   https://gbdev.gg8.se/wiki/index.php?title=Gameboy_sound_hardware
   https://github.com/AntonioND/giibiiadvance/blob/master/docs/other_docs/GBSOUND.txt
*)

let reset (m : Machine.t) =
  m.apu.enabled <- false;
  m.apu.buffer_fill <- 0;
  Bigarray.Array1.fill m.apu.buffer 0l;
  m.apu.vin_l <- false;
  m.apu.vin_r <- false;
  m.apu.vol_r <- Uint8.zero;
  m.apu.vol_l <- Uint8.zero;
  m.apu.channelenable <- Uint8.zero;
  m.apu.downsample_count <- 95;
  m.apu.frameseq_counter <- 8192;
  m.apu.frameseq <- 0;
  Square.reset m.sc1

let set (m : Machine.t) a v =
  match a with
  | 0xFF26 ->
    if m.apu.enabled && not (Uint8.is_bit_set v 7) then
      reset m
    else if not m.apu.enabled && (Uint8.is_bit_set v 7) then begin
      m.apu.enabled <- true;
      m.apu.frameseq_counter <- 0;
      m.apu.frameseq <- 0;
      m.sc1.step <- 0
    end
  | 0xFF24 ->
    if m.apu.enabled then begin
      m.apu.vin_l <- Uint8.is_bit_set v 7;
      m.apu.vol_l <- Uint8.((v lsr inj 4) land inj 0b111);
      m.apu.vin_r <- Uint8.is_bit_set v 3;
      m.apu.vol_r <- Uint8.(v land inj 0b111);
    end
  | 0xFF25 -> begin
      if m.apu.enabled then
        begin
          m.apu.channelenable <- v
        end
  end
  | _ -> assert false

let get (m : Machine.t) a =
  let open Uint8 in
  match a with
  | 0xFF24 ->
    (if m.apu.vin_l then inj 0b10000000 else zero)
    lor
    (m.apu.vol_l lsl inj 4)
    lor
    (if m.apu.vin_r then inj 0b100 else zero)
    lor
    m.apu.vol_r
  | 0xFF25 -> m.apu.channelenable
  | 0xFF26 ->
    (if m.apu.enabled then inj 0b10000000 else zero)
    lor
    inj 0b1110000
    lor
    (if m.sc1.enabled then one else zero)
  | _ -> assert false

let step (m : Machine.t) cycles =
  let rec aux cycles =
    if cycles <= 0 then
      ()
    else begin
      m.apu.frameseq_counter <- m.apu.frameseq_counter - 1;


      Square.step m.sc1;
      Square.step m.sc2;
      Wave.step m;
      Noise.step m.sc4;

      if m.apu.frameseq_counter <= 0 then (
        m.apu.frameseq_counter <- 8192;
        (match m.apu.frameseq with
         | 0 ->
           Square.length_tick m.sc1;
           Square.length_tick m.sc2;
           Noise.length_tick m.sc4;
           Wave.length_tick m.sc3
         | 2 ->
           Square.sweep_tick m.sc1;
           Noise.length_tick m.sc4;
           Wave.length_tick m.sc3;
           Square.length_tick m.sc1;
           Square.length_tick m.sc2
         | 4 ->
           Wave.length_tick m.sc3;
           Noise.length_tick m.sc4;
           Square.length_tick m.sc1;
           Square.length_tick m.sc2
         | 6 ->
           Wave.length_tick m.sc3;
           Noise.length_tick m.sc4;
           Square.sweep_tick m.sc1;
           Square.length_tick m.sc1;
           Square.length_tick m.sc2
         | 7 ->
           Square.env_tick m.sc1;
           Square.env_tick m.sc2;
           Noise.env_tick m.sc4

        | _ -> ());
        m.apu.frameseq <- m.apu.frameseq + 1;
        if m.apu.frameseq >= 8 then
          m.apu.frameseq <- 0
      );

      m.apu.downsample_count <- m.apu.downsample_count - 1;

      if m.apu.downsample_count <= 0 then (
        m.apu.downsample_count <- (4194304 / m.config.sample_rate);
        let sc1 = Square.sample m.sc1 in
        let sc2 = Square.sample m.sc2 in
        let sc3 = Wave.sample m.sc3 in
        let sc4 = Noise.sample m.sc4 in
        let l =
          (if Uint8.is_bit_set m.apu.channelenable 4 then sc1 else 0) |>
          (+) (if Uint8.is_bit_set m.apu.channelenable 5 then sc2 else 0) |>
          (+) (if Uint8.is_bit_set m.apu.channelenable 6 then sc3 else 0) |>
          (+) (if Uint8.is_bit_set m.apu.channelenable 7 then sc4 else 0)
        in
        let r =
          (if Uint8.is_bit_set m.apu.channelenable 0 then sc1 else 0) |>
          (+) (if Uint8.is_bit_set m.apu.channelenable 1 then sc2 else 0) |>
          (+) (if Uint8.is_bit_set m.apu.channelenable 2 then sc3 else 0) |>
          (+) (if Uint8.is_bit_set m.apu.channelenable 3 then sc4 else 0)
        in
        let l = (l * ((Uint8.proj m.apu.vol_l) * 8)) mod Uint16.max_int in
        let r = (r * ((Uint8.proj m.apu.vol_r) * 8)) mod Uint16.max_int in
        m.apu.buffer.{m.apu.buffer_fill} <- Int32.of_int ((l lsl 16) lor r);
        m.apu.buffer_fill <- m.apu.buffer_fill + 1;
      );

      if m.apu.buffer_fill >= m.config.sample_size then (
        m.apu.buffer_fill <- 0;
        m.apu.need_queue <- true;
      );

      aux (cycles - 1)

    end

  in

  aux (cycles)
