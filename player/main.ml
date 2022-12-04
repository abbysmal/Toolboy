module Gb = Lesserboy
module U8 = Vgm.Uint8
open Tsdl
open Result

let gb_cycles = 4194304
let sample_rate = 44100
let per_sample = gb_cycles / sample_rate

let create machine =
  let audio_setup () =
    let desired_audiospec =
      { Sdl.as_freq = machine.Gb.Machine.config.sample_rate;
        as_format = Sdl.Audio.s16_sys;
        Sdl.as_channels = 2;
        Sdl.as_samples = machine.config.sample_size;
        Sdl.as_silence = 0;
        Sdl.as_size = 0l;
        Sdl.as_callback = None; }
    in
    match Sdl.open_audio_device None false desired_audiospec 0 with
    | Error _ -> Sdl.log "Can't open audio device"; exit 1
    | Ok (device_id, _) -> device_id
  in
  match Sdl.init Sdl.Init.audio with
  | Error (`Msg _e) -> assert false
  | Ok () ->
     let device_id = audio_setup () in
     let () = Sdl.pause_audio_device device_id false in
     device_id

let process machine l =
  let device_id = create machine in

  (* we make sure we do turn on the sound *)
  machine.apu.enabled <- true;
  machine.apu.channelenable <- (Gb.Uint8.inj 0xFF);
  machine.apu.frameseq_counter <- 0;
  machine.apu.frameseq <- 0;
  machine.sc1.step <- 0;

  let rec aux l cycles =
    if machine.apu.need_queue then (
      while Sdl.get_queued_audio_size device_id > machine.config.sample_size * 4 do
        Sdl.delay Int32.one
      done;
      match Sdl.queue_audio device_id machine.apu.buffer with
      | Error _ -> assert false
      | Ok _ -> machine.apu.need_queue <- false
    );

    match cycles with
    | 0 -> begin
      match l with
      | (Vgm.Parser.Wait n)::xs ->
         aux xs (n * per_sample)
      | Write (n, r)::xs ->
         Gb.Mmu.put machine (n + 0xFF10) (Char.unsafe_chr r); aux xs 1
      | Osef::xs ->
         aux xs 0
      | [] -> ()
    end
    | cycles ->
       (Gb.Spu.step machine 1); aux l (cycles - 1)
  in
  aux l 0

let load_file path =
  let open Rresult.R.Infix in
  Bos.OS.File.read path
  >>= fun content -> Ok content

(* https://github.com/mirage/decompress/blob/master/lib/gz.mli *)
let inflate_string str =
  let i = De.bigstring_create De.io_buffer_size in
  let o = De.bigstring_create De.io_buffer_size in
  let r = Buffer.create 0x1000 in
  let p = ref 0 in
  let refill buf =
    let len = min (String.length str - !p) De.io_buffer_size in
    Bigstringaf.blit_from_string str ~src_off:!p buf ~dst_off:0 ~len ;
    p := !p + len ; len in
  let flush buf len =
    let str = Bigstringaf.substring buf ~off:0 ~len in
    Buffer.add_string r str in
  match Gz.Higher.uncompress ~refill ~flush i o with
  | Ok _ ->
     let s = Buffer.contents r in
     let bs = Bigstringaf.of_string ~off:0 ~len:(String.length s) s in
     Ok bs
  | Error _ as err -> err

let main () =
  let open Rresult.R.Infix in

  let cfg = {
    Gb.Machine.sample_rate = 44100;
    sample_size = 4096;
    bios = None;
    rom = None;
    sav = None;
  } in
  let machine = Gb.Machine.make cfg in

  (if (Array.length Sys.argv) != 2 then
    Error (`Msg "command is ./main.exe vgm/vgz file")
  else Ok Sys.argv.(1))
  >>= fun in_file ->
  Fpath.of_string in_file >>= fun filename ->
  load_file filename >>= fun content ->
  (match Fpath.get_ext filename with
  | ".vgm" -> Ok (Bigstringaf.of_string ~off:0 ~len:(String.length content) content)
  | ".vgz" -> inflate_string content
  | _ -> Error (`Msg "unsupported file type: need .vgm or .vgz"))
  >>= fun content ->
  let parse =  Angstrom.parse_bigstring
                 ~consume:Angstrom.Consume.All
                 Vgm.Parser.parse_header content
  in
  match parse with
  | Error err -> Error (`Msg err)
  | Ok l -> process machine l; Ok ()

let () =
  match main () with
  | Error (`Msg err) -> print_endline err
  | Ok () -> ()
