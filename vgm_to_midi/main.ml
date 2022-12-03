module Gb = Lesserboy
module U8 = Vgm.Uint8

open! Vgm.Imports
open Result.Syntax


let turn_off_everything device =
ignore @@
    Portmidi.write_output device
      [ Portmidi.Portmidi_event.create ~status:'\176' ~data1:'\123' ~data2:'\000' ~timestamp:0l ]

let note_on ~channel note volume =
  let status = 0b10010000 + (channel - 1) |> Char.chr in
  Portmidi.Portmidi_event.create ~status ~data1:note ~data2:volume ~timestamp:0l
let note_off ~channel note volume =
  let status = 0b10000000 + (channel - 1) |> Char.chr in
  Portmidi.Portmidi_event.create ~status ~data1:note ~data2:volume ~timestamp:0l
let gb_cycles = 4194304
let sample_rate = 44100
let per_sample = gb_cycles / sample_rate

type last_on = {
  mutable sc1 : char;
  mutable sc2 : char;
  mutable sc3 : char;
  mutable sc4 : bool;
}
let process device machine l =
  let open Gb.Machine in
  let cy = Int64.of_int 1000000 in
  (* we make sure we do turn on the sound *)
  machine.apu.enabled <- true;
  machine.apu.channelenable <- (Gb.Uint8.inj 0xFF);
  machine.apu.frameseq_counter <- 0;
  machine.apu.frameseq <- 0;
  machine.sc1.step <- 0;
  let last_on = { sc1 = '\000'; sc2 = '\000'; sc3 = '\000'; sc4 = false;} in


  let rec aux l cycles w =


    match cycles with
    | 0 -> begin
      match l with
      | (Vgm.Parser.Wait n)::xs ->
         aux xs (n * per_sample) w
      | Write (n, r)::xs -> 
         Gb.Mmu.put machine (n + 0xFF10) (Char.unsafe_chr r);
         let now = Mtime_clock.now_ns () in
         let elapsed = Int64.sub now w in

         if elapsed < cy then begin
           aux xs 0 w
        end 
        else begin

          if machine.sc1.frequency > 30 then begin
          let note_sc1 = machine.sc1.frequency |> Vgm.Sound_table.map_sound_to_midi |> Vgm.Uint8.inj in

          let ev1_sc1 = note_off ~channel:1 last_on.sc1 '\000' in
          let ev2_sc1 = note_on ~channel:1 note_sc1 '\090' in

          if last_on.sc1 != note_sc1 then
            ignore @@ Portmidi.write_output device [ ev1_sc1; ev2_sc1 ];

            last_on.sc1 <- note_sc1;
          end;

          if machine.sc2.frequency > 30 then begin

          let note_sc2 = machine.sc2.frequency |> Vgm.Sound_table.map_sound_to_midi |> Vgm.Uint8.inj in


          let ev1_sc2 = note_off ~channel:1 last_on.sc2 '\000' in
          let ev2_sc2 = note_on ~channel:1 note_sc2 '\090' in


          if last_on.sc2 != note_sc2 then
            ignore @@ Portmidi.write_output device [ ev1_sc2; ev2_sc2 ];

          last_on.sc2 <- note_sc2;
          end;

          if machine.sc3.frequency > 30 then begin

          let note_sc3 = machine.sc3.frequency |> Vgm.Sound_table.map_sound_to_midi |> Vgm.Uint8.inj in
          let ev1_sc3 = note_off ~channel:1 last_on.sc3 '\000' in
          let ev2_sc3 = note_on ~channel:1 note_sc3 '\040' in

          if last_on.sc3 != note_sc3 then
            ignore @@ Portmidi.write_output device [ ev1_sc3; ev2_sc3 ];
          last_on.sc3 <- note_sc3;
          end;
          aux xs 1 now
         end 
      | Osef::xs ->
         aux xs 0 w
      | [] -> ()
    end
    | cycles ->
       for _i = cycles downto 0 do 
        (Gb.Spu.step machine 1)
       done;
       let to_s = (0.004) in
       ignore @@ Unix.select [] [] [] to_s; 
       aux l 0 w
  in
  aux l 0 (Mtime_clock.now_ns ())




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

  let _ = Portmidi.initialize () in
  let* device =
    match Portmidi.open_output ~device_id:5 ~buffer_size:Int32.zero ~latency:Int32.zero with
    | Error _ -> Error (`Msg "oh no")
    | Ok device -> Ok device
  in
  let cfg = {
    Gb.Machine.sample_rate = 44100;
    sample_size = 4096;
    bios = None;
    rom = None;
    sav = None;
  } in
  let machine = Gb.Machine.make cfg in
  let* in_file =
    if (Array.length Sys.argv) != 2 then
      Error (`Msg "command is ./main.exe vgm/vgz file")
    else
      Ok Sys.argv.(1)
  in
  let* filename = Fpath.of_string in_file in
  let* content = load_file filename in
  let* content = 
    match Fpath.get_ext filename with
    | ".vgm" -> Ok (Bigstringaf.of_string ~off:0 ~len:(String.length content) content)
    | ".vgz" -> inflate_string content
    | _ -> Error (`Msg "unsupported file type: need .vgm or .vgz")
in
let parse =  Angstrom.parse_bigstring
              ~consume:Angstrom.Consume.All
              Vgm.Parser.parse_header content
  in
  match parse with
  | Error err -> Error (`Msg err)
  | Ok l -> process device machine l; turn_off_everything device;turn_off_everything device;Ok ()

let () =
  match main () with
  | Error (`Msg err) -> print_endline err
  | Ok () -> ()
