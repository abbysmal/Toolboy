(* module type Engine_impl = sig
 * 
 *   type t
 * 
 *   val step : t -> Machine.t -> t
 *   val create : Machine.t -> (t * Machine.t)
 *   val refresh : t -> Machine.t -> unit
 *   val poll_input : t -> Machine.t -> Joypad.key_event option
 *   val teardown : t -> unit
 *   val is_paused : t -> bool
 * 
 * end
 * 
 * module type Engine = sig
 * 
 *   type t
 * 
 *   val make : ?bios:bytes -> rom:bytes -> unit -> t
 *   val step : t -> t
 *   val is_paused : t -> bool
 *     
 * end
 * 
 * module Make (M : Engine_impl) : (Engine with type t = (M.t * Machine.t)) = struct
 * 
 *   type t = M.t * Machine.t
 * 
 *   let make ?bios ~rom () =
 *     let state = Machine.make ?bios ~rom () in
 *     M.create state
 * 
 *   let handle_key_event t key =
 *     (match key with
 *     | `Button_up key -> Joypad.button_up t key 
 *     | `Button_down key -> Joypad.button_up t key);
 *     let isf = Mmu.get t 0xFF0F in
 *     Mmu.put t 0xFF0F (Uint8.set_bit isf Interrupts.joypad)
 * 
 *   let step (t, state) =
 *     if M.is_paused t then
 *       let t = M.step t state in
 *       let _ = M.poll_input t state in 
 *       (t, state)
 *     else
 *       let run () =
 *         let () = Cpu_exec.step state in
 *         let t = M.step t state in
 *         if state.gpu.redraw then begin
 *           M.refresh t state;
 *           state.gpu.redraw <- false;
 *         end; 
 *         let key = M.poll_input t state in 
 *         match key with
 *         | Some key -> handle_key_event state key
 *         | None -> ();
 *       in
 *       run ();
 *       (t, state)
 * 
 *   let is_paused (t, _) = M.is_paused t
 * 
 * end *)

let trigger_interrupt t int =
  let isf = Mmu.get t 0xFF0F in
  Mmu.put t 0xFF0F (Uint8.set_bit isf int)

let initial_state m =
  let set a v = Mmu.put m a (Uint8.inj v) in
  Cpu.put_register_pair m Cpu.AF 0x01B0;
  m.cpu.n <- false;
  Cpu.put_register_pair m Cpu.BC 0x0013;
  Cpu.put_register_pair m Cpu.DE 0x00D8;
  Cpu.put_register_pair m Cpu.HL 0x014D;
  m.cpu.sp <- 0xFFFE; 
  m.cpu.pc <- 0x0100; 
  set 0xFF05 0x00;
  set 0xFF06 0x00;
  set 0xFF07 0x00;
  set 0xFF10 0x80;
  set 0xFF11 0xBF;
  set 0xFF12 0xF3;
  set 0xFF14 0xBF;
  set 0xFF16 0x3F;
  set 0xFF17 0x00;
  set 0xFF19 0xBF;
  set 0xFF1A 0x7F;
  set 0xFF1B 0xFF;
  set 0xFF1C 0x9F;
  set 0xFF1E 0xBF;
  set 0xFF20 0xFF;
  set 0xFF21 0x00;
  set 0xFF22 0x00;
  set 0xFF23 0xBF;
  set 0xFF24 0x77;
  set 0xFF25 0xF3;
  set 0xFF26 0xF1;
  set 0xFF40 0x91;
  set 0xFF42 0x00;
  set 0xFF43 0x00;
  set 0xFF45 0x00;
  set 0xFF47 0xFC;
  set 0xFF48 0xFF;
  set 0xFF49 0xFF;
  set 0xFF4A 0x00;
  set 0xFF4B 0x00;
  set 0xFFFF 0x00
