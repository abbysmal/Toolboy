exception Unknown_instruction of string

open Cpu
    
type instruction =
    Ld_n of register
  | Jp_c of bool
  | Jp_z of bool
  | Ld_n_hl
  | Ld_nn_r of register
  | Or of (register * register)
  | Or_n of register
  | Or_hl
  | And of (register * register)
  | And_n
  | And_hl
  | Ld_rr of (register * register)
  | Ld_nn_sp
  | Ld_hl_sp_n
  | Ld_hl_sp
  | Ld_sp
  | Ld_r_hl of register
  | Ld_hl_r of register
  | Ld_r_nn of register
  | Ld_nn of paired_register
  | Ld_p_nn of (paired_register * register)
  | Ldd of (register * paired_register)
  | Ldd_hl of (paired_register * register)
  | Ld_0xFF00_a
  | Ld_0xFF00_r_n of register
  | Ld_0xFF00_n_r of register
  | Ld_0xFF00_ra_r of (register * register)
  | Ld_0xFF00_r_ra of (register * register)
  | Ld_p of (paired_register * register)
  | Ldi_hl_r of register
  | Ldi_r_hl of register
  | Call_nn
  | Push_p of paired_register
  | Pop_p of paired_register
  | Bit of (int * register)
  | Jr of (flag * bool)
  | Jrz of (flag)
  | Jr_n
  | Jp_hl
  | Ret
  | Reti
  | Bit_hl of int
  | Call_c of bool
  | Call_z of bool
  | Ret_z of bool
  | Ret_c of bool
  | Rst of int
  | Noop
  | Rr of register
  | Rra
  | Rr_hl
  | Ccf
  | Cp of register
  | Sra_r of register
  | Sra_hl
  | Rrca
  | Rrc_r of register
  | Rrc_hl
  | Cpl
  | Cp_n
  | Cp_p
  | Cp_hl
  | Srl of register
  | Srl_hl
  | Set of (register * int)
  | Set_hl of int
  | Sbc_hl of register
  | Sbc of (register * register)
  | Sbc_n of register
  | Xor of (register * register)
  | Xor_n of register
  | Xor_hl of register
  | Swap of register
  | Swap_hl
  | Add of (register * register)
  | Add_hl
  | Add_n
  | Sub_n
  | Sub_hl
  | Add_n_hl of paired_register
  | Add_n_sp
  | Add_sp_hl
  | Adc_n of register
  | Adc of (register * register)
  | Adc_hl of register
  | Jp_nn
  | Rla
  | Rlc
  | Rlc_hl
  | Rlc_n of register
  | Rl_r of register
  | Rl_hl
  | Halt
  | Dec of register
  | Dec_p of paired_register
  | Inc_p of paired_register
  | Inc of register
  | Inc_sp
  | Dec_sp
  | Sla of register
  | Sla_hl
  | Daa
  | Res of (int * register)
  | Res_hl of int
  | Sub_r of register
  | Di
  | Ei
  | Scf
  | Dec_hl
  | Inc_hl
  | Illegal
  | Unk of string [@@deriving sexp]

(* let instruction_size i =
 *   match i with
 *   | Ld_n _
 *   | Ld_n_hl
 *   | Cp_n
 *   | Add_n
 *   | Sub_n
 *   | Jr_n
 *   | Ld_0xFF00_r_n _
 *   | Jr _
 *   | Jrz _
 *   | And_n
 *   | Ld_hl_sp_n
 *   | Add_n_hl _
 *   | Ld_0xFF00_n_r _ -> 2
 *   | Call_nn
 *   | Call_c _
 *   | Call_z _
 *   | Ld_r_nn _
 *   | Ld_sp
 *   | Ld_nn_sp
 *   | Jp_nn
 *   | Jp_c _
 *   | Jp_z _
 *   | Ld_p_nn _
 *   | Ld_nn_r _
 *   | Ld_nn _ -> 3
 *   | Ld_rr _
 *   | And _
 *   | Jp_hl
 *   | And_hl
 *   | Ld_r_hl _
 *   | Ldd _
 *   | Ld_0xFF00_a
 *   | Ld_0xFF00_ra_r _
 *   | Ld_0xFF00_r_ra _
 *   | Swap _
 *   | Ld_p _
 *   | Ldi_hl_r _
 *   | Push_p _
 *   | Pop_p _
 *   | Bit _
 *   | Ret
 *   | Reti
 *   | Ret_z _
 *   | Ret_c _
 *   | Noop
 *   | Xor _
 *   | Sub_r _
 *   | Adc
 *   | Add_hl
 *   | Rlc
 *   | Rla
 *   | Rlc_r _
 *   | Rl_r _
 *   | Halt
 *   | Dec _
 *   | Inc_p _
 *   | Unk _
 *   | Cp_p
 *   | Ldi_r_hl _
 *   | Cp_hl
 *   | Res _
 *   | Di
 *   | Ei
 *   | Dec_p _
 *   | Or _
 *   | Cpl
 *   | Scf
 *   | Rst _
 *   | Ld_hl_r _
 *   | Add _
 *   | Dec_hl
 *   | Inc_hl
 *   | Daa
 *   | Inc _ -> 1 *)

let compile_extended code =
  match Char.code code with
  | 0x38 -> Srl B
  | 0x39 -> Srl C
  | 0x3A -> Srl D
  | 0x3B -> Srl E
  | 0x3C -> Srl H
  | 0x3D -> Srl L
  | 0x3E -> Srl_hl
  | 0x3F -> Srl A
  | 0xC0 -> Set (B, 0)
  | 0xC1 -> Set (C, 0)
  | 0xC2 -> Set (D, 0)
  | 0xC3 -> Set (E, 0)
  | 0xC4 -> Set (H, 0)
  | 0xC5 -> Set (L, 0)
  | 0xC6 -> Set_hl 0
  | 0xC7 -> Set (A, 0)
  | 0xC8 -> Set (B, 1)
  | 0xC9 -> Set (C, 1)
  | 0xCA -> Set (D, 1)
  | 0xCB -> Set (E, 1)
  | 0xCC -> Set (H, 1)
  | 0xCD -> Set (L, 1)
  | 0xCE -> Set_hl 1
  | 0xCF -> Set (A, 1)
  | 0xD0 -> Set (B, 2)
  | 0xD1 -> Set (C, 2)
  | 0xD2 -> Set (D, 2)
  | 0xD3 -> Set (E, 2)
  | 0xD4 -> Set (H, 2)
  | 0xD5 -> Set (L, 2)
  | 0xD6 -> Set_hl 2
  | 0xD7 -> Set (A, 2)
  | 0xD8 -> Set (B, 3)
  | 0xD9 -> Set (C, 3)
  | 0xDA -> Set (D, 3)
  | 0xDB -> Set (E, 3)
  | 0xDC -> Set (H, 3)
  | 0xDD -> Set (L, 3)
  | 0xDE -> Set_hl 3
  | 0xDF -> Set (A, 3)
  | 0xE0 -> Set (B, 4)
  | 0xE1 -> Set (C, 4)
  | 0xE2 -> Set (D, 4)
  | 0xE3 -> Set (E, 4)
  | 0xE4 -> Set (H, 4)
  | 0xE5 -> Set (L, 4)
  | 0xE6 -> Set_hl 4
  | 0xE7 -> Set (A, 4)
  | 0xE8 -> Set (B, 5)
  | 0xE9 -> Set (C, 5)
  | 0xEA -> Set (D, 5)
  | 0xEB -> Set (E, 5)
  | 0xEC -> Set (H, 5)
  | 0xED -> Set (L, 5)
  | 0xEE -> Set_hl 5
  | 0xEF -> Set (A, 5)
  | 0xF0 -> Set (B, 6)
  | 0xF1 -> Set (C, 6)
  | 0xF2 -> Set (D, 6)
  | 0xF3 -> Set (E, 6)
  | 0xF4 -> Set (H, 6)
  | 0xF5 -> Set (L, 6)
  | 0xF6 -> Set_hl 6
  | 0xF7 -> Set (A, 6)
  | 0xF8 -> Set (B, 7)
  | 0xF9 -> Set (C, 7)
  | 0xFA -> Set (D, 7)
  | 0xFB -> Set (E, 7)
  | 0xFC -> Set (H, 7)
  | 0xFD -> Set (L, 7)
  | 0xFE -> Set_hl 7
  | 0xFF -> Set (A, 7)
  | 0x18 -> Rr B
  | 0x19 -> Rr C
  | 0x1A -> Rr D
  | 0x1B -> Rr E
  | 0x1C -> Rr H
  | 0x1D -> Rr L
  | 0x1F -> Rr A
  | 0x1E -> Rr_hl
  | 0x10 -> Rl_r B
  | 0x11 -> Rl_r C
  | 0x12 -> Rl_r D
  | 0x13 -> Rl_r E
  | 0x14 -> Rl_r H
  | 0x15 -> Rl_r L
  | 0x16 -> Rl_hl
  | 0x17 -> Rl_r A
  | 0x80 -> Res (0, B)
  | 0x81 -> Res (0, C)
  | 0x82 -> Res (0, D)
  | 0x83 -> Res (0, E)
  | 0x84 -> Res (0, H)
  | 0x85 -> Res (0, L)
  | 0x86 -> Res_hl 0
  | 0x87 -> Res (0, A)
  | 0x88 -> Res (1, B)
  | 0x89 -> Res (1, C)
  | 0x8A -> Res (1, D)
  | 0x8B -> Res (1, E)
  | 0x8C -> Res (1, H)
  | 0x8D -> Res (1, L)
  | 0x8E -> Res_hl 1
  | 0x8F -> Res (1, A)
  | 0x90 -> Res (2, B)
  | 0x91 -> Res (2, C)
  | 0x92 -> Res (2, D)
  | 0x93 -> Res (2, E)
  | 0x94 -> Res (2, H)
  | 0x95 -> Res (2, L)
  | 0x96 -> Res_hl 2
  | 0x97 -> Res (2, A)
  | 0x98 -> Res (3, B)
  | 0x99 -> Res (3, C)
  | 0x9A -> Res (3, D)
  | 0x9B -> Res (3, E)
  | 0x9C -> Res (3, H)
  | 0x9D -> Res (3, L)
  | 0x9E -> Res_hl 3
  | 0x9F -> Res (3, A)
  | 0xA0 -> Res (4, B)
  | 0xA1 -> Res (4, C)
  | 0xA2 -> Res (4, D)
  | 0xA3 -> Res (4, E)
  | 0xA4 -> Res (4, H)
  | 0xA5 -> Res (4, L)
  | 0xA6 -> Res_hl 4
  | 0xA7 -> Res (4, A)
  | 0xA8 -> Res (5, B)
  | 0xA9 -> Res (5, C)
  | 0xAA -> Res (5, D)
  | 0xAB -> Res (5, E)
  | 0xAC -> Res (5, H)
  | 0xAD -> Res (5, L)
  | 0xAE -> Res_hl 5
  | 0xAF -> Res (5, A)
  | 0xB0 -> Res (6, B)
  | 0xB1 -> Res (6, C)
  | 0xB2 -> Res (6, D)
  | 0xB3 -> Res (6, E)
  | 0xB4 -> Res (6, H)
  | 0xB5 -> Res (6, L)
  | 0xB6 -> Res_hl 6
  | 0xB7 -> Res (6, A)
  | 0xB8 -> Res (7, B)
  | 0xB9 -> Res (7, C)
  | 0xBA -> Res (7, D)
  | 0xBB -> Res (7, E)
  | 0xBC -> Res (7, H)
  | 0xBD -> Res (7, L)
  | 0xBE -> Res_hl 7
  | 0xBF -> Res (7, A)
  | 0x40 -> Bit (0, B)
  | 0x41 -> Bit (0, C)
  | 0x42 -> Bit (0, D)
  | 0x43 -> Bit (0, E)
  | 0x44 -> Bit (0, H)
  | 0x45 -> Bit (0, L)
  | 0x46 -> Bit_hl 0
  | 0x47 -> Bit (0, A)
  | 0x48 -> Bit (1, B)
  | 0x49 -> Bit (1, C)
  | 0x4A -> Bit (1, D)
  | 0x4B -> Bit (1, E)
  | 0x4C -> Bit (1, H)
  | 0x4D -> Bit (1, L)
  | 0x4E -> Bit_hl 1
  | 0x4F -> Bit (1, A)
  | 0x50 -> Bit (2, B)
  | 0x51 -> Bit (2, C)
  | 0x52 -> Bit (2, D)
  | 0x53 -> Bit (2, E)
  | 0x54 -> Bit (2, H)
  | 0x55 -> Bit (2, L)
  | 0x56 -> Bit_hl 2
  | 0x57 -> Bit (2, A)
  | 0x58 -> Bit (3, B)
  | 0x59 -> Bit (3, C)
  | 0x5A -> Bit (3, D)
  | 0x5B -> Bit (3, E)
  | 0x5C -> Bit (3, H)
  | 0x5D -> Bit (3, L)
  | 0x5E -> Bit_hl 3
  | 0x5F -> Bit (3, A)
  | 0x60 -> Bit (4, B)
  | 0x61 -> Bit (4, C)
  | 0x62 -> Bit (4, D)
  | 0x63 -> Bit (4, E)
  | 0x64 -> Bit (4, H)
  | 0x65 -> Bit (4, L)
  | 0x66 -> Bit_hl 4
  | 0x67 -> Bit (4, A)
  | 0x68 -> Bit (5, B)
  | 0x69 -> Bit (5, C)
  | 0x6A -> Bit (5, D)
  | 0x6B -> Bit (5, E)
  | 0x6C -> Bit (5, H)
  | 0x6D -> Bit (5, L)
  | 0x6E -> Bit_hl 5
  | 0x6F -> Bit (5, A)
  | 0x70 -> Bit (6, B)
  | 0x71 -> Bit (6, C)
  | 0x72 -> Bit (6, D)
  | 0x73 -> Bit (6, E)
  | 0x74 -> Bit (6, H)
  | 0x75 -> Bit (6, L)
  | 0x76 -> Bit_hl 6
  | 0x77 -> Bit (6, A)
  | 0x78 -> Bit (7, B)
  | 0x79 -> Bit (7, C)
  | 0x7A -> Bit (7, D)
  | 0x7B -> Bit (7, E)
  | 0x7C -> Bit (7, H)
  | 0x7D -> Bit (7, L)
  | 0x7E -> Bit_hl 7
  | 0x7F -> Bit (7, A)
  | 0x37 -> Swap A
  | 0x36 -> Swap_hl
  | 0x0E -> Rrc_hl
  | 0x0F -> Rrc_r A
  | 0x08 -> Rrc_r B
  | 0x09 -> Rrc_r C
  | 0x0A -> Rrc_r D
  | 0x0B -> Rrc_r E
  | 0x0C -> Rrc_r H
  | 0x0D -> Rrc_r L
  | 0x30 -> Swap B
  | 0x31 -> Swap C
  | 0x32 -> Swap D
  | 0x33 -> Swap E
  | 0x34 -> Swap H
  | 0x35 -> Swap L
  | 0x2E -> Sra_hl
  | 0x2F -> Sra_r A
  | 0x28 -> Sra_r B
  | 0x29 -> Sra_r C
  | 0x2A -> Sra_r D
  | 0x2B -> Sra_r E
  | 0x2C -> Sra_r H
  | 0x2D -> Sra_r L
  | 0x26 -> Sla_hl
  | 0x27 -> Sla A
  | 0x20 -> Sla B
  | 0x21 -> Sla C
  | 0x22 -> Sla D
  | 0x23 -> Sla E
  | 0x24 -> Sla H
  | 0x25 -> Sla L
  | 0x06 -> Rlc_hl
  | 0x07 -> Rlc_n A
  | 0x00 -> Rlc_n B
  | 0x01 -> Rlc_n C
  | 0x02 -> Rlc_n D
  | 0x03 -> Rlc_n E
  | 0x04 -> Rlc_n H
  | 0x05 -> Rlc_n L
  | c -> Unk (Utils.show_hex_i16 c)

let compile code =
  match code with
  | 0x0F -> Rrca
  | 0x33 -> Inc_sp
  | 0x3B -> Dec_sp
  | 0x07 -> Rlc
  | 0x27 -> Daa
  | 0x00 -> Noop
  | 0xF8 -> Ld_hl_sp_n
  | 0x2A -> Ldi_hl_r A
  | 0x06 -> Ld_n B
  | 0x0E -> Ld_n C
  | 0x3F -> Ccf
  | 0x16 -> Ld_n D
  | 0x1E -> Ld_n E
  | 0x26 -> Ld_n H
  | 0x2E -> Ld_n L
  | 0x3E -> Ld_n A
  | 0x98 -> Sbc (A, B)
  | 0x99 -> Sbc (A, C)
  | 0x9A -> Sbc (A, D)
  | 0x9B -> Sbc (A, E)
  | 0x9C -> Sbc (A, H)
  | 0x9D -> Sbc (A, L)
  | 0x9F -> Sbc (A, A)
  | 0xDE -> Sbc_n A
  | 0x1F -> Rra
  | 0x48 -> Ld_rr (C, B)
  | 0x49 -> Ld_rr (C, C)
  | 0x4A -> Ld_rr (C, D)
  | 0x4B -> Ld_rr (C, E)
  | 0x4C -> Ld_rr (C, H)
  | 0x4D -> Ld_rr (C, L)
  | 0x4F -> Ld_rr (C, A)
  | 0x78 -> Ld_rr (A, B)
  | 0x79 -> Ld_rr (A, C)
  | 0x7A -> Ld_rr (A, D)
  | 0x7B -> Ld_rr (A, E)
  | 0x7C -> Ld_rr (A, H)
  | 0x7D -> Ld_rr (A, L)
  | 0x7F -> Ld_rr (A, A)
  | 0x40 -> Ld_rr (B, B)
  | 0x41 -> Ld_rr (B, C)
  | 0x42 -> Ld_rr (B, D)
  | 0x43 -> Ld_rr (B, E)
  | 0x44 -> Ld_rr (B, H)
  | 0x45 -> Ld_rr (B, L)
  | 0x47 -> Ld_rr (B, A)
  | 0x50 -> Ld_rr (D, B)
  | 0x51 -> Ld_rr (D, C)
  | 0x52 -> Ld_rr (D, D)
  | 0x53 -> Ld_rr (D, E)
  | 0x54 -> Ld_rr (D, H)
  | 0x55 -> Ld_rr (D, L)
  | 0x57 -> Ld_rr (D, A)
  | 0x58 -> Ld_rr (E, B)
  | 0x59 -> Ld_rr (E, C)
  | 0x5A -> Ld_rr (E, D)
  | 0x5B -> Ld_rr (E, E)
  | 0x5C -> Ld_rr (E, H)
  | 0x5D -> Ld_rr (E, L)
  | 0x5F -> Ld_rr (E, A)
  | 0x67 -> Ld_rr (H, A)
  | 0x68 -> Ld_rr (L, B)
  | 0x69 -> Ld_rr (L, C)
  | 0x6A -> Ld_rr (L, D)
  | 0x6B -> Ld_rr (L, E)
  | 0x6C -> Ld_rr (L, H)
  | 0x6D -> Ld_rr (L, L)
  | 0x6F -> Ld_rr (L, A)
  | 0x60 -> Ld_rr (H, B)
  | 0x61 -> Ld_rr (H, C)
  | 0x62 -> Ld_rr (H, D)
  | 0x63 -> Ld_rr (H, E)
  | 0x64 -> Ld_rr (H, H)
  | 0x65 -> Ld_rr (H, L)
  | 0x87 -> Add (A, A)
  | 0x80 -> Add (B, A)
  | 0x81 -> Add (C, A)
  | 0x82 -> Add (D, A)
  | 0x83 -> Add (E, A)
  | 0x84 -> Add (H, A)
  | 0x85 -> Add (L, A)
  | 0x86 -> Add_hl
  | 0x97 -> Sub_r A
  | 0x90 -> Sub_r B
  | 0x91 -> Sub_r C
  | 0x92 -> Sub_r D
  | 0x93 -> Sub_r E
  | 0x94 -> Sub_r H
  | 0x95 -> Sub_r L
  | 0x0A -> Ld_p (BC, A)
  | 0x1A -> Ld_p (DE, A)
  | 0x02 -> Ld_p_nn (BC, A)
  | 0x12 -> Ld_p_nn (DE, A)
  | 0x77 -> Ld_p_nn (HL, A)
  | 0xE0 -> Ld_0xFF00_r_n A
  | 0xF0 -> Ld_0xFF00_n_r A
  | 0xF2 -> Ld_0xFF00_ra_r (C, A)
  | 0xE2 -> Ld_0xFF00_r_ra (A, C)
  | 0x22 -> Ldi_r_hl A
  | 0x17 -> Rla
  | 0x20 -> Jr (Z, false)
  | 0x28 -> Jrz Z
  | 0x38 -> Jr (CA, true)
  | 0x30 -> Jr (CA, false)
  | 0x18 -> Jr_n
  | 0x88 -> Adc (A, B)
  | 0x89 -> Adc (A, C)
  | 0x8A -> Adc (A, D)
  | 0x8B -> Adc (A, E)
  | 0x8C -> Adc (A, H)
  | 0x8D -> Adc (A, L)
  | 0x8F -> Adc (A, A)
  | 0x8E -> Adc_hl A
  | 0xCE -> Adc_n A
  | 0xC5 -> Push_p BC
  | 0xD5 -> Push_p DE
  | 0xE5 -> Push_p HL
  | 0xF5 -> Push_p AF
  | 0xC1 -> Pop_p BC
  | 0xD1 -> Pop_p DE
  | 0xE1 -> Pop_p HL
  | 0xF1 -> Pop_p AF
  | 0xC9 -> Ret
  | 0xD9 -> Reti
  | 0xC0 -> Ret_z false
  | 0xC8 -> Ret_z true
  | 0xD0 -> Ret_c false
  | 0xD8 -> Ret_c true
  | 0x01 -> Ld_nn BC
  | 0x11 -> Ld_nn DE
  | 0x21 -> Ld_nn HL
  | 0x32 -> Ldd (A, HL)
  | 0x3A -> Ldd_hl (HL, A)
  | 0x70 -> Ld_r_hl B
  | 0x71 -> Ld_r_hl C
  | 0x72 -> Ld_r_hl D
  | 0x73 -> Ld_r_hl E
  | 0x74 -> Ld_r_hl H
  | 0x75 -> Ld_r_hl L
  | 0xEA -> Ld_r_nn A
  | 0xFA -> Ld_nn_r A
  | 0x7E -> Ld_hl_r A
  | 0x46 -> Ld_hl_r B
  | 0x4E -> Ld_hl_r C
  | 0x56 -> Ld_hl_r D
  | 0x5E -> Ld_hl_r E
  | 0x66 -> Ld_hl_r H
  | 0x6E -> Ld_hl_r L
  | 0x76 -> Halt
  | 0xC3 -> Jp_nn
  | 0xE9 -> Jp_hl
  | 0xAE -> Xor_hl A
  | 0xEE -> Xor_n A
  | 0xA8 -> Xor (A, B)
  | 0xA9 -> Xor (A, C)
  | 0xAA -> Xor (A, D)
  | 0xAB -> Xor (A, E)
  | 0xAC -> Xor (A, H)
  | 0xAD -> Xor (A, L)
  | 0xAF -> Xor (A, A)
  | 0xCD -> Call_nn
  | 0xC4 -> Call_z false
  | 0xCC -> Call_z true
  | 0xD4 -> Call_c false
  | 0xDC -> Call_c true
  | 0x31 -> Ld_sp
  | 0x3C -> Inc A
  | 0x04 -> Inc B
  | 0x0C -> Inc C
  | 0x14 -> Inc D
  | 0x1C -> Inc E
  | 0x24 -> Inc H
  | 0x2C -> Inc L
  | 0x03 -> Inc_p BC
  | 0x13 -> Inc_p DE
  | 0x23 -> Inc_p HL
  | 0x05 -> Dec B
  | 0x0D -> Dec C
  | 0x15 -> Dec D
  | 0x1D -> Dec E
  | 0x25 -> Dec H
  | 0x2D -> Dec L
  | 0x3D -> Dec A
  | 0x0B -> Dec_p BC
  | 0x1B -> Dec_p DE
  | 0x2B -> Dec_p HL
  | 0xFE -> Cp_n
  | 0xBE -> Cp_hl
  | 0xF3 -> Di
  | 0xFB -> Ei
  | 0x36 -> Ld_n_hl
  | 0xB7 -> Or (A, A)
  | 0xB0 -> Or (B, A)
  | 0xB1 -> Or (C, A)
  | 0xB2 -> Or (D, A)
  | 0xB3 -> Or (E, A)
  | 0xB4 -> Or (H, A)
  | 0xB5 -> Or (L, A)
  | 0xB6 -> Or_hl
  | 0xF6 -> Or_n A
  | 0xA7 -> And (A, A)
  | 0xA0 -> And (B, A)
  | 0xA1 -> And (C, A)
  | 0xA2 -> And (D, A)
  | 0xA3 -> And (E, A)
  | 0xA4 -> And (H, A)
  | 0xA5 -> And (L, A)
  | 0xE6 -> And_n
  | 0xA6 -> And_hl
  | 0x2F -> Cpl
  | 0x37 -> Scf
  | 0xC7 -> Rst 0x0000
  | 0xCF -> Rst 0x0008
  | 0xD7 -> Rst 0x0010
  | 0xDF -> Rst 0x0018
  | 0xE7 -> Rst 0x0020
  | 0xEF -> Rst 0x0028
  | 0xF7 -> Rst 0x0030
  | 0xFF -> Rst 0x0038
  | 0x09 -> Add_n_hl BC
  | 0x19 -> Add_n_hl DE
  | 0x29 -> Add_n_hl HL
  | 0x39 -> Add_sp_hl
  | 0xC6 -> Add_n
  | 0xD6 -> Sub_n
  | 0x96 -> Sub_hl
  | 0xC2 -> Jp_z false
  | 0xCA -> Jp_z true
  | 0xD2 -> Jp_c false
  | 0xDA -> Jp_c true
  | 0x35 -> Dec_hl
  | 0x34 -> Inc_hl
  | 0x08 -> Ld_nn_sp
  | 0xBF -> Cp A
  | 0xB8 -> Cp B
  | 0xB9 -> Cp C
  | 0xBA -> Cp D
  | 0xBB -> Cp E
  | 0xBC -> Cp H
  | 0xBD -> Cp L
  | 0xF9 -> Ld_hl_sp
  | 0xE8 -> Add_n_sp
  | 0x9E -> Sbc_hl A
  | 0xEC
  | 0xFC -> Illegal
  | c -> Unk (Utils.show_hex_i16 c)
